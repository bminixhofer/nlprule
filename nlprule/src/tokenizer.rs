//! A tokenizer to split raw text into tokens.
//! Tokens are assigned lemmas and part-of-speech tags by lookup from a [Tagger][tag::Tagger] and chunks containing
//! information about noun / verb and grammatical case by a statistical [Chunker][chunk::Chunker].
//! Tokens are *disambiguated* (i. e. information from the initial assignment is changed) in a rule-based way by
//! [DisambiguationRule][crate::rule::DisambiguationRule]s.

use crate::{types::*, utils::parallelism::MaybeParallelRefIterator};
use lazy_static::lazy_static;
use onig::Regex;
use serde::{Deserialize, Serialize};
use std::{
    collections::HashSet,
    fs::File,
    io::{BufReader, Read},
    path::Path,
    sync::Arc,
};
use unicode_segmentation::UnicodeSegmentation;

pub mod chunk;
pub mod tag;

use chunk::Chunker;
use tag::Tagger;

use crate::rule::DisambiguationRule;

// see https://stackoverflow.com/a/40296745
fn split<F>(text: &str, split_func: F) -> Vec<&str>
where
    F: Fn(char) -> bool,
{
    let mut result = Vec::new();
    let mut last = 0;
    for (index, matched) in text.match_indices(split_func) {
        if last != index {
            result.push(&text[last..index]);
        }
        result.push(matched);
        last = index + matched.len();
    }
    if last < text.len() {
        result.push(&text[last..]);
    }

    result
}

fn get_token_strs(text: &str) -> Vec<&str> {
    let mut tokens = Vec::new();

    lazy_static! {
        // see https://stackoverflow.com/a/17773849
        static ref URL_REGEX: Regex = Regex::new(r"(https?:\/\/(?:www\.|(?!www))[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\.[^\s]{2,}|www\.[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\.[^\s]{2,}|https?:\/\/(?:www\.|(?!www))[a-zA-Z0-9]+\.[^\s]{2,}|www\.[a-zA-Z0-9]+\.[^\s]{2,})").unwrap();
    }

    let mut prev = 0;
    let split_func = |c: char| c.is_whitespace() || crate::utils::splitting_chars().contains(c);

    for (start, end) in URL_REGEX.find_iter(text) {
        tokens.extend(split(&text[prev..start], split_func));
        tokens.push(&text[start..end]);
        prev = end;
    }

    tokens.extend(split(&text[prev..text.len()], split_func));

    tokens
}

/// *Finalizes* the tokens by e. g. adding a specific UNKNOWN part-of-speech tag.
/// After finalization grammatical error correction rules can be used on the tokens.
pub fn finalize(tokens: Vec<IncompleteToken>) -> Vec<Token> {
    if tokens.is_empty() {
        return Vec::new();
    }

    let mut finalized = vec![Token::sent_start(tokens[0].text, tokens[0].tagger)];
    finalized.extend(tokens.into_iter().map(|x| x.into()));

    finalized
}

/// Options for a tokenizer.
#[derive(Serialize, Deserialize, Clone)]
pub struct TokenizerOptions {
    /// Whether to allow errors while constructing the tokenizer.
    pub allow_errors: bool,
    /// Whether to retain the last tag if disambiguation leads to an empty tag.
    /// Language-specific in LT so it has to be an option.
    pub retain_last: bool,
    /// Whether to use a heuristic to split potential compound words.
    pub use_compound_split_heuristic: bool,
    /// Whether to always add tags for a lowercase version of the word when assigning part-of-speech tags.
    pub always_add_lower_tags: bool,
    /// Disambiguation Rule IDs to use in this tokenizer.
    #[serde(default)]
    pub ids: Vec<String>,
    /// Disambiguation Rule IDs to ignore in this tokenizer.
    #[serde(default)]
    pub ignore_ids: Vec<String>,
    /// Specific examples in the notation `{id}:{example_index}` which are known to fail.
    #[serde(default)]
    pub known_failures: Vec<String>,
    /// Used part-of-speech tags which are not in the tagger dictionary.
    #[serde(default)]
    pub extra_tags: Vec<String>,
}

impl Default for TokenizerOptions {
    fn default() -> Self {
        TokenizerOptions {
            allow_errors: false,
            retain_last: false,
            use_compound_split_heuristic: false,
            always_add_lower_tags: false,
            ids: Vec::new(),
            ignore_ids: Vec::new(),
            known_failures: Vec::new(),
            extra_tags: Vec::new(),
        }
    }
}

/// The complete Tokenizer doing tagging, chunking and disambiguation.
#[derive(Serialize, Deserialize, Default)]
pub struct Tokenizer {
    rules: Vec<DisambiguationRule>,
    chunker: Option<Chunker>,
    tagger: Arc<Tagger>,
    options: TokenizerOptions,
}

impl Tokenizer {
    /// Creates a Tokenizer from a path to an XML file containing disambiguation rules.
    #[cfg(feature = "compile")]
    pub fn from_xml<P: AsRef<std::path::Path>>(
        path: P,
        tagger: Arc<Tagger>,
        chunker: Option<Chunker>,
        options: TokenizerOptions,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        use log::warn;

        let rules = crate::rule::read_disambiguation_rules(path);
        let mut error = None;

        let rules: Vec<_> = rules
            .into_iter()
            .filter_map(|x| match x {
                Ok((rule_structure, id)) => {
                    match DisambiguationRule::from_rule_structure(rule_structure, tagger.as_ref()) {
                        Ok(mut rule) => {
                            if error.is_none()
                                && (options.ids.is_empty() || options.ids.contains(&id))
                                && !options.ignore_ids.contains(&id)
                            {
                                rule.set_id(id);

                                Some(rule)
                            } else {
                                None
                            }
                        }
                        Err(x) => {
                            error = Some(format!("[Rule] {}", x));
                            None
                        }
                    }
                }
                Err(x) => {
                    error = Some(format!("[Structure] {}", x));
                    None
                }
            })
            .collect();

        if let Some(x) = error {
            if options.allow_errors {
                warn!("Error constructing Disambiguator: {}", x)
            } else {
                return Err(format!("Error constructing Disambiguator: {}", x).into());
            }
        }

        Ok(Tokenizer {
            tagger,
            chunker,
            rules,
            options,
        })
    }

    /// Creates a new tokenizer from a file.
    pub fn new<P: AsRef<Path>>(p: P) -> bincode::Result<Self> {
        let reader = BufReader::new(File::open(p).unwrap());
        bincode::deserialize_from(reader)
    }

    /// Creates a new tokenizer from a reader.
    pub fn new_from<R: Read>(reader: R) -> bincode::Result<Self> {
        bincode::deserialize_from(reader)
    }

    pub fn rules(&self) -> &Vec<DisambiguationRule> {
        &self.rules
    }

    pub fn tagger(&self) -> &Arc<Tagger> {
        &self.tagger
    }

    pub fn chunker(&self) -> &Option<Chunker> {
        &self.chunker
    }

    pub fn options(&self) -> &TokenizerOptions {
        &self.options
    }

    pub(crate) fn disambiguate_up_to_id<'t>(
        &'t self,
        mut tokens: Vec<IncompleteToken<'t>>,
        id: Option<&str>,
    ) -> Vec<IncompleteToken<'t>> {
        let n = id.map_or(self.rules.len(), |id| {
            self.rules.iter().position(|x| x.id == id).unwrap()
        });
        let mut i = 0;

        while i < n {
            let finalized = finalize(tokens.clone());
            let result = self.rules[i..n]
                .maybe_par_iter()
                .enumerate()
                .filter_map(|(j, rule)| {
                    let changes = rule.apply(&finalized, &self);
                    if changes.is_empty() {
                        None
                    } else {
                        Some((j + i, changes))
                    }
                })
                .find_first(|_| true);

            if let Some((index, changes)) = result {
                self.rules[index].change(&mut tokens, &self, changes);
                i = index + 1;
            } else {
                i = n;
            }
        }

        tokens
    }

    /// Apply rule-based disambiguation to the tokens.
    /// This does not change the number of tokens, but can change the content arbitrarily.
    pub fn disambiguate<'t>(
        &'t self,
        tokens: Vec<IncompleteToken<'t>>,
    ) -> Vec<IncompleteToken<'t>> {
        self.disambiguate_up_to_id(tokens, None)
    }

    /// Tokenize the given text. This applies chunking and tagging, but does not do disambiguation.
    pub fn tokenize<'t>(&'t self, text: &'t str) -> Vec<IncompleteToken<'t>> {
        let sentence_indices = text
            .unicode_sentences()
            .map(|sentence| {
                let ptr = sentence.as_ptr() as usize;
                (ptr, ptr + sentence.len())
            })
            .fold((HashSet::new(), HashSet::new()), |mut a, x| {
                a.0.insert(x.0);
                a.1.insert(x.1);
                a
            });

        let mut current_char = 0;
        let token_strs = get_token_strs(text);
        let mut tokens: Vec<_> = token_strs
            .into_iter()
            .map(|x| {
                let char_start = current_char;
                let ptr = x.as_ptr() as usize;
                current_char += x.chars().count();

                let byte_start = ptr - text.as_ptr() as usize;
                let trimmed = x.trim();

                let is_sentence_start = sentence_indices.0.contains(&ptr);
                let is_sentence_end = sentence_indices.1.contains(&(ptr + x.len()));

                IncompleteToken {
                    word: Word::new_with_tags(
                        self.tagger.id_word(trimmed.into()),
                        self.tagger.get_tags(
                            trimmed,
                            is_sentence_start || self.options.always_add_lower_tags,
                            self.options.use_compound_split_heuristic,
                        ),
                    ),
                    char_span: (char_start, current_char),
                    byte_span: (byte_start, byte_start + x.len()),
                    is_sentence_end,
                    has_space_before: text[..byte_start].ends_with(char::is_whitespace),
                    chunks: Vec::new(),
                    text,
                    tagger: self.tagger.as_ref(),
                }
            })
            .filter(|token| !token.word.text.as_ref().is_empty())
            .collect();

        if !tokens.is_empty() {
            let last_idx = tokens.len() - 1;
            tokens[last_idx].is_sentence_end = true;

            if let Some(chunker) = &self.chunker {
                chunker.apply(&mut tokens);
            }
        }

        tokens
    }
}

#[cfg(test)]
mod tests {
    use super::Tokenizer;
    use lazy_static::lazy_static;
    use quickcheck_macros::quickcheck;
    use std::fs::File;
    use std::io::BufReader;

    #[quickcheck]
    fn can_tokenize_anything(text: String) -> bool {
        lazy_static! {
            static ref TOKENIZER: Tokenizer = {
                let reader = BufReader::new(File::open("../storage/en_tokenizer.bin").unwrap());
                bincode::deserialize_from(reader).unwrap()
            };
        }

        TOKENIZER.tokenize(&text);
        true
    }
}
