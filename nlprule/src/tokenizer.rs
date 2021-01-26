//! A tokenizer to split raw text into tokens.
//! Tokens are assigned lemmas and part-of-speech tags by lookup from a [Tagger][tag::Tagger] and chunks containing
//! information about noun / verb and grammatical case by a statistical [Chunker][chunk::Chunker].
//! Tokens are *disambiguated* (i. e. information from the initial assignment is changed) in a rule-based way by
//! [DisambiguationRule][crate::rule::DisambiguationRule]s.

use crate::{types::*, utils::parallelism::MaybeParallelRefIterator};
use lazy_static::lazy_static;
use once_cell::sync::OnceCell;
use onig::Regex;
use serde::{Deserialize, Serialize};
use std::{
    fs::File,
    io::{BufReader, Read},
    ops::Deref,
    path::Path,
    sync::Arc,
};

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

fn get_token_strs<'t>(
    text: &'t str,
    extra_split_chars: &[char],
    extra_join_regexes: &[String],
) -> Vec<&'t str> {
    let mut tokens = Vec::new();

    lazy_static! {
        // see https://stackoverflow.com/a/17773849
        static ref URL_REGEX: Regex = Regex::new(r"(https?:\/\/(?:www\.|(?!www))[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\.[^\s]{2,}|www\.[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\.[^\s]{2,}|https?:\/\/(?:www\.|(?!www))[a-zA-Z0-9]+\.[^\s]{2,}|www\.[a-zA-Z0-9]+\.[^\s]{2,})").unwrap();
    }

    lazy_static! {
        static ref EXTRA_REGEXES: OnceCell<Vec<Regex>> = OnceCell::default();
    }

    let extra_regexes = EXTRA_REGEXES.get_or_init(|| {
        extra_join_regexes
            .iter()
            .map(|string| Regex::new(string).unwrap())
            .collect()
    });

    let split_func = |c: char| {
        c.is_whitespace()
            || crate::utils::splitting_chars().contains(c)
            || extra_split_chars.contains(&c)
    };

    let mut joined_mask = vec![false; text.len()];
    let mut joins = Vec::new();

    for regex in Some(URL_REGEX.deref())
        .into_iter()
        .chain(extra_regexes.iter())
    {
        for (start, end) in regex.find_iter(text) {
            if !joined_mask[start..end].iter().any(|x| *x) {
                joins.push(start..end);
                joined_mask[start..end].iter_mut().for_each(|x| *x = true);
            }
        }
    }

    joins.sort_by(|a, b| a.start.cmp(&b.start));

    let mut prev = 0;
    for range in joins {
        tokens.extend(split(&text[prev..range.start], split_func));
        prev = range.end;
        tokens.push(&text[range]);
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
    /// Extra language-specific characters to split text on.
    #[serde(default)]
    pub extra_split_chars: Vec<char>,
    /// Extra language-specific Regexes of which the matches will *not* be split into multiple tokens.
    #[serde(default)]
    pub extra_join_regexes: Vec<String>,
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
            extra_split_chars: Vec::new(),
            extra_join_regexes: Vec::new(),
        }
    }
}

/// The complete Tokenizer doing tagging, chunking and disambiguation.
#[derive(Serialize, Deserialize, Default)]
pub struct Tokenizer {
    pub(crate) rules: Vec<DisambiguationRule>,
    pub(crate) chunker: Option<Chunker>,
    pub(crate) tagger: Arc<Tagger>,
    pub(crate) options: TokenizerOptions,
}

impl Tokenizer {
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
        let mut current_char = 0;
        let token_strs = get_token_strs(
            text,
            &self.options.extra_split_chars,
            &self.options.extra_join_regexes,
        );
        let mut tokens: Vec<_> = token_strs
            .iter()
            .enumerate()
            .map(|(i, x)| {
                let char_start = current_char;
                let ptr = x.as_ptr() as usize;
                current_char += x.chars().count();

                let byte_start = ptr - text.as_ptr() as usize;
                let trimmed = x.trim();

                let is_sentence_start = i == 0;
                let is_sentence_end = i == token_strs.len() - 1;

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
