use lazy_static::lazy_static;
use onig::Regex;
use serde::{Deserialize, Serialize};
use std::{collections::HashSet, sync::Arc};
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

#[derive(Debug, Clone, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct WordData {
    pub lemma: String,
    pub pos: String,
}

impl WordData {
    pub fn new(lemma: String, pos: String) -> Self {
        WordData { lemma, pos }
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Word {
    pub text: String,
    pub tags: Vec<WordData>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IncompleteToken {
    pub word: Word,
    pub byte_span: (usize, usize),
    pub char_span: (usize, usize),
    pub is_sentence_end: bool,
    pub has_space_before: bool,
    pub chunks: Vec<String>,
}

impl<'a> From<IncompleteToken> for Token {
    fn from(data: IncompleteToken) -> Token {
        let mut word = data.word.clone();

        word.tags
            .push(WordData::new(data.word.text.to_string(), String::new()));

        if word.tags.iter().all(|x| x.pos.is_empty()) {
            word.tags
                .push(WordData::new(data.word.text.to_string(), "UNKNOWN".into()));
        }

        if data.is_sentence_end {
            word.tags
                .push(WordData::new(data.word.text.to_string(), "SENT_END".into()));
        }

        Token {
            word,
            byte_span: data.byte_span,
            char_span: data.char_span,
            has_space_before: data.has_space_before,
            chunks: data.chunks,
        }
    }
}

impl Word {
    pub fn new_with_tags(text: String, tags: Vec<WordData>) -> Self {
        Word { text, tags }
    }
}

#[derive(Debug)]
pub struct Token {
    pub word: Word,
    pub char_span: (usize, usize),
    pub byte_span: (usize, usize),
    pub has_space_before: bool,
    pub chunks: Vec<String>,
}

impl<'a> Token {
    fn sent_start() -> Token {
        Token {
            word: Word::new_with_tags(
                String::new(),
                vec![WordData::new(String::new(), "SENT_START".into())]
                    .into_iter()
                    .collect(),
            ),
            char_span: (0, 0),
            byte_span: (0, 0),
            has_space_before: false,
            chunks: Vec::new(),
        }
    }
}

pub fn finalize(tokens: Vec<IncompleteToken>) -> Vec<Token> {
    let mut finalized = vec![Token::sent_start()];
    finalized.extend(tokens.into_iter().map(|x| x.into()));

    finalized
}

#[derive(Serialize, Deserialize, Clone)]
pub struct TokenizerOptions {
    pub allow_errors: bool,
    pub retain_last: bool,
    pub use_compound_split_heuristic: bool,
    pub always_add_lower_tags: bool,
    #[serde(default)]
    pub ids: Vec<String>,
    #[serde(default)]
    pub ignore_ids: Vec<String>,
    #[serde(default)]
    pub known_failures: Vec<String>,
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
        }
    }
}

#[derive(Serialize, Deserialize, Default)]
pub struct Tokenizer {
    rules: Vec<DisambiguationRule>,
    chunker: Option<Chunker>,
    tagger: Arc<Tagger>,
    options: TokenizerOptions,
}

impl Tokenizer {
    #[cfg(feature = "compile")]
    pub fn from_xml<P: AsRef<std::path::Path>>(
        path: P,
        tagger: Arc<Tagger>,
        chunker: Option<Chunker>,
        options: TokenizerOptions,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        use log::warn;
        use std::convert::TryFrom;

        let rules = crate::from_structure::structure::read_disambiguation_rules(path);
        let mut error = None;

        let rules: Vec<_> = rules
            .into_iter()
            .filter_map(|x| match x {
                Ok((rule_structure, id)) => match DisambiguationRule::try_from(rule_structure) {
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
                },
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

    pub fn disambiguate_up_to_id(
        &self,
        mut tokens: Vec<IncompleteToken>,
        id: &str,
    ) -> Vec<IncompleteToken> {
        let mut previously_computed_tokens = None;

        for rule in &self.rules {
            if rule.id == id {
                break;
            }

            let x = rule.apply(tokens, &self, previously_computed_tokens);
            tokens = x.0;
            previously_computed_tokens = x.1;
        }

        tokens
    }

    pub fn disambiguate(&self, mut tokens: Vec<IncompleteToken>) -> Vec<IncompleteToken> {
        let mut previously_computed_tokens = None;

        for rule in &self.rules {
            let x = rule.apply(tokens, &self, previously_computed_tokens);
            tokens = x.0;
            previously_computed_tokens = x.1;
        }

        tokens
    }

    pub fn tokenize(&self, text: &str) -> Vec<IncompleteToken> {
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
                        trimmed.to_string(),
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
                }
            })
            .filter(|token| !token.word.text.is_empty())
            .collect();

        if !tokens.is_empty() {
            let last_idx = tokens.len() - 1;
            tokens[last_idx].is_sentence_end = true;

            if let Some(chunker) = &self.chunker {
                chunker.apply(text, &mut tokens);
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

    #[quickcheck]
    fn can_tokenize_anything(text: String) -> bool {
        lazy_static! {
            static ref TOKENIZER: Tokenizer = {
                let reader = flate2::read::GzDecoder::new(
                    File::open("../storage/en/tokenizer.bin.gz").unwrap(),
                );
                bincode::deserialize_from(reader).unwrap()
            };
        }

        TOKENIZER.tokenize(&text);
        true
    }
}
