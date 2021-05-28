//! A tokenizer to split raw text into tokens.
//! Tokens are assigned lemmas and part-of-speech tags by lookup from a [Tagger][tag::Tagger] and chunks containing
//! information about noun / verb and grammatical case by a statistical [Chunker][chunk::Chunker].
//! Tokens are *disambiguated* (i. e. information from the initial assignment is changed) in a rule-based way by
//! [DisambiguationRule][crate::rule::DisambiguationRule]s.

#[cfg(feature = "compile")]
mod compile;

use fs_err::File;
use std::ops::Range;

use crate::types::*;
use crate::{properties::*, utils::regex::Regex};
use lazy_static::lazy_static;
use once_cell::sync::OnceCell;
use serde::{Deserialize, Serialize};

use super::{tagger::Tagger, Component};

/// Split a text at the points where the given function is true.
/// Keeps the separators. See https://stackoverflow.com/a/40296745.
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

/// Options for a tokenizer.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub(crate) struct TokenizerLangOptions {
    /// Extra language-specific characters to split text on.
    #[serde(default)]
    pub extra_split_chars: Vec<char>,
    /// Extra language-specific Regexes of which the matches will *not* be split into multiple tokens.
    #[serde(default)]
    pub extra_join_regexes: Vec<Regex>,
}

/// The complete Tokenizer doing tagging, chunking and disambiguation.
#[derive(Serialize, Deserialize, Default, Clone)]
pub struct Tokenizer {
    whitelist: DefaultHashSet<String>,
    sentencizer: srx::Rules,
    tagger: Tagger,
    lang_options: TokenizerLangOptions,
    #[serde(skip)]
    properties: OnceCell<PropertiesMut>,
}

impl Tokenize for Tokenizer {
    fn properties(&self) -> PropertiesMut {
        lazy_static! {
            static ref PROPERTIES: PropertiesMut = Properties::default().write(&[Property::Tags]);
        }
        *PROPERTIES
    }

    fn tokenize<'t>(&'t self, text: &'t str) -> Box<dyn Iterator<Item = Sentence<'t>> + 't> {
        Box::new(SentenceIter {
            text,
            splits: self.sentencizer.split_ranges(text),
            tokenizer: &self,
            index: 0,
            position: Position::default(),
        })
    }

    fn tokenize_sentence<'t>(&'t self, sentence: &'t str) -> Option<Sentence<'t>> {
        if sentence.trim().is_empty() {
            return None;
        }

        let token_strs = self
            .get_token_ranges(sentence)
            .filter(|range| !sentence[range.clone()].trim().is_empty());

        let n_token_strs = token_strs.clone().count();

        let tokens: Vec<_> = token_strs
            .enumerate()
            .map(|(i, range)| {
                let byte_start = range.start;
                let char_start = sentence[..byte_start].chars().count();

                let token_text = sentence[range].trim();

                let is_sentence_start = i == 0;
                let is_sentence_end = i == n_token_strs - 1;

                Token::new(
                    token_text,
                    Span::new(
                        byte_start..byte_start + token_text.len(),
                        char_start..char_start + token_text.chars().count(),
                    ),
                    is_sentence_start,
                    is_sentence_end,
                    sentence[..byte_start].ends_with(char::is_whitespace),
                )
            })
            .collect();

        let mut sentence = Sentence::new(tokens, sentence, &self.tagger);
        let guard = self.property_guard(&mut sentence).expect("TODO");

        sentence = self.tagger.transform(sentence, guard).expect("TODO");

        Some(sentence)
    }
}

/// An iterator over sentences. Has some key properties:
/// - Preceding whitespace is always included so the first sentence always starts at byte and char index zero.
/// - There are no gaps between sentences i.e. `sentence[i - 1].span().end() == sentence[i].span().start()`.
/// - Behavior for trailing whitespace is not defined. Can be included in the last sentence or not be part of any sentence.
pub struct SentenceIter<'t> {
    text: &'t str,
    splits: Vec<Range<usize>>,
    tokenizer: &'t Tokenizer,
    index: usize,
    position: Position,
}

impl<'t> Iterator for SentenceIter<'t> {
    type Item = Sentence<'t>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index == self.splits.len() {
            return None;
        }

        let mut range = self.splits[self.index].clone();
        self.index += 1;

        // as long as the current sentence contains only whitespace, add the next sentence
        // in practice, this might never happen, but we can not make any assumption about
        // SRX rule behavior here.
        while self.text[range.clone()].trim().is_empty() && self.index < self.splits.len() {
            range.end = self.splits[self.index].end;
            self.index += 1;
        }

        let sentence = self
            .tokenizer
            .tokenize_sentence(&self.text[range.clone()])
            .map(|x| x.rshift(self.position));

        self.position += Position {
            char: self.text[range.clone()].chars().count(),
            byte: range.len(),
        };

        sentence
    }
}

impl Component for Tokenizer {
    fn name() -> &'static str {
        "tokenizer"
    }
}

impl Tokenizer {
    /// Gets the lexical tagger.
    pub fn tagger(&self) -> &Tagger {
        &self.tagger
    }

    fn get_token_ranges<'t>(
        &self,
        text: &'t str,
    ) -> impl ExactSizeIterator<Item = Range<usize>> + 't + Clone {
        let mut tokens = Vec::new();

        let split_char = |c: char| c.is_whitespace() || crate::utils::splitting_chars().contains(c);
        let split_text = |text: &'t str| {
            let mut tokens = Vec::new();
            for pretoken in split(text, split_char) {
                // if the token is in the dictionary, we add it right away
                if self.whitelist.contains(pretoken) {
                    tokens.push(pretoken);
                } else {
                    // otherwise, potentially split it again with `extra_split_chars` e. g. "-"
                    tokens.extend(split(pretoken, |c| {
                        split_char(c) || self.lang_options.extra_split_chars.contains(&c)
                    }));
                }
            }
            tokens
        };

        let mut joined_mask = vec![false; text.len()];
        let mut joins = Vec::new();

        for regex in self.lang_options.extra_join_regexes.iter() {
            for mat in regex.find_iter(text) {
                if !joined_mask[mat.start()..mat.end()].iter().any(|x| *x) {
                    joins.push(mat.start()..mat.end());
                    joined_mask[mat.start()..mat.end()]
                        .iter_mut()
                        .for_each(|x| *x = true);
                }
            }
        }

        joins.sort_by(|a, b| a.start.cmp(&b.start));

        let mut prev = 0;
        for range in joins {
            tokens.extend(split_text(&text[prev..range.start]));
            prev = range.end;
            tokens.push(&text[range]);
        }

        tokens.extend(split_text(&text[prev..text.len()]));
        tokens.into_iter().map(move |token| {
            let byte_start = (token.as_ptr() as usize)
                .checked_sub(text.as_ptr() as usize)
                .expect("Each token str is a slice of the text str.");

            byte_start..byte_start + token.len()
        })
    }
}
