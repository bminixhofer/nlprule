//! A tokenizer to split raw text into tokens.
//! Tokens are assigned lemmas and part-of-speech tags by lookup from a [Tagger][tag::Tagger] and chunks containing
//! information about noun / verb and grammatical case by a statistical [Chunker][chunk::Chunker].
//! Tokens are *disambiguated* (i. e. information from the initial assignment is changed) in a rule-based way by
//! [DisambiguationRule][crate::rule::DisambiguationRule]s.

use crate::{
    rule::id::{Index, Selector},
    rule::MatchSentence,
    types::*,
    utils::{parallelism::MaybeParallelRefIterator, regex::Regex},
    Error,
};
use fs_err::File;
use serde::{Deserialize, Serialize};
use std::{
    io::{BufReader, Read},
    ops::Range,
    path::Path,
    sync::Arc,
};

pub mod chunk;
pub mod multiword;
pub mod tag;

use chunk::Chunker;
use multiword::MultiwordTagger;
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

/// Options for a tokenizer.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct TokenizerLangOptions {
    /// Whether to allow errors while constructing the tokenizer.
    pub allow_errors: bool,
    /// Whether to retain the last tag if disambiguation leads to an empty tag.
    /// Language-specific in LT so it has to be an option.
    pub retain_last: bool,
    /// Disambiguation Rule selectors to use in this tokenizer.
    #[serde(default)]
    pub ids: Vec<Selector>,
    /// Disambiguation Rule selectors to ignore in this tokenizer.
    #[serde(default)]
    pub ignore_ids: Vec<Selector>,
    /// Specific examples in the notation `{id}:{example_index}` which are known to fail.
    #[serde(default)]
    pub known_failures: Vec<String>,
    /// Extra language-specific characters to split text on.
    #[serde(default)]
    pub extra_split_chars: Vec<char>,
    /// Extra language-specific Regexes of which the matches will *not* be split into multiple tokens.
    #[serde(default)]
    pub extra_join_regexes: Vec<Regex>,
}

impl Default for TokenizerLangOptions {
    fn default() -> Self {
        TokenizerLangOptions {
            allow_errors: false,
            retain_last: false,
            ids: Vec::new(),
            ignore_ids: Vec::new(),
            known_failures: Vec::new(),
            extra_split_chars: Vec::new(),
            extra_join_regexes: Vec::new(),
        }
    }
}

/// An iterator over [IncompleteSentence]s. Has the same properties as [SentenceIter].
pub struct IncompleteSentenceIter<'t> {
    text: &'t str,
    splits: Vec<Range<usize>>,
    tokenizer: &'t Tokenizer,
    index: usize,
    position: Position,
}

impl<'t> Iterator for IncompleteSentenceIter<'t> {
    type Item = IncompleteSentence<'t>;

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
            .tokenize(&self.text[range.clone()])
            .map(|x| x.rshift(self.position));

        self.position += Position {
            char: self.text[range.clone()].chars().count(),
            byte: range.len(),
        };

        sentence
    }
}

/// An iterator over [Sentence]s. Has some key properties:
/// - Preceding whitespace is always included so the first sentence always starts at byte and char index zero.
/// - There are no gaps between sentences i.e. `sentence[i - 1].span().end() == sentence[i].span().start()`.
/// - Behavior for trailing whitespace is not defined. Can be included in the last sentence or not be part of any sentence.
pub struct SentenceIter<'t> {
    inner: IncompleteSentenceIter<'t>,
    tokenizer: &'t Tokenizer,
}

impl<'t> Iterator for SentenceIter<'t> {
    type Item = Sentence<'t>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .next()
            .map(|sentence| self.tokenizer.disambiguate(sentence).into_sentence())
    }
}

/// The complete Tokenizer doing tagging, chunking and disambiguation.
#[derive(Serialize, Deserialize, Default)]
pub struct Tokenizer {
    pub(crate) rules: Vec<DisambiguationRule>,
    pub(crate) chunker: Option<Chunker>,
    pub(crate) sentencizer: srx::Rules,
    pub(crate) multiword_tagger: Option<MultiwordTagger>,
    pub(crate) tagger: Arc<Tagger>,
    pub(crate) lang_options: TokenizerLangOptions,
}

impl Tokenizer {
    /// Creates a new tokenizer from a path to a binary.
    ///
    /// # Errors
    /// - If the file can not be opened.
    /// - If the file content can not be deserialized to a rules set.
    pub fn new<P: AsRef<Path>>(p: P) -> Result<Self, Error> {
        let reader = BufReader::new(File::open(p.as_ref())?);
        Ok(bincode::deserialize_from(reader)?)
    }

    /// Creates a new tokenizer from a reader.
    pub fn from_reader<R: Read>(reader: R) -> Result<Self, Error> {
        Ok(bincode::deserialize_from(reader)?)
    }

    /// Gets all disambigation rules in the order they are applied.
    pub fn rules(&self) -> &[DisambiguationRule] {
        &self.rules
    }

    /// Gets the lexical tagger.
    pub fn tagger(&self) -> &Arc<Tagger> {
        &self.tagger
    }

    /// Gets the chunker if one exists.
    pub fn chunker(&self) -> &Option<Chunker> {
        &self.chunker
    }

    pub(crate) fn lang_options(&self) -> &TokenizerLangOptions {
        &self.lang_options
    }

    pub(crate) fn disambiguate_up_to_id<'t>(
        &'t self,
        mut sentence: IncompleteSentence<'t>,
        id: Option<&Index>,
    ) -> IncompleteSentence<'t> {
        let n = id.map_or(self.rules.len(), |id| {
            self.rules.iter().position(|x| x.id == *id).unwrap()
        });
        let mut i = 0;

        while i < n {
            let complete_sentence = sentence.clone().into_sentence();
            let match_sentence = MatchSentence::new(&complete_sentence);

            let result = self.rules[i..n]
                .maybe_par_iter()
                .enumerate()
                .filter_map(|(j, rule)| {
                    let changes = rule.apply(&match_sentence);
                    if changes.is_empty() {
                        None
                    } else {
                        Some((j + i, changes))
                    }
                })
                .find_first(|_| true);

            if let Some((index, changes)) = result {
                self.rules[index].change(&mut sentence, &self, changes);
                i = index + 1;
            } else {
                i = n;
            }
        }

        sentence
    }

    /// Apply rule-based disambiguation to the tokens.
    /// This does not change the number of tokens, but can change the content arbitrarily.
    pub fn disambiguate<'t>(&'t self, sentence: IncompleteSentence<'t>) -> IncompleteSentence<'t> {
        self.disambiguate_up_to_id(sentence, None)
    }

    fn get_token_strs<'t>(&self, text: &'t str) -> Vec<&'t str> {
        let mut tokens = Vec::new();

        let split_char = |c: char| c.is_whitespace() || crate::utils::splitting_chars().contains(c);
        let split_text = |text: &'t str| {
            let mut tokens = Vec::new();
            for pretoken in split(text, split_char) {
                // if the token is in the dictionary, we add it right away
                if self.tagger.id_word(pretoken.into()).1.is_some() {
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
        tokens
    }

    /// Tokenize the given sentence. This applies chunking and tagging, but does not do disambiguation.
    // NB: this is not public because it could be easily misused by passing a text instead of one sentence.
    pub(crate) fn tokenize<'t>(&'t self, sentence: &'t str) -> Option<IncompleteSentence<'t>> {
        if sentence.trim().is_empty() {
            return None;
        }

        let token_strs = self.get_token_strs(sentence);

        let mut tokens: Vec<_> = token_strs
            .iter()
            .enumerate()
            .filter(|(_, token_text)| !token_text.trim().is_empty())
            .map(|(i, token_text)| {
                let token_ptr = token_text.as_ptr() as usize;
                let sentence_ptr = sentence.as_ptr() as usize;

                debug_assert!(token_ptr >= sentence_ptr); // see https://stackoverflow.com/q/38268529
                let byte_start = token_ptr - sentence_ptr;
                let char_start = sentence[..byte_start].chars().count();

                let trimmed = token_text.trim();

                let is_sentence_start = i == 0;
                let is_sentence_end = i == token_strs.len() - 1;

                IncompleteToken::new(
                    Word::new_with_tags(
                        self.tagger.id_word(trimmed.into()),
                        self.tagger.get_tags_with_options(
                            trimmed,
                            if is_sentence_start { Some(true) } else { None },
                            None,
                        ),
                    ),
                    Span::new(
                        byte_start..byte_start + token_text.len(),
                        char_start..char_start + token_text.chars().count(),
                    ),
                    is_sentence_end,
                    sentence[..byte_start].ends_with(char::is_whitespace),
                    Vec::new(),
                    None,
                )
            })
            .collect();

        let last_idx = tokens.len() - 1;
        *tokens[last_idx].is_sentence_end_mut() = true;

        let mut sentence = IncompleteSentence::new(tokens, sentence, &self.tagger);

        if let Some(chunker) = &self.chunker {
            chunker.apply(&mut sentence);
        }

        if let Some(multiword_tagger) = &self.multiword_tagger {
            multiword_tagger.apply(&mut sentence);
        }

        Some(sentence)
    }

    /// Splits the text into sentences and tokenizes each sentence.
    pub fn sentencize<'t>(&'t self, text: &'t str) -> IncompleteSentenceIter<'t> {
        IncompleteSentenceIter {
            text,
            splits: self.sentencizer.split_ranges(text),
            tokenizer: &self,
            index: 0,
            position: Position::default(),
        }
    }

    /// Applies the entire tokenization pipeline including sentencization, tagging, chunking and disambiguation.
    pub fn pipe<'t>(&'t self, text: &'t str) -> SentenceIter<'t> {
        SentenceIter {
            inner: self.sentencize(text),
            tokenizer: &self,
        }
    }
}
