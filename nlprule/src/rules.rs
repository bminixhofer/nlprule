//! Sets of grammatical error correction rules.

use crate::tokenizer::Tokenizer;
use crate::types::*;
use crate::utils::parallelism::MaybeParallelRefIterator;
use crate::{rule::Rule, Error};
use fs_err::File;
use serde::{Deserialize, Serialize};
use std::{
    io::{BufReader, Read},
    iter::{FromIterator, IntoIterator, Iterator},
    path::Path,
};

/// Options for a rule set.
#[derive(Serialize, Deserialize, Clone)]
pub struct RulesOptions {
    /// Whether to allow errors while constructing the rules.
    pub allow_errors: bool,
    /// Grammar Rule IDs to use in this set.
    #[serde(default)]
    pub ids: Vec<String>,
    /// Grammar Rule IDs to ignore in this set.
    #[serde(default)]
    pub ignore_ids: Vec<String>,
}

impl Default for RulesOptions {
    fn default() -> Self {
        RulesOptions {
            allow_errors: true,
            ids: Vec::new(),
            ignore_ids: Vec::new(),
        }
    }
}

/// A set of grammatical error correction rules.
#[derive(Serialize, Deserialize, Default)]
pub struct Rules {
    pub(crate) rules: Vec<Rule>,
}

impl Rules {
    /// Creates a new rules set from a path to a binary.
    ///
    /// # Errors
    /// - If the file can not be opened.
    /// - If the file content can not be deserialized to a rules set.
    pub fn new<P: AsRef<Path>>(p: P) -> Result<Self, Error> {
        let reader = BufReader::new(File::open(p)?);
        Ok(bincode::deserialize_from(reader)?)
    }

    /// Creates a new rules set from a reader.
    pub fn from_reader<R: Read>(reader: R) -> Result<Self, Error> {
        Ok(bincode::deserialize_from(reader)?)
    }

    /// All rules ordered by priority.
    pub fn rules(&self) -> &[Rule] {
        &self.rules
    }

    /// Finds a rule by ID.
    pub fn rule(&self, id: &str) -> Option<&Rule> {
        self.rules.iter().find(|x| x.id() == id)
    }

    /// Compute the suggestions for the given tokens by checking all rules.
    pub fn apply(&self, tokens: &[Token], tokenizer: &Tokenizer) -> Vec<Suggestion> {
        if tokens.is_empty() {
            return Vec::new();
        }

        let mut output: Vec<(usize, Suggestion)> = self
            .rules
            .maybe_par_iter()
            .enumerate()
            .filter(|(_, x)| x.on())
            .map(|(i, rule)| {
                let mut output = Vec::new();

                for suggestion in rule.apply(tokens, tokenizer) {
                    output.push((i, suggestion));
                }

                output
            })
            .flatten()
            .collect();

        output.sort_by(|(ia, a), (ib, b)| a.start.cmp(&b.start).then_with(|| ib.cmp(ia)));

        let mut mask = vec![false; tokens[0].sentence.chars().count()];

        output
            .into_iter()
            .filter_map(|(_, suggestion)| {
                if mask[suggestion.start..suggestion.end].iter().all(|x| !x) {
                    mask[suggestion.start..suggestion.end]
                        .iter_mut()
                        .for_each(|x| *x = true);
                    Some(suggestion)
                } else {
                    None
                }
            })
            .collect()
    }

    /// Compute the suggestions for a text by checking all rules.
    pub fn suggest(&self, text: &str, tokenizer: &Tokenizer) -> Vec<Suggestion> {
        if text.is_empty() {
            return Vec::new();
        }

        let mut suggestions = Vec::new();
        let mut char_offset = 0;

        // get suggestions sentence by sentence
        for tokens in tokenizer.pipe(text) {
            suggestions.extend(
                self.apply(&tokens, tokenizer)
                    .into_iter()
                    .map(|mut suggestion| {
                        suggestion.rshift(char_offset);
                        suggestion
                    }),
            );

            char_offset += tokens[0].sentence.chars().count();
        }

        suggestions
    }

    /// Correct a text by first tokenizing, then finding all suggestions and choosing the first replacement of each suggestion.
    pub fn correct(&self, text: &str, tokenizer: &Tokenizer) -> String {
        let suggestions = self.suggest(text, tokenizer);
        apply_suggestions(text, &suggestions)
    }

    /// A referential iterator.
    pub fn iter(&self) -> RulesIter {
        RulesIter::new(self)
    }
}

/// Correct a text by applying suggestions to it.
/// In the case of multiple possible replacements, always chooses the first one.
pub fn apply_suggestions(text: &str, suggestions: &[Suggestion]) -> String {
    let mut offset: isize = 0;
    let mut chars: Vec<_> = text.chars().collect();

    for suggestion in suggestions {
        let replacement: Vec<_> = suggestion.replacements[0].chars().collect();
        chars.splice(
            (suggestion.start as isize + offset) as usize
                ..(suggestion.end as isize + offset) as usize,
            replacement.iter().cloned(),
        );
        offset = offset + replacement.len() as isize - (suggestion.end - suggestion.start) as isize;
    }

    chars.into_iter().collect()
}

/// A wrapping helper iterator.
pub struct RulesIter<'a> {
    inner: std::slice::Iter<'a, Rule>,
}

impl<'a> RulesIter<'a> {
    fn new(owner: &'a Rules) -> Self {
        Self {
            inner: owner.rules.iter(),
        }
    }
}

impl<'a> Iterator for RulesIter<'a> {
    type Item = &'a Rule;
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

pub struct RulesIntoIter {
    inner: std::vec::IntoIter<Rule>,
}

impl<'a> Iterator for RulesIntoIter {
    type Item = Rule;
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

impl IntoIterator for Rules {
    type Item = Rule;
    type IntoIter = RulesIntoIter;
    fn into_iter(self) -> Self::IntoIter {
        RulesIntoIter {
            inner: self.rules.into_iter(),
        }
    }
}

impl<R> FromIterator<R> for Rules
where
    R: Into<Rule>,
{
    fn from_iter<I: IntoIterator<Item = R>>(iter: I) -> Self {
        let rules = iter.into_iter().map(|x| x.into()).collect::<Vec<Rule>>();
        Self { rules }
    }
}
