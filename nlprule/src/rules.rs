//! Sets of grammatical error correction rules.

use crate::properties::*;
use crate::types::*;
use crate::utils::parallelism::MaybeParallelRefIterator;
use crate::{rule::id::Selector, rule::MatchSentence, rule::Rule, tokenizer::Tokenizer, Error};
use fs_err::File;
use once_cell::sync::OnceCell;
use serde::{Deserialize, Serialize};
use std::{
    io::{BufReader, Read, Write},
    iter::FromIterator,
    path::Path,
};

/// Language-dependent options for a rule set.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct RulesLangOptions {
    /// Whether to allow errors while constructing the rules.
    pub allow_errors: bool,
    /// Grammar Rule selectors to use in this set.
    #[serde(default)]
    pub ids: Vec<Selector>,
    /// Grammar Rule selectors to ignore in this set.
    #[serde(default)]
    pub ignore_ids: Vec<Selector>,
}

impl Default for RulesLangOptions {
    fn default() -> Self {
        RulesLangOptions {
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
    #[serde(skip)]
    pub(crate) properties: OnceCell<Properties>,
}

impl ReadProperties for Rules {
    fn properties(&self) -> Properties {
        *self
            .properties
            .get_or_init(|| self.rules.iter().map(ReadProperties::properties).collect())
    }
}

impl Rules {
    /// Creates a new rule set from a path to a binary.
    ///
    /// # Errors
    /// - If the file can not be opened.
    /// - If the file content can not be deserialized to a rules set.
    pub fn new<P: AsRef<Path>>(p: P) -> Result<Self, Error> {
        let reader = BufReader::new(File::open(p.as_ref())?);
        let rules: Rules = bincode::deserialize_from(reader)?;
        Ok(rules)
    }

    /// Creates a new rules set from a reader.
    pub fn from_reader<R: Read>(reader: R) -> Result<Self, Error> {
        Ok(bincode::deserialize_from(reader)?)
    }

    /// Serializes this rules set to a writer.
    pub fn to_writer<W: Write>(&self, writer: W) -> Result<(), Error> {
        Ok(bincode::serialize_into(writer, &self)?)
    }

    /// All rules ordered by priority.
    pub fn rules(&self) -> &[Rule] {
        &self.rules
    }

    /// All rules ordered by priority (mutable).
    pub fn rules_mut(&mut self) -> &mut [Rule] {
        &mut self.rules
    }

    /// Returns an iterator over all rules matching the selector.
    pub fn select<'a>(&'a self, selector: &'a Selector) -> RulesIter<'a> {
        RulesIter {
            inner: self.rules.iter(),
            selector: Some(selector),
        }
    }

    /// Returns an iterator over all rules matching the selector (mutable).
    pub fn select_mut<'a>(&'a mut self, selector: &'a Selector) -> RulesIterMut<'a> {
        RulesIterMut {
            inner: self.rules.iter_mut(),
            selector: Some(selector),
        }
    }

    /// Compute the suggestions for the given sentence by checking all rules.
    pub fn apply(&self, sentence: &Sentence) -> Result<Vec<Suggestion>, crate::properties::Error> {
        let sentence = MatchSentence::new(sentence, self.property_guard(sentence)?);

        let mut output: Vec<(usize, Suggestion)> = self
            .rules
            .maybe_par_iter()
            .enumerate()
            .filter(|(_, rule)| rule.enabled())
            .map(|(i, rule)| {
                let mut output = Vec::new();

                for suggestion in rule.apply(&sentence) {
                    match suggestion {
                        Ok(suggestion) => output.push((i, suggestion)),
                        Err(err) => return Err(err),
                    }
                }

                Ok(output)
            })
            .collect::<Result<Vec<Vec<_>>, crate::properties::Error>>()?
            .into_iter()
            .flatten()
            .collect();

        output.sort_by(|(ia, a), (ib, b)| {
            a.span()
                .char()
                .start
                .cmp(&b.span().char().start)
                .then_with(|| ib.cmp(ia))
        });

        let mut mask = vec![false; sentence.text().chars().count()];

        Ok(output
            .into_iter()
            .filter_map(|(_, suggestion)| {
                let span = suggestion.span().clone().lshift(sentence.span().start());

                if mask[span.char().clone()].iter().all(|x| !x) {
                    mask[span.char().clone()].iter_mut().for_each(|x| *x = true);
                    Some(suggestion)
                } else {
                    None
                }
            })
            .collect())
    }

    /// Compute the suggestions for a text by checking all rules.
    pub fn suggest(
        &self,
        text: &str,
        tokenizer: &Tokenizer,
    ) -> Result<Vec<Suggestion>, crate::properties::Error> {
        if text.is_empty() {
            return Ok(Vec::new());
        }

        let mut suggestions = Vec::new();

        // get suggestions sentence by sentence
        for sentence in tokenizer.pipe(text) {
            suggestions.extend(self.apply(&sentence?)?);
        }

        Ok(suggestions)
    }

    /// Correct a text by first tokenizing, then finding all suggestions and choosing the first replacement of each suggestion.
    pub fn correct(
        &self,
        text: &str,
        tokenizer: &Tokenizer,
    ) -> Result<String, crate::properties::Error> {
        let suggestions = self.suggest(text, tokenizer)?;
        Ok(apply_suggestions(text, &suggestions))
    }
}

/// Correct a text by applying suggestions to it.
/// In the case of multiple possible replacements, always chooses the first one.
pub fn apply_suggestions(text: &str, suggestions: &[Suggestion]) -> String {
    let mut offset: isize = 0;
    let mut chars: Vec<_> = text.chars().collect();

    for suggestion in suggestions {
        let replacement: Vec<_> = suggestion.replacements()[0].chars().collect();
        chars.splice(
            (suggestion.span().char().start as isize + offset) as usize
                ..(suggestion.span().char().end as isize + offset) as usize,
            replacement.iter().cloned(),
        );
        offset = offset + replacement.len() as isize - suggestion.span().char().len() as isize;
    }

    chars.into_iter().collect()
}

/// An iterator over references to rules.
pub struct RulesIter<'a> {
    selector: Option<&'a Selector>,
    inner: std::slice::Iter<'a, Rule>,
}

impl<'a> Iterator for RulesIter<'a> {
    type Item = &'a Rule;
    fn next(&mut self) -> Option<Self::Item> {
        let selector = self.selector.as_ref();

        self.inner
            .find(|rule| selector.map_or(true, |s| s.is_match(rule.id())))
    }
}

/// An iterator over mutable references to rules.
pub struct RulesIterMut<'a> {
    selector: Option<&'a Selector>,
    inner: std::slice::IterMut<'a, Rule>,
}

impl<'a> Iterator for RulesIterMut<'a> {
    type Item = &'a mut Rule;
    fn next(&mut self) -> Option<Self::Item> {
        let selector = self.selector.as_ref();

        self.inner
            .find(|rule| selector.map_or(true, |s| s.is_match(rule.id())))
    }
}

impl IntoIterator for Rules {
    type Item = Rule;
    type IntoIter = std::vec::IntoIter<Rule>;
    fn into_iter(self) -> Self::IntoIter {
        self.rules.into_iter()
    }
}

impl<R> FromIterator<R> for Rules
where
    R: Into<Rule>,
{
    fn from_iter<I: IntoIterator<Item = R>>(iter: I) -> Self {
        let rules: Vec<Rule> = iter.into_iter().map(|x| x.into()).collect();
        Self {
            rules,
            properties: OnceCell::default(),
        }
    }
}
