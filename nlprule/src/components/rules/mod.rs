use log::info;
use serde::{Deserialize, Serialize};
use std::iter::FromIterator;

use fs_err::File;

use crate::properties::*;
use crate::rule::Rule;
use crate::types::*;
use crate::utils::parallelism::MaybeParallelRefIterator;
use crate::{
    properties::Transform,
    rule::{
        id::{Index, Selector},
        DisambiguationRule, MatchSentence,
    },
    types::Sentence,
};
use once_cell::sync::OnceCell;

use super::Component;

mod compile;

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Disambiguator {
    rules: Vec<DisambiguationRule>,
    #[serde(skip)]
    properties: OnceCell<PropertiesMut>,
}

impl Transform for Disambiguator {
    fn properties(&self) -> PropertiesMut {
        *self.properties.get_or_init(|| {
            self.rules
                .iter()
                .map(|rule| rule.compute_properties())
                .collect()
        })
    }

    fn transform<'t>(
        &'t self,
        sentence: Sentence<'t>,
    ) -> Result<Sentence<'t>, crate::properties::Error> {
        self.disambiguate_up_to_id(sentence, None)
    }

    fn test<TOK: Tokenize>(&self, tokenizer: TOK) -> Result<(), crate::Error> {
        let mut current_rules: Vec<&DisambiguationRule> = Vec::new();
        let mut passes = 0;

        for rule in self.rules() {
            let pipeline = tokenize::Pipeline::new((
                &tokenizer,
                current_rules
                    .iter()
                    .map(|x| (*x).clone())
                    .collect::<Disambiguator>(),
            ))?;

            if rule.test(&pipeline).is_ok() {
                passes += 1;
            }

            current_rules.push(rule);
        }

        info!(
            "{0} out of {1} Disambiguation Rule tests passed.",
            passes,
            self.rules.len()
        );

        if passes == self.rules().len() {
            Ok(())
        } else {
            Err(crate::Error::TestFailed)
        }
    }
}

impl Component for Disambiguator {
    fn name() -> &'static str {
        "disambiguator"
    }
}

impl Disambiguator {
    /// Gets all disambigation rules in the order they are applied.
    pub fn rules(&self) -> &[DisambiguationRule] {
        &self.rules
    }

    pub(crate) fn disambiguate_up_to_id<'t>(
        &'t self,
        mut sentence: Sentence<'t>,
        id: Option<&Index>,
    ) -> Result<Sentence<'t>, crate::properties::Error> {
        let n = id.map_or(self.rules.len(), |id| {
            self.rules.iter().position(|x| x.id == *id).unwrap()
        });
        let mut i = 0;

        let guard = self.property_guard(&mut sentence)?;

        while i < n {
            let match_sentence = MatchSentence::new(&sentence, guard.downgrade());

            let result = self.rules[i..n]
                .maybe_par_iter()
                .enumerate()
                .filter_map(|(j, rule)| {
                    let changes = rule.apply(&match_sentence);

                    match changes {
                        Ok(changes) => {
                            if changes.is_empty() {
                                None
                            } else {
                                Some(Ok((j + i, changes)))
                            }
                        }
                        Err(err) => Some(Err(err)),
                    }
                })
                .find_first(|_| true)
                .transpose()?;

            if let Some((index, changes)) = result {
                self.rules[index].change(&mut sentence, changes, guard)?;
                i = index + 1;
            } else {
                i = n;
            }
        }

        Ok(sentence)
    }
}

/// A set of grammatical error correction rules.
#[derive(Serialize, Deserialize, Default, Clone)]
pub struct Rules {
    rules: Vec<Rule>,
    #[serde(skip)]
    properties: OnceCell<Properties>,
}

impl Component for Rules {
    fn name() -> &'static str {
        "rules"
    }
}

impl Suggest for Rules {
    fn properties(&self) -> Properties {
        *self.properties.get_or_init(|| {
            self.rules
                .iter()
                .map(|rule| rule.compute_properties())
                .collect()
        })
    }

    fn suggest(&self, sentence: &Sentence) -> Result<Vec<Suggestion>, crate::properties::Error> {
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

    fn test<TOK: Tokenize>(&self, tokenizer: TOK) -> Result<(), crate::Error> {
        let mut passes = 0;

        for rule in self.rules() {
            if rule.test(&tokenizer).is_ok() {
                passes += 1;
            };
        }

        info!(
            "{0} out of {1} Grammar Rule tests passed.",
            passes,
            self.rules.len()
        );

        if passes == self.rules().len() {
            Ok(())
        } else {
            Err(crate::Error::TestFailed)
        }
    }
}

impl Rules {
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

impl IntoIterator for Disambiguator {
    type Item = DisambiguationRule;
    type IntoIter = std::vec::IntoIter<DisambiguationRule>;
    fn into_iter(self) -> Self::IntoIter {
        self.rules.into_iter()
    }
}

impl<R> FromIterator<R> for Disambiguator
where
    R: Into<DisambiguationRule>,
{
    fn from_iter<I: IntoIterator<Item = R>>(iter: I) -> Self {
        let rules: Vec<DisambiguationRule> = iter.into_iter().map(|x| x.into()).collect();
        Self {
            rules,
            properties: OnceCell::default(),
        }
    }
}
