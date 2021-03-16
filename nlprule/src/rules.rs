//! Sets of grammatical error correction rules.

use crate::{rule::id::Selector, tokenizer::Tokenizer};
use crate::{rule::Rule, Error};
use crate::{spell::Spell, types::*, utils::parallelism::MaybeParallelRefIterator};
use fs_err::File;
use serde::{Deserialize, Serialize};
use std::{
    io::{BufReader, Read, Write},
    path::Path,
    sync::Arc,
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

#[derive(Serialize, Deserialize, Default)]
struct RulesFields {
    pub(crate) rules: Vec<Rule>,
    pub(crate) spell: Spell,
}

impl From<Rules> for RulesFields {
    fn from(rules: Rules) -> Self {
        RulesFields {
            rules: rules.rules,
            spell: rules.spell,
        }
    }
}

/// A set of grammatical error correction rules.
#[derive(Clone, Default, Serialize, Deserialize)]
pub struct Rules {
    pub(crate) rules: Vec<Rule>,
    pub(crate) spell: Spell,
    pub(crate) tokenizer: Arc<Tokenizer>,
}

impl Rules {
    /// Serializes the rules set to a writer.
    pub fn to_writer<W: Write>(&self, writer: &mut W) -> Result<(), Error> {
        // TODO: the .clone() here could be avoided
        let fields: RulesFields = self.clone().into();
        writer.write_all(&bincode::serialize(&fields)?)?;
        Ok(())
    }

    /// Creates a new rules set from a reader.
    pub fn from_reader<R: Read>(reader: R, tokenizer: Arc<Tokenizer>) -> Result<Self, Error> {
        let fields: RulesFields = bincode::deserialize_from(reader)?;
        let rules = Rules {
            rules: fields.rules,
            spell: fields.spell,
            tokenizer,
        };
        Ok(rules)
    }

    /// Creates a new rule set from a path to a binary.
    ///
    /// # Errors
    /// - If the file can not be opened.
    /// - If the file content can not be deserialized to a rules set.
    pub fn new<P: AsRef<Path>>(p: P, tokenizer: Arc<Tokenizer>) -> Result<Self, Error> {
        let reader = BufReader::new(File::open(p.as_ref())?);

        Self::from_reader(reader, tokenizer)
    }

    /// Gets the spellchecker associated with this rules set. The spellchecker always exists, even if spellchecking is disabled (default).
    pub fn spell(&self) -> &Spell {
        &self.spell
    }

    /// Mutably gets the spellchecker.
    pub fn spell_mut(&mut self) -> &mut Spell {
        &mut self.spell
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

    /// Compute the suggestions for the given tokens by checking all rules.
    pub fn apply(&self, tokens: &[Token]) -> Vec<Suggestion> {
        if tokens.is_empty() {
            return Vec::new();
        }

        let mut output: Vec<(usize, Suggestion)> = self
            .rules
            .maybe_par_iter()
            .enumerate()
            .filter(|(_, rule)| rule.enabled())
            .map(|(i, rule)| {
                let mut output = Vec::new();

                for suggestion in rule.apply(tokens, self.tokenizer.as_ref()) {
                    output.push((i, suggestion));
                }

                output
            })
            .flatten()
            .collect();

        output.extend(self.spell.suggest(tokens).into_iter().map(|x| (0, x)));

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
    pub fn suggest(&self, text: &str) -> Vec<Suggestion> {
        if text.is_empty() {
            return Vec::new();
        }

        let mut suggestions = Vec::new();
        let mut char_offset = 0;

        // get suggestions sentence by sentence
        for tokens in self.tokenizer.pipe(text) {
            if tokens.is_empty() {
                continue;
            }

            suggestions.extend(self.apply(&tokens).into_iter().map(|mut suggestion| {
                suggestion.rshift(char_offset);
                suggestion
            }));

            char_offset += tokens[0].sentence.chars().count();
        }

        suggestions
    }

    /// Correct a text by first tokenizing, then finding all suggestions and choosing the first replacement of each suggestion.
    pub fn correct(&self, text: &str) -> String {
        let suggestions = self.suggest(text);
        apply_suggestions(text, &suggestions)
    }
}

/// Correct a text by applying suggestions to it.
/// - In case of multiple possible replacements, always chooses the first one.
/// - In case of a suggestion without any replacements, ignores the suggestion.
pub fn apply_suggestions(text: &str, suggestions: &[Suggestion]) -> String {
    let mut offset: isize = 0;
    let mut chars: Vec<_> = text.chars().collect();

    for suggestion in suggestions {
        if let Some(replacement) = suggestion.replacements.get(0) {
            let replacement_chars: Vec<_> = replacement.chars().collect();

            chars.splice(
                (suggestion.start as isize + offset) as usize
                    ..(suggestion.end as isize + offset) as usize,
                replacement_chars.iter().cloned(),
            );
            offset = offset + replacement_chars.len() as isize
                - (suggestion.end - suggestion.start) as isize;
        }
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
