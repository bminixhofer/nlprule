//! Sets of grammatical error correction rules.

use crate::rule::Rule;
use crate::tokenizer::Tokenizer;
use crate::types::*;
use crate::utils::parallelism::MaybeParallelRefIterator;
use serde::{Deserialize, Serialize};
use std::{
    fs::File,
    io::{BufReader, Read},
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
    /// Creates a new rules set from a file.
    pub fn new<P: AsRef<Path>>(p: P) -> bincode::Result<Self> {
        let reader = BufReader::new(File::open(p).unwrap());
        bincode::deserialize_from(reader)
    }

    /// Creates a new rules set from a reader.
    pub fn new_from<R: Read>(reader: R) -> bincode::Result<Self> {
        bincode::deserialize_from(reader)
    }

    /// All rules ordered by priority.
    pub fn rules(&self) -> &Vec<Rule> {
        &self.rules
    }

    /// Finds a rule by ID.
    pub fn rule(&self, id: &str) -> Option<&Rule> {
        self.rules.iter().find(|x| x.id() == id)
    }

    /// Compute the suggestions for the given tokens by checking all rules.
    pub fn suggest(&self, tokens: &[Token], tokenizer: &Tokenizer) -> Vec<Suggestion> {
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

        let mut mask = vec![false; tokens[0].text.chars().count()];

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
}

/// Correct a text by applying suggestions to it.
/// In the case of multiple possible replacements, always chooses the first one.
pub fn correct(text: &str, suggestions: &[Suggestion]) -> String {
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
