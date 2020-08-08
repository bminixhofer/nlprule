use crate::composition::{Composition, Group, MatchGraph};
use crate::tokenizer::{tokenize, Token};
use crate::utils;
use log::{info, warn};
use std::collections::HashSet;

pub mod from_structure;

#[derive(Debug)]
pub struct Suggestion {
    pub start: usize,
    pub end: usize,
    pub text: Vec<String>,
}

impl std::cmp::PartialEq for Suggestion {
    fn eq(&self, other: &Suggestion) -> bool {
        let a: HashSet<&String> = self.text.iter().collect();
        let b: HashSet<&String> = other.text.iter().collect();

        a.intersection(&b).count() > 0 && other.start == self.start && other.end == self.end
    }
}

#[derive(Debug)]
pub struct Test {
    text: String,
    suggestion: Option<Suggestion>,
}

pub struct Match {
    index: usize,
    conversion: Box<dyn Fn(&str) -> String>,
}

impl std::fmt::Debug for Match {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(fmt, "Match {{ index: {:?} }}", self.index)?;
        Ok(())
    }
}

impl Match {
    fn apply(&self, graph: &MatchGraph) -> String {
        (self.conversion)(
            graph
                .by_id(self.index)
                .unwrap_or_else(|| panic!("group must exist in graph: {}", self.index))
                .tokens[0]
                .text,
        )
    }

    fn new(index: usize, conversion: Box<dyn Fn(&str) -> String>) -> Self {
        Match { index, conversion }
    }
}

#[derive(Debug)]
pub enum SuggesterPart {
    Text(String),
    Match(Match),
}

#[derive(Debug)]
pub struct Suggester {
    parts: Vec<SuggesterPart>,
}

impl Suggester {
    fn apply(&self, groups: &MatchGraph, start_group: &Group, _end_group: &Group) -> String {
        let mut output = Vec::new();
        let mut only_text = true;

        for part in &self.parts {
            match part {
                SuggesterPart::Text(t) => output.push(t.clone()),
                SuggesterPart::Match(m) => {
                    only_text = false;
                    output.push(m.apply(groups));
                }
            }
        }

        let suggestion = output.join("");

        // if the suggestion contains only text, make it title case if:
        // * at sentence start
        // * the replaced text is title case
        if only_text
            && !start_group.tokens.is_empty()
            && (start_group.tokens[0].is_sentence_start
                || start_group.tokens[0]
                    .text
                    .chars()
                    .next()
                    .expect("token must have at least one char")
                    .is_uppercase())
        {
            utils::apply_to_first(&suggestion, |x| x.to_uppercase().collect())
        } else {
            suggestion
        }
    }
}

pub struct Rule {
    id: String,
    composition: Composition,
    tests: Vec<Test>,
    suggesters: Vec<Suggester>,
    start: usize,
    end: usize,
}

impl Rule {
    pub fn set_id(&mut self, id: String) {
        self.id = id;
    }

    pub fn apply<'a>(&self, tokens: &[Token<'a>]) -> Vec<Suggestion> {
        let refs: Vec<&Token> = tokens.iter().collect();
        let mut suggestions = Vec::new();

        for i in 0..tokens.len() {
            if let Some(graph) = self.composition.apply(&refs[i..]) {
                let start_group = graph
                    .by_id(self.start)
                    .unwrap_or_else(|| panic!("group must exist in graph: {}", self.start));
                let end_group = graph
                    .by_id(self.end - 1)
                    .unwrap_or_else(|| panic!("group must exist in graph: {}", self.end - 1));

                let start = start_group.char_start;
                let end = end_group.char_end;
                suggestions.push(Suggestion {
                    start,
                    end,
                    text: self
                        .suggesters
                        .iter()
                        .map(|x| x.apply(&graph, start_group, end_group))
                        .collect(),
                })
            }
        }

        suggestions
    }

    pub fn test(&self) -> bool {
        let mut passes = Vec::new();

        for test in &self.tests {
            let tokens = tokenize(&test.text);
            info!("Tokens: {:#?}", tokens);
            let suggestions = self.apply(&tokens);

            assert!(
                suggestions.len() < 2,
                format!(
                    "{} test texts must have one or zero corrections {:?}",
                    self.id, suggestions
                )
            );

            let pass = match &test.suggestion {
                Some(correct_suggestion) => {
                    suggestions.len() == 1 && correct_suggestion == &suggestions[0]
                }
                None => suggestions.is_empty(),
            };

            if !pass {
                warn!(
                    "Rule {}: test \"{}\" failed. Expected: {:#?}. Found: {:#?}.",
                    self.id, test.text, test.suggestion, suggestions
                );
            }

            passes.push(pass);
        }

        passes.iter().all(|x| *x)
    }
}
