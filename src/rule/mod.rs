use crate::composition::{Composition, MatchGraph};
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

#[derive(Debug)]
pub struct Match {
    index: usize,
}

impl Match {
    fn apply(&self, graph: &MatchGraph) -> String {
        graph.groups[self.index].tokens[0].text.to_string()
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
    fn apply(&self, groups: &MatchGraph) -> String {
        let mut output = Vec::new();

        for part in &self.parts {
            match part {
                SuggesterPart::Text(t) => output.push(t.clone()),
                SuggesterPart::Match(m) => output.push(m.apply(groups)),
            }
        }

        output.join("")
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
    pub fn apply<'a>(&self, tokens: &[Token<'a>]) -> Vec<Suggestion> {
        let refs: Vec<&Token> = tokens.iter().collect();
        let mut suggestions = Vec::new();

        for i in 0..tokens.len() {
            if let Some(graph) = self.composition.apply(&refs[i..]) {
                let start_group = &graph.groups[self.start];
                let end_group = &graph.groups[self.end - 1];

                let start = start_group.char_start;
                let end = end_group.char_end;
                suggestions.push(Suggestion {
                    start,
                    end,
                    text: self
                        .suggesters
                        .iter()
                        .map(|x| {
                            let suggestion = x.apply(&graph);

                            // adjust case
                            if !start_group.tokens.is_empty()
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
                        })
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
