use log::warn;
use nlprule::composition::Composition;
use nlprule::Token;
use std::collections::{HashMap, HashSet};

#[derive(Debug)]
pub struct Correction {
    pub start: usize,
    pub end: usize,
    pub text: Vec<String>,
}

impl std::cmp::PartialEq for Correction {
    fn eq(&self, other: &Correction) -> bool {
        let a: HashSet<&String> = self.text.iter().collect();
        let b: HashSet<&String> = other.text.iter().collect();

        a.intersection(&b).count() > 0 && other.start == self.start && other.end == self.end
    }
}

#[derive(Debug)]
pub struct Test {
    text: String,
    correction: Option<Correction>,
}

pub struct Rule {
    id: String,
    composition: Composition,
    tests: Vec<Test>,
    suggestions: Vec<String>,
    start: usize,
    end: usize,
}

impl Rule {
    pub fn apply<'a>(&self, tokens: &[Token<'a>]) -> Vec<Correction> {
        let refs: Vec<&Token> = tokens.iter().collect();
        let mut corrections = Vec::new();

        for i in 0..tokens.len() {
            if let Some(groups) = self.composition.apply(&refs[i..]) {
                let start_group = &groups[self.start];
                let end_group = &groups[self.end - 1];

                assert!(
                    !start_group.is_empty() && !end_group.is_empty(),
                    "groups must not be empty"
                );

                let start = start_group[0].char_span.0;
                let end = end_group[end_group.len() - 1].char_span.1;
                corrections.push(Correction {
                    start,
                    end,
                    text: self.suggestions.to_vec(),
                })
            }
        }

        corrections
    }

    pub fn test(&self) -> bool {
        let mut passes = Vec::new();

        for test in &self.tests {
            let tokens = Token::str_to_tokens(&test.text);
            let corrections = self.apply(&tokens);

            assert!(
                corrections.len() < 2,
                "test texts must have one or zero corrections"
            );

            let pass = match &test.correction {
                Some(correction) => corrections.len() == 1 && correction == &corrections[0],
                None => corrections.is_empty(),
            };

            if !pass {
                warn!(
                    "Rule {}: test \"{}\" of failed. Expected: {:#?}. Found: {:#?}.",
                    self.id, test.text, test.correction, corrections
                );
            }

            passes.push(pass);
        }

        passes.iter().all(|x| *x)
    }
}

mod structure_to_rule {
    use nlprule::composition::{Atom, Composition, MatchAtom, Quantifier, StringMatcher};
    use nlprule::structure;

    fn atom_from_token(token: &structure::Token) -> (Box<dyn Atom>, Quantifier) {
        let atom = MatchAtom::new(StringMatcher::new(token.text.clone()), |token| {
            token.lower.as_str()
        });

        (Box::new(atom), Quantifier::new(1, 1))
    }

    impl From<(structure::Rule, structure::ExtraInfo)> for super::Rule {
        fn from(data: (structure::Rule, structure::ExtraInfo)) -> super::Rule {
            let id = data.1.id;
            let mut start = None;
            let mut end = None;

            let mut atoms = Vec::new();

            for part in &data.0.pattern.parts {
                match part {
                    structure::PatternPart::Token(token) => atoms.push(atom_from_token(token)),
                    structure::PatternPart::Marker(marker) => {
                        start = Some(atoms.len());

                        for token in &marker.tokens {
                            atoms.push(atom_from_token(token));
                        }

                        end = Some(atoms.len());
                    }
                }
            }

            let start = start.unwrap_or(0);
            let end = end.unwrap_or_else(|| atoms.len());

            let suggestions = data
                .0
                .message
                .parts
                .iter()
                .filter_map(|x| match x {
                    structure::MessagePart::Suggestion(suggestion) => {
                        Some(suggestion.text.to_string())
                    }
                    structure::MessagePart::Text(_) => None,
                })
                .collect::<Vec<_>>();

            let mut tests = Vec::new();
            for example in &data.0.examples {
                let mut texts = Vec::new();
                let mut char_length = 0;
                let mut correction: Option<super::Correction> = None;

                for part in &example.parts {
                    match part {
                        structure::ExamplePart::Text(text) => {
                            texts.push(text.as_str());
                            char_length += text.chars().count();
                        }
                        structure::ExamplePart::Marker(marker) => {
                            assert!(
                                correction.is_none(),
                                "example must have one or zero markers"
                            );

                            texts.push(marker.text.as_str());
                            let length = marker.text.chars().count();

                            if let Some(correction_text) = &example.correction {
                                correction = Some(super::Correction {
                                    start: char_length,
                                    end: char_length + length,
                                    text: correction_text
                                        .split('|')
                                        .map(|x| x.to_string())
                                        .collect(),
                                });
                            }

                            char_length += marker.text.chars().count();
                        }
                    }
                }

                tests.push(super::Test {
                    text: texts.join(""),
                    correction,
                });
            }

            let composition = Composition::new(atoms);

            super::Rule {
                composition,
                tests,
                suggestions,
                start,
                end,
                id,
            }
        }
    }
}

fn main() {
    env_logger::init();
    let rules = nlprule::structure::read_rules("data/grammar.canonic.xml");
    let mut errors: HashMap<String, usize> = HashMap::new();

    let rules = rules
        .into_iter()
        .filter_map(|x| match x {
            Ok(rule) => Some(rule),
            Err(err) => {
                errors
                    .entry(format!("{}", err))
                    .and_modify(|x| *x += 1)
                    .or_insert(1);
                None
            }
        })
        .collect::<Vec<_>>();

    let mut errors: Vec<(String, usize)> = errors.into_iter().collect();
    errors.sort_by_key(|x| -(x.1 as i32));

    println!("Top errors: {:#?}", &errors[..5]);
    println!("Parsed rules: {}", rules.len());

    let rules: Vec<_> = rules.into_iter().map(Rule::from).collect();
    println!(
        "Rules passing tests: {}",
        rules
            .iter()
            .fold(0, |count, rule| count + rule.test() as usize)
    );
}
