use nlprule::composition::Composition;
use nlprule::Token;
use std::collections::HashMap;

#[derive(Debug)]
struct Correction {
    pub start: usize,
    pub end: usize,
    pub text: String,
}

#[derive(Debug)]
struct Test {
    text: String,
    correction: Option<Correction>,
}

pub struct Rule {
    composition: Composition,
    tests: Vec<Test>,
    suggestions: Vec<String>,
    start: usize,
    end: usize,
}

impl Rule {
    pub fn apply<'a>(&self, tokens: &[Token<'a>]) {
        for i in 0..tokens.len() {
            self.composition.apply(&tokens[i..]);
        }
    }

    pub fn test(&self) {
        for test in &self.tests {
            let tokens = Token::str_to_tokens(&test.text);
            self.apply(&tokens);
        }
    }
}

mod structure_to_rule {
    use nlprule::composition::{Atom, Composition, MatchAtom, Quantifier, StringMatcher};
    use nlprule::structure;

    fn atom_from_token(token: &structure::Token) -> (Box<dyn Atom>, Quantifier) {
        let atom = MatchAtom::new(StringMatcher::new(token.text.clone()), |token| token.text);

        (Box::new(atom), Quantifier::new(1, 1))
    }

    impl From<structure::Rule> for super::Rule {
        fn from(structure: structure::Rule) -> super::Rule {
            let mut start = None;
            let mut end = None;

            let mut atoms = Vec::new();

            for part in &structure.pattern.parts {
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

            let suggestions = structure
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
            for example in &structure.examples {
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

                            correction = Some(super::Correction {
                                start: char_length,
                                end: char_length + length,
                                text: example
                                    .correction
                                    .clone()
                                    .map(|x| {
                                        x.split('|')
                                            .next()
                                            .expect("correction must not be empty")
                                            .to_string()
                                    })
                                    .expect("example must have correction if it has marker"),
                            });
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
            }
        }
    }
}

fn main() {
    let rules = nlprule::structure::read_rules("data/grammar.canonic.xml");
    let mut errors: HashMap<String, usize> = HashMap::new();

    let mut rules = rules
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
    println!("{:#?}", rules[0]);
    let rule = Rule::from(rules.remove(0));

    println!("{:#?}", rule.test());
}
