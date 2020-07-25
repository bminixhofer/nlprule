use serde_value::Value;
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

struct Rule {
    rule: seqrule::Rule<Value>,
    tests: Vec<Test>,
}

mod structure_to_rule {
    use nlprule::structure;
    use seqrule::{Atom, Match, Quantifier, Rule};
    use serde_value::Value;

    fn atom_from_token(token: &structure::Token) -> Atom<Value> {
        Atom::new(
            Box::new(token.text.clone().with_key(Vec::new())),
            Quantifier::new(1, 1),
        )
    }

    impl From<structure::Rule> for super::Rule {
        fn from(structure: structure::Rule) -> super::Rule {
            let mut start = 0;
            let mut end = 0;

            let mut atoms = Vec::new();

            for part in &structure.pattern.parts {
                match part {
                    structure::PatternPart::Token(token) => atoms.push(atom_from_token(token)),
                    structure::PatternPart::Marker(marker) => {
                        start = atoms.len();

                        for token in &marker.tokens {
                            atoms.push(atom_from_token(token));
                        }

                        end = atoms.len();
                    }
                }
            }

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

            let rule = Rule::new(atoms);

            super::Rule { rule, tests }
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
    println!("{:#?}", Rule::from(rules.remove(0)).tests);
}
