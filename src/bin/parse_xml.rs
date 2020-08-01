use log::warn;
use nlprule::composition::Composition;
use nlprule::{utils, Token};
use std::collections::{HashMap, HashSet};

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
struct Match {
    index: usize,
}

impl Match {
    fn apply(&self, groups: &[Vec<&Token>]) -> String {
        groups[self.index][0].text.to_string()
    }
}

#[derive(Debug)]
enum SuggesterPart {
    Text(String),
    Match(Match),
}

#[derive(Debug)]
pub struct Suggester {
    parts: Vec<SuggesterPart>,
}

impl Suggester {
    fn apply(&self, groups: &[Vec<&Token>]) -> String {
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
            if let Some(groups) = self.composition.apply(&refs[i..]) {
                let start_group = &groups[self.start];
                let end_group = &groups[self.end - 1];

                assert!(
                    !start_group.is_empty() && !end_group.is_empty(),
                    "groups must not be empty"
                );

                let start = start_group[0].char_span.0;
                let end = end_group[end_group.len() - 1].char_span.1;
                suggestions.push(Suggestion {
                    start,
                    end,
                    text: self
                        .suggesters
                        .iter()
                        .map(|x| {
                            let suggestion = x.apply(&groups);

                            // adjust case
                            if start_group[0].is_sentence_start
                                || start_group[0]
                                    .text
                                    .chars()
                                    .next()
                                    .expect("token must have at least one char")
                                    .is_uppercase()
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
            let tokens = Token::str_to_tokens(&test.text);
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

mod structure_to_rule {
    use lazy_static::lazy_static;
    use nlprule::composition::{
        Atom, Composition, MatchAtom, Quantifier, RegexMatcher, StringMatcher,
    };
    use nlprule::{structure, utils};
    use regex::{Regex, RegexBuilder};

    fn atom_from_token(token: &structure::Token) -> (Box<dyn Atom>, Quantifier) {
        let is_regex = token.regexp.clone().map_or(false, |x| x == "yes");

        let atom = if is_regex {
            let regex = utils::fix_regex(&token.text);
            let regex = RegexBuilder::new(&regex)
                .case_insensitive(true)
                .build()
                .expect("invalid regex");

            Box::new(MatchAtom::new(RegexMatcher::new(regex), |token| {
                token.lower.as_str()
            })) as Box<dyn Atom>
        } else {
            Box::new(MatchAtom::new(
                StringMatcher::new(token.text.to_lowercase()),
                |token| token.lower.as_str(),
            )) as Box<dyn Atom>
        };

        (atom, Quantifier::new(1, 1))
    }

    impl From<Vec<structure::SuggestionPart>> for super::Suggester {
        fn from(data: Vec<structure::SuggestionPart>) -> super::Suggester {
            let mut parts = Vec::new();

            lazy_static! {
                static ref MATCH_REGEX: Regex = Regex::new(r"\\(\d)").unwrap();
            }

            for part in data {
                match part {
                    structure::SuggestionPart::Text(text) => {
                        let mut end_index = 0;

                        for capture in MATCH_REGEX.captures_iter(&text) {
                            let mat = capture.get(0).unwrap();
                            if end_index != mat.start() {
                                parts.push(super::SuggesterPart::Text(
                                    (&text[end_index..mat.start()]).to_string(),
                                ))
                            }

                            let index = capture
                                .get(1)
                                .unwrap()
                                .as_str()
                                .parse::<usize>()
                                .expect("match regex capture must be parsable as usize.")
                                - 1;

                            parts.push(super::SuggesterPart::Match(super::Match { index }));
                            end_index = mat.end();
                        }

                        if end_index < text.len() {
                            parts.push(super::SuggesterPart::Text((&text[end_index..]).to_string()))
                        }
                    }
                }
            }

            super::Suggester { parts }
        }
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

            let suggesters = data
                .0
                .message
                .parts
                .into_iter()
                .filter_map(|x| match x {
                    structure::MessagePart::Suggestion(suggestion) => Some(suggestion.parts.into()),
                    structure::MessagePart::Text(_) => None,
                })
                .collect::<Vec<super::Suggester>>();

            let mut tests = Vec::new();
            for example in &data.0.examples {
                let mut texts = Vec::new();
                let mut char_length = 0;
                let mut suggestion: Option<super::Suggestion> = None;

                for part in &example.parts {
                    match part {
                        structure::ExamplePart::Text(text) => {
                            texts.push(text.as_str());
                            char_length += text.chars().count();
                        }
                        structure::ExamplePart::Marker(marker) => {
                            assert!(
                                suggestion.is_none(),
                                "example must have one or zero markers"
                            );

                            texts.push(marker.text.as_str());
                            let length = marker.text.chars().count();

                            if let Some(correction_text) = &example.correction {
                                suggestion = Some(super::Suggestion {
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
                    suggestion,
                });
            }

            let composition = Composition::new(atoms);

            super::Rule {
                composition,
                tests,
                suggesters,
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

    println!(
        "Top errors: {:#?}",
        &errors[..std::cmp::min(10, errors.len())]
    );
    println!("Parsed rules: {}", rules.len());

    let rules: Vec<_> = rules.into_iter().map(Rule::from).collect();
    println!(
        "Rules passing tests: {}",
        rules
            .iter()
            .fold(0, |count, rule| count + rule.test() as usize)
    );
}
