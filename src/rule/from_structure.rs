use crate::composition::{Atom, Composition, MatchAtom, Quantifier, RegexMatcher, StringMatcher};
use crate::rule;
use crate::tokenizer::Token;
use crate::{structure, utils, Error};
use lazy_static::lazy_static;
use regex::{Regex, RegexBuilder};
use std::convert::TryFrom;

fn atom_from_token(token: &structure::Token, case_sensitive: bool) -> (Box<dyn Atom>, Quantifier) {
    let case_sensitive = match &token.case_sensitive {
        Some(string) => string == "yes",
        None => case_sensitive,
    };

    let is_regex = token.regexp.clone().map_or(false, |x| x == "yes");
    let accessor: Box<dyn for<'a> Fn(&'a Token) -> &'a str> = if case_sensitive {
        Box::new(|token: &Token| token.text)
    } else {
        Box::new(|token: &Token| token.lower.as_str())
    };

    let atom = if is_regex {
        let regex = utils::fix_regex(&token.text);
        let regex = RegexBuilder::new(&regex)
            .case_insensitive(!case_sensitive)
            .build()
            .expect("invalid regex");
        let matcher = RegexMatcher::new(regex);
        Box::new(MatchAtom::new(matcher, accessor)) as Box<dyn Atom>
    } else {
        let text = if case_sensitive {
            token.text.clone()
        } else {
            token.text.to_lowercase()
        };

        Box::new(MatchAtom::new(StringMatcher::new(text), accessor)) as Box<dyn Atom>
    };

    (atom, Quantifier::new(1, 1))
}

impl From<Vec<structure::SuggestionPart>> for rule::Suggester {
    fn from(data: Vec<structure::SuggestionPart>) -> rule::Suggester {
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
                            parts.push(rule::SuggesterPart::Text(
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

                        parts.push(rule::SuggesterPart::Match(rule::Match { index }));
                        end_index = mat.end();
                    }

                    if end_index < text.len() {
                        parts.push(rule::SuggesterPart::Text((&text[end_index..]).to_string()))
                    }
                }
            }
        }

        rule::Suggester { parts }
    }
}

impl TryFrom<(structure::Rule, structure::ExtraInfo)> for rule::Rule {
    type Error = Error;

    fn try_from(data: (structure::Rule, structure::ExtraInfo)) -> Result<rule::Rule, Self::Error> {
        let id = data.1.id;
        let mut start = None;
        let mut end = None;

        let mut atoms = Vec::new();
        let case_sensitive = match &data.0.pattern.case_sensitive {
            Some(string) => string == "yes",
            None => false,
        };

        for part in &data.0.pattern.parts {
            match part {
                structure::PatternPart::Token(token) => {
                    atoms.push(atom_from_token(token, case_sensitive))
                }
                structure::PatternPart::Marker(marker) => {
                    start = Some(atoms.len());

                    for token in &marker.tokens {
                        atoms.push(atom_from_token(token, case_sensitive));
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
            .collect::<Vec<rule::Suggester>>();

        if suggesters.is_empty() {
            return Err(Error::Unimplemented("rule with no suggestion".into()));
        }

        let mut tests = Vec::new();
        for example in &data.0.examples {
            let mut texts = Vec::new();
            let mut char_length = 0;
            let mut suggestion: Option<rule::Suggestion> = None;

            for part in &example.parts {
                match part {
                    structure::ExamplePart::Text(text) => {
                        texts.push(text.as_str());
                        char_length += text.chars().count();
                    }
                    structure::ExamplePart::Marker(marker) => {
                        if suggestion.is_some() {
                            return Err(Error::Unexpected(
                                "example must have one or zero markers".into(),
                            ));
                        }

                        texts.push(marker.text.as_str());
                        let length = marker.text.chars().count();

                        if let Some(correction_text) = &example.correction {
                            suggestion = Some(rule::Suggestion {
                                start: char_length,
                                end: char_length + length,
                                text: correction_text.split('|').map(|x| x.to_string()).collect(),
                            });
                        }

                        char_length += marker.text.chars().count();
                    }
                }
            }

            tests.push(rule::Test {
                text: texts.join(""),
                suggestion,
            });
        }

        let composition = Composition::new(atoms);

        Ok(rule::Rule {
            composition,
            tests,
            suggesters,
            start,
            end,
            id,
        })
    }
}
