use crate::composition::{
    AndAtom, Atom, Composition, GenericMatcher, MatchAtom, NotAtom, OffsetAtom, OrAtom, Part,
    Quantifier, RegexMatcher, StringMatcher, TrueAtom,
};
use crate::rule;
use crate::tokenizer::Token;
use crate::{structure, utils, Error};
use lazy_static::lazy_static;
use regex::{Regex, RegexBuilder};
use std::convert::TryFrom;

// TODO: should be an option in config OR restricted to one sentence
fn max_matches() -> usize {
    20
}

fn parse_match_attribs(
    attribs: impl structure::MatchAttributes,
    text: Option<&str>,
    case_sensitive: bool,
) -> Box<dyn Atom> {
    let mut atoms: Vec<Box<dyn Atom>> = Vec::new();

    let case_sensitive = if let Some(case_sensitive) = attribs.case_sensitive() {
        match case_sensitive.as_str() {
            "yes" => true,
            "no" => false,
            x => panic!("unknown case_sensitive value {}", x),
        }
    } else {
        case_sensitive
    };

    let inflected = if let Some(inflected) = attribs.inflected() {
        match inflected.as_str() {
            "yes" => true,
            "no" => false,
            x => panic!("unknown inflected value {}", x),
        }
    } else {
        false
    };

    let is_regex = if let Some(regexp) = attribs.regexp() {
        match regexp.as_str() {
            "yes" => true,
            x => panic!("unknown regexp value {}", x),
        }
    } else {
        false
    };

    macro_rules! make_atom {
        ($matcher:expr) => {
            if case_sensitive && inflected {
                Box::new(MatchAtom::new($matcher, |token: &Token| {
                    &token.inflections[..]
                }))
            } else if case_sensitive {
                Box::new(MatchAtom::new($matcher, |token: &Token| token.text))
            } else if inflected {
                Box::new(MatchAtom::new($matcher, |token: &Token| {
                    &token.lower_inflections[..]
                }))
            } else {
                Box::new(MatchAtom::new($matcher, |token: &Token| {
                    token.lower.as_str()
                }))
            }
        };
    }

    if let Some(text) = text {
        let text_atom: Box<dyn Atom> = if is_regex {
            let regex = utils::fix_regex(&text.trim(), true);
            let regex = RegexBuilder::new(&regex)
                .case_insensitive(!case_sensitive)
                .build()
                .expect("invalid regex");
            let matcher = RegexMatcher::new(regex);

            make_atom!(matcher)
        } else {
            let text = if case_sensitive {
                text.to_string()
            } else {
                text.to_lowercase()
            };

            let matcher = StringMatcher::new(text.trim().to_string());

            make_atom!(matcher)
        };

        atoms.push(text_atom);
    }

    if let Some(postag) = attribs.postag() {
        let tag_atom = MatchAtom::new(StringMatcher::new(postag.trim().to_string()), |token| {
            &token.postags[..]
        });

        atoms.push(Box::new(tag_atom));
    }

    if let Some(space_before) = attribs.spacebefore() {
        let value = match space_before.as_str() {
            "yes" => true,
            "no" => false,
            _ => panic!("unknown spacebefore value {}", space_before),
        };

        atoms.push(Box::new(MatchAtom::new(
            GenericMatcher::new(value),
            |token| &token.has_space_before,
        )));
    }

    if atoms.is_empty() {
        Box::new(TrueAtom::new())
    } else {
        let mut out_atom: Box<dyn Atom> = Box::new(AndAtom::new(atoms));

        if let Some(negate) = attribs.negate() {
            match negate.as_str() {
                "yes" => out_atom = Box::new(NotAtom::new(out_atom)),
                _ => panic!("unknown negate value {}", negate),
            }
        }

        out_atom
    }
}

fn parse_token(token: &structure::Token, case_sensitive: bool) -> Vec<Part> {
    let mut parts = Vec::new();
    let text = if let Some(parts) = &token.parts {
        parts.iter().find_map(|x| match x {
            structure::TokenPart::Text(text) => Some(text.as_str()),
            _ => None,
        })
    } else {
        None
    };

    let min = token
        .min
        .clone()
        .map(|x| {
            if x == "-1" {
                max_matches()
            } else {
                x.parse().expect("can't parse min as usize")
            }
        })
        .unwrap_or(1usize);
    let max = token
        .max
        .clone()
        .map(|x| {
            if x == "-1" {
                max_matches()
            } else {
                x.parse().expect("can't parse max as usize")
            }
        })
        .unwrap_or(1usize);

    let quantifier = Quantifier::new(min, max);
    let mut atom = parse_match_attribs(token, text, case_sensitive);

    if let Some(parts) = &token.parts {
        let exceptions = parts
            .iter()
            .filter_map(|x| match x {
                structure::TokenPart::Exception(x) => Some(x),
                _ => None,
            })
            .map(|x| {
                let exception_text = if let Some(exception_text) = &x.text {
                    Some(exception_text.as_str())
                } else {
                    None
                };
                let mut atom = parse_match_attribs(x, exception_text, case_sensitive);

                let offset = if let Some(scope) = &x.scope {
                    match scope.as_str() {
                        "next" => 1,
                        "current" => 0,
                        "previous" => -1,
                        _ => panic!("unknown scope value {}", scope),
                    }
                } else {
                    0
                };

                if offset != 0 {
                    atom = Box::new(OffsetAtom::new(atom, offset));
                }

                atom
            })
            .collect::<Vec<_>>();

        if !exceptions.is_empty() {
            atom = Box::new(AndAtom::new(vec![
                atom,
                Box::new(NotAtom::new(Box::new(OrAtom::new(exceptions)))),
            ]));
        }
    }

    parts.push(Part::new(atom, quantifier, true));

    if let Some(to_skip) = token.skip.clone() {
        let to_skip = if to_skip == "-1" {
            max_matches()
        } else {
            to_skip.parse().expect("can't parse skip as usize or -1")
        };
        parts.push(Part::new(
            Box::new(TrueAtom::new()),
            Quantifier::new(0, to_skip),
            false,
        ));
    }

    parts
}

fn parse_suggestion(
    data: Vec<structure::SuggestionPart>,
    composition: &Composition,
) -> rule::Suggester {
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

                    parts.push(rule::SuggesterPart::Match(rule::Match::new(
                        index, None, None,
                    )));
                    end_index = mat.end();
                }

                if end_index < text.len() {
                    parts.push(rule::SuggesterPart::Text((&text[end_index..]).to_string()))
                }
            }
            structure::SuggestionPart::Match(m) => {
                let last_id = get_last_id(&composition.parts);
                let mut id =
                    m.no.parse::<usize>()
                        .expect("no must be parsable as usize.")
                        - 1;

                if id > last_id {
                    id = last_id;
                }

                let case_conversion = if let Some(conversion) = &m.case_conversion {
                    Some(conversion.as_str())
                } else {
                    None
                };

                let replacer = match (m.regexp_match, m.regexp_replace) {
                    (Some(regex_match), Some(regex_replace)) => Some((
                        Regex::new(&utils::fix_regex(&regex_match, false))
                            .expect("invalid regex_match regex."),
                        utils::fix_regex_replacement(&regex_replace),
                    )),
                    _ => None,
                };

                parts.push(rule::SuggesterPart::Match(rule::Match::new(
                    id,
                    match case_conversion {
                        Some("alllower") => Some(Box::new(|x| x.to_lowercase())),
                        Some("startlower") => Some(Box::new(|x| {
                            utils::apply_to_first(x, |c| c.to_lowercase().collect())
                        })),
                        Some("startupper") => Some(Box::new(|x| {
                            utils::apply_to_first(x, |c| c.to_uppercase().collect())
                        })),
                        Some("allupper") => Some(Box::new(|x| x.to_uppercase())),
                        Some(x) => panic!("case conversion {} not supported.", x),
                        None => None,
                    },
                    replacer,
                )));
            }
        }
    }

    rule::Suggester { parts }
}

fn get_last_id(parts: &[Part]) -> usize {
    parts.iter().fold(0, |a, x| a + x.visible as usize) - 1
}

fn parse_pattern(pattern: structure::Pattern) -> (Composition, usize, usize) {
    let mut start = None;
    let mut end = None;

    let mut composition_parts = Vec::new();
    let case_sensitive = match &pattern.case_sensitive {
        Some(string) => string == "yes",
        None => false,
    };

    for part in &pattern.parts {
        match part {
            structure::PatternPart::Token(token) => {
                composition_parts.extend(parse_token(token, case_sensitive))
            }
            structure::PatternPart::Marker(marker) => {
                start = Some(get_last_id(&composition_parts) + 1);

                for token in &marker.tokens {
                    let atoms_to_add = parse_token(token, case_sensitive);
                    composition_parts.extend(atoms_to_add);
                }

                end = Some(get_last_id(&composition_parts) + 1);
            }
        }
    }

    let start = start.unwrap_or(0);
    let end = end.unwrap_or_else(|| get_last_id(&composition_parts) + 1);

    let composition = Composition::new(composition_parts);

    (composition, start, end)
}

impl TryFrom<structure::Rule> for rule::Rule {
    type Error = Error;

    fn try_from(data: structure::Rule) -> Result<rule::Rule, Self::Error> {
        let (composition, start, end) = parse_pattern(data.pattern);

        let antipatterns = if let Some(antipatterns) = data.antipatterns {
            antipatterns
                .into_iter()
                .map(|x| parse_pattern(x).0)
                .collect()
        } else {
            Vec::new()
        };

        let suggesters = data
            .message
            .parts
            .into_iter()
            .filter_map(|x| match x {
                structure::MessagePart::Suggestion(suggestion) => {
                    Some(parse_suggestion(suggestion.parts, &composition))
                }
                structure::MessagePart::Text(_) => None, // the actual message is ignored at the moment
                structure::MessagePart::Match(_) => None,
            })
            .chain(
                data.suggestions
                    .unwrap_or_else(Vec::new)
                    .into_iter()
                    .map(|x| parse_suggestion(x.parts, &composition)),
            )
            .collect::<Vec<rule::Suggester>>();

        if suggesters.is_empty() {
            return Err(Error::Unimplemented("rule with no suggestion".into()));
        }

        let mut tests = Vec::new();
        for example in &data.examples {
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

        Ok(rule::Rule {
            composition,
            antipatterns,
            tests,
            suggesters,
            start,
            end,
            id: String::new(),
        })
    }
}
