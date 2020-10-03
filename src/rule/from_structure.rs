use crate::composition::{
    AndAtom, Atom, Composition, GenericMatcher, MatchAtom, Matcher, NotAtom, OffsetAtom, OrAtom,
    Part, Quantifier, TrueAtom, WordDataMatcher,
};
use crate::filter::get_filter;
use crate::rule;
use crate::tokenizer::{Word, WordData};
use crate::{structure, utils, Error};
use lazy_static::lazy_static;
use onig::Regex;
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

    // TODO: also reformat is_regex etc., maybe macro?
    let is_postag_regexp = match attribs.postag_regexp().as_deref() {
        Some("yes") => true,
        None => false,
        x => panic!("unknown is_postag_regexp value {:?}", x),
    };

    let negate = match attribs.negate().as_deref() {
        Some("yes") => true,
        None => false,
        x => panic!("unknown negate value {:?}", x),
    };

    let negate_pos = match attribs.negate_pos().as_deref() {
        Some("yes") => true,
        None => false,
        x => panic!("unknown negate_pos value {:?}", x),
    };

    let mut inflect_matcher = None;
    let mut pos_matcher = None;

    if let Some(text) = text {
        let matcher = if is_regex {
            let regex = utils::new_regex(text.trim(), true, case_sensitive);
            Matcher::new_regex(regex, negate)
        } else if case_sensitive {
            Matcher::new_string(text.trim().to_string(), negate)
        } else {
            Matcher::new_string(text.trim().to_lowercase(), negate)
        };

        if inflected {
            inflect_matcher = Some(matcher);
        } else {
            let text_atom: Box<dyn Atom> = if is_regex {
                Box::new(MatchAtom::new(matcher, |t, m| m.is_match(&t.word.text)))
            } else if case_sensitive {
                Box::new(MatchAtom::new(matcher, |t, m| m.is_match(&t.word.text)))
            } else {
                Box::new(MatchAtom::new(matcher, |t, m| {
                    m.is_match(&t.word.text.to_lowercase())
                }))
            };

            atoms.push(text_atom);
        }
    }

    if let Some(postag) = attribs.postag() {
        pos_matcher = Some(if is_postag_regexp {
            let regex = utils::new_regex(&postag.trim(), true, true);
            Matcher::new_regex(regex, negate_pos)
        } else {
            Matcher::new_string(postag.trim().to_string(), negate_pos)
        });
    }

    if pos_matcher.is_some() || inflect_matcher.is_some() {
        let matcher = WordDataMatcher::new(pos_matcher, inflect_matcher);

        let atom: Box<dyn Atom> = if case_sensitive {
            Box::new(MatchAtom::new(matcher, |t, m| {
                m.is_match(
                    &t.word
                        .tags
                        .iter()
                        .map(|x| (&x.pos, &x.lemma))
                        .collect::<Vec<_>>(),
                )
            }))
        } else {
            Box::new(MatchAtom::new(matcher, |t, m| {
                m.is_match(
                    &t.word
                        .tags
                        .iter()
                        .map(|x| (&x.pos, x.lemma.to_lowercase()))
                        .collect::<Vec<_>>(),
                )
            }))
        };

        atoms.push(atom);
    }

    if let Some(chunk) = attribs.chunk() {
        let chunk_atom = MatchAtom::new(
            Matcher::new_string(chunk.trim().to_string(), false),
            |t, m| m.is_slice_match(&t.chunks[..]),
        );

        atoms.push(Box::new(chunk_atom));
    }

    if let Some(space_before) = attribs.spacebefore() {
        let value = match space_before.as_str() {
            "yes" => true,
            "no" => false,
            _ => panic!("unknown spacebefore value {}", space_before),
        };

        atoms.push(Box::new(MatchAtom::new(
            GenericMatcher::new(value),
            |t, m| m.is_match(&t.has_space_before),
        )));
    }

    Box::new(AndAtom::new(atoms))
}

fn get_exceptions(token: &structure::Token, case_sensitive: bool) -> Box<dyn Atom> {
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
        Box::new(NotAtom::new(Box::new(OrAtom::new(exceptions))))
    } else {
        Box::new(TrueAtom {})
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

    atom = Box::new(AndAtom::new(vec![
        atom,
        get_exceptions(token, case_sensitive),
    ]));

    parts.push(Part::new(atom, quantifier, true));

    if let Some(to_skip) = token.skip.clone() {
        let to_skip = if to_skip == "-1" {
            max_matches()
        } else {
            to_skip.parse().expect("can't parse skip as usize or -1")
        };

        parts.push(Part::new(
            get_exceptions(token, case_sensitive),
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
                    let (start, end) = capture.pos(0).unwrap();

                    if end_index != start {
                        parts.push(rule::SuggesterPart::Text(
                            (&text[end_index..end]).to_string(),
                        ))
                    }

                    let index = capture
                        .at(1)
                        .unwrap()
                        .parse::<usize>()
                        .expect("match regex capture must be parsable as usize.")
                        - 1;

                    parts.push(rule::SuggesterPart::Match(rule::Match::new(
                        index, None, None,
                    )));
                    end_index = end;
                }

                if end_index < text.len() {
                    parts.push(rule::SuggesterPart::Text((&text[end_index..]).to_string()))
                }
            }
            structure::SuggestionPart::Match(m) => {
                let last_id = get_last_id(&composition.parts) as usize;
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
                        utils::new_regex(&regex_match, false, true),
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

fn get_last_id(parts: &[Part]) -> isize {
    parts.iter().fold(0, |a, x| a + x.visible as isize) - 1
}

fn parse_parallel_tokens(tokens: &[structure::Token], case_sensitive: bool) -> Vec<Box<dyn Atom>> {
    tokens
        .iter()
        .map(|x| {
            let mut parsed = parse_token(x, case_sensitive);

            if parsed.len() != 1 || parsed[0].quantifier.min != 1 || parsed[0].quantifier.max != 1 {
                panic!("control flow in parallel tokens is not implemented.")
            }

            parsed.remove(0).atom
        })
        .collect()
}

fn parse_token_combination(
    token_combination: &structure::TokenCombination,
    case_sensitive: bool,
) -> Vec<Part> {
    let atoms_to_add = match token_combination {
        structure::TokenCombination::Token(token) => parse_token(token, case_sensitive),
        structure::TokenCombination::And(tokens) => {
            let atom = AndAtom::new(parse_parallel_tokens(&tokens.tokens, case_sensitive));
            vec![Part::new(Box::new(atom), Quantifier::new(1, 1), true)]
        }
        structure::TokenCombination::Or(tokens) => {
            let atom = OrAtom::new(parse_parallel_tokens(&tokens.tokens, case_sensitive));
            vec![Part::new(Box::new(atom), Quantifier::new(1, 1), true)]
        }
        structure::TokenCombination::Unify(unify) => {
            let mut out = Vec::new();

            for token_combination in &unify.tokens {
                out.extend(match token_combination {
                    structure::UnifyTokenCombination::Token(token) => {
                        parse_token(token, case_sensitive)
                    }
                    structure::UnifyTokenCombination::And(tokens) => {
                        let atom =
                            AndAtom::new(parse_parallel_tokens(&tokens.tokens, case_sensitive));
                        vec![Part::new(Box::new(atom), Quantifier::new(1, 1), true)]
                    }
                    structure::UnifyTokenCombination::Or(tokens) => {
                        let atom =
                            OrAtom::new(parse_parallel_tokens(&tokens.tokens, case_sensitive));
                        vec![Part::new(Box::new(atom), Quantifier::new(1, 1), true)]
                    }
                    structure::UnifyTokenCombination::Feature(_) => vec![],
                });
            }

            out
        }
    };

    atoms_to_add
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
                    let atoms_to_add = parse_token_combination(token, case_sensitive);
                    composition_parts.extend(atoms_to_add);
                }

                end = Some(get_last_id(&composition_parts) + 1);
            }
            structure::PatternPart::And(tokens) => {
                let atom = AndAtom::new(parse_parallel_tokens(&tokens.tokens, case_sensitive));

                composition_parts.push(Part::new(Box::new(atom), Quantifier::new(1, 1), true));
            }
            structure::PatternPart::Or(tokens) => {
                let atom = OrAtom::new(parse_parallel_tokens(&tokens.tokens, case_sensitive));

                composition_parts.push(Part::new(Box::new(atom), Quantifier::new(1, 1), true));
            }
        }
    }

    let start = start.unwrap_or(0) as usize;
    let end = end.unwrap_or_else(|| get_last_id(&composition_parts) + 1) as usize;

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

fn parse_tag_form(form: &str) -> Word {
    lazy_static! {
        static ref REGEX: Regex = Regex::new(r"(.+?)\[(.+?)\]").unwrap();
    }

    let captures = REGEX.captures(form).unwrap();
    let word = captures.at(1).unwrap().to_string();
    let tags = captures.at(2).unwrap();

    let tags = tags
        .split(',')
        .filter_map(|x| {
            if x == "</S>" {
                // special symbol, presumably for SENT_END, can be ignored
                return None;
            }

            let parts: Vec<_> = x.split('/').collect();
            if parts.len() < 2 {
                None
            } else {
                Some(WordData::new(parts[0].to_string(), parts[1].to_string()))
            }
        })
        .collect();

    rule::Word::new_with_tags(word, tags)
}

impl From<structure::WordData> for WordData {
    fn from(data: structure::WordData) -> Self {
        WordData::new(data.lemma.unwrap_or_else(String::new), data.pos)
    }
}

fn parse_pos_filter(postag: &str, postag_regexp: Option<&str>) -> rule::POSFilter {
    match postag_regexp.as_deref() {
        Some("yes") => rule::POSFilter::Regex(utils::new_regex(&postag, true, true)),
        Some(_) | None => rule::POSFilter::String(postag.to_string()),
    }
}

impl TryFrom<structure::DisambiguationRule> for rule::DisambiguationRule {
    type Error = Error;

    fn try_from(
        data: structure::DisambiguationRule,
    ) -> Result<rule::DisambiguationRule, Self::Error> {
        // might need the pattern later so clone it here
        let (composition, start, end) = parse_pattern(data.pattern.clone());

        let antipatterns = if let Some(antipatterns) = data.antipatterns {
            antipatterns
                .into_iter()
                .map(|x| parse_pattern(x).0)
                .collect()
        } else {
            Vec::new()
        };

        let word_datas: Vec<_> = if let Some(wds) = data.disambig.word_datas {
            wds.into_iter()
                .map(|part| match part {
                    structure::DisambiguationPart::WordData(x) => either::Left(x.into()),
                    structure::DisambiguationPart::Match(x) => either::Right(parse_pos_filter(
                        &x.postag.unwrap(),
                        x.postag_regexp.as_deref(),
                    )),
                })
                .collect()
        } else {
            Vec::new()
        };

        let disambiguations = match data.disambig.action.as_deref() {
            Some("remove") => {
                if let Some(postag) = data.disambig.postag.as_ref() {
                    Ok(vec![rule::Disambiguation::Remove(either::Right(
                        parse_pos_filter(postag, Some("yes")),
                    ))])
                } else {
                    Ok(word_datas
                        .into_iter()
                        .map(rule::Disambiguation::Remove)
                        .collect())
                }
            }
            Some("add") => {
                if data.disambig.postag.is_some() {
                    panic!("postag not supported for `add`.")
                }

                Ok(word_datas
                    .into_iter()
                    .map(|x| x.left().expect("match not supported for `add`"))
                    .map(rule::Disambiguation::Add)
                    .collect())
            }
            Some("replace") => {
                if data.disambig.postag.is_some() {
                    panic!("postag not supported for `replace`.")
                }

                Ok(word_datas
                    .into_iter()
                    .map(|x| {
                        x.left()
                            .expect("match not supported for `replace` disambiguation")
                    })
                    .map(rule::Disambiguation::Replace)
                    .collect())
            }
            Some("ignore_spelling") => Ok(Vec::new()), // ignore_spelling can be ignored since we dont check spelling
            Some("filterall") => {
                let mut disambig = Vec::new();
                let mut marker_disambig = Vec::new();
                let mut has_marker = false;

                for part in &data.pattern.parts {
                    match part {
                        structure::PatternPart::Marker(marker) => {
                            has_marker = true;
                            for token in &marker.tokens {
                                let token = match token {
                                    structure::TokenCombination::Token(token) => token,
                                    structure::TokenCombination::And(tokens)
                                    | structure::TokenCombination::Or(tokens) => &tokens.tokens[0],
                                    structure::TokenCombination::Unify(_) => {
                                        panic!("`unify` not supported in `filterall`")
                                    }
                                };

                                marker_disambig.push(
                                    token.postag.as_ref().map(|x| {
                                        parse_pos_filter(x, token.postag_regexp.as_deref())
                                    }),
                                );
                            }
                        }
                        structure::PatternPart::Token(token) => disambig.push(
                            token
                                .postag
                                .as_ref()
                                .map(|x| parse_pos_filter(x, token.postag_regexp.as_deref())),
                        ),
                        structure::PatternPart::And(tokens)
                        | structure::PatternPart::Or(tokens) => {
                            disambig.push(tokens.tokens[0].postag.as_ref().map(|x| {
                                parse_pos_filter(x, tokens.tokens[0].postag_regexp.as_deref())
                            }))
                        }
                    }
                }

                let disambiguations = if has_marker {
                    marker_disambig
                } else {
                    disambig
                };

                Ok(disambiguations
                    .into_iter()
                    .map(|x| match x {
                        Some(filter) => rule::Disambiguation::Filter(filter),
                        None => rule::Disambiguation::Nop,
                    })
                    .collect())
            }
            Some("filter") => {
                if let Some(postag) = data.disambig.postag.as_ref() {
                    Ok(vec![rule::Disambiguation::Filter(parse_pos_filter(
                        postag,
                        Some("yes"),
                    ))])
                } else {
                    Ok(word_datas
                        .into_iter()
                        .map(|x| {
                            x.left()
                                .expect("match not supported for `filter` disambiguation")
                        })
                        .map(rule::Disambiguation::Limit)
                        .collect())
                }
            }
            Some("unify") => {
                for part in &data.pattern.parts {
                    if let structure::PatternPart::Marker(marker) = part {
                        match &marker.tokens[..] {
                            [structure::TokenCombination::Unify(_)] => {}
                            _ => panic!("only `unify` as only element in `marker` is implemented"),
                        }
                    }
                }

                Ok(vec![])
            }
            None => {
                if let Some(postag) = data.disambig.postag.as_ref() {
                    Ok(vec![rule::Disambiguation::Limit(WordData::new(
                        String::new(),
                        postag.to_string(),
                    ))])
                } else {
                    Ok(word_datas
                        .into_iter()
                        .map(|wd_or_filter| match wd_or_filter {
                            either::Left(wd) => rule::Disambiguation::Limit(wd),
                            either::Right(filter) => rule::Disambiguation::Filter(filter),
                        })
                        .collect())
                }
            }
            Some(x) => Err(Error::Unimplemented(format!("action {}", x))),
        }?;

        let filter = if let Some(filter_data) = data.filter {
            let args = filter_data
                .args
                .split(' ')
                .map(|x| {
                    let idx = x.find(':').unwrap();
                    (
                        x[..idx].to_string(),
                        x[(idx + ':'.len_utf8())..].to_string(),
                    )
                })
                .collect();

            Some(get_filter(
                filter_data.class.split('.').next_back().unwrap(),
                args,
            ))
        } else {
            None
        };

        let mut tests = Vec::new();

        if let Some(examples) = data.examples.as_ref() {
            for example in examples {
                let mut texts = Vec::new();
                let mut char_span: Option<(usize, usize)> = None;
                let mut char_length = 0;

                for part in &example.parts {
                    match part {
                        structure::ExamplePart::Text(text) => {
                            texts.push(text.as_str());
                            char_length += text.chars().count();
                        }
                        structure::ExamplePart::Marker(marker) => {
                            if char_span.is_some() {
                                return Err(Error::Unexpected(
                                    "example must have one or zero markers".into(),
                                ));
                            }

                            texts.push(marker.text.as_str());
                            let length = marker.text.chars().count();

                            char_span = Some((char_length, char_length + length));

                            char_length += marker.text.chars().count();
                        }
                    }
                }

                let text = texts.join("");

                let test = match example.kind.as_str() {
                    "untouched" => rule::DisambiguationTest::Unchanged(text),
                    "ambiguous" => rule::DisambiguationTest::Changed(rule::DisambiguationChange {
                        text,
                        before: parse_tag_form(
                            example
                                .inputform
                                .as_ref()
                                .expect("must have inputform when ambiguous example"),
                        ),
                        after: parse_tag_form(
                            &example
                                .outputform
                                .as_ref()
                                .expect("must have inputform when ambiguous example"),
                        ),
                        char_span: char_span.expect("must have marker when ambiguous example"),
                    }),
                    x => panic!("unknown disambiguation example type {}", x),
                };

                tests.push(test);
            }
        }

        Ok(rule::DisambiguationRule {
            composition,
            filter,
            antipatterns,
            disambiguations,
            start,
            end,
            tests,
            id: String::new(),
        })
    }
}
