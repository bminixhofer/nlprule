use std::{io::BufReader, ops::Range};

use crate::compile::{BuildInfo, Error};
use crate::types::*;
use crate::{utils, utils::regex::Regex};
use fs_err::File;
use lazy_static::lazy_static;
use serde::Deserialize;
use serde_xml_rs::EventReader;

use crate::rule::disambiguation::*;
use crate::rule::engine::composition::concrete::*;
use crate::rule::engine::composition::*;
use crate::rule::engine::*;
use crate::rule::grammar::*;
use crate::rule::{id::Index, DisambiguationRule, Rule, Unification};

use super::Category;

// this is set arbitrarily at the moment, could be an option
#[inline]
fn max_matches() -> usize {
    20
}

fn parse_match_attribs(
    attribs: impl super::MatchAttributes,
    text: Option<&str>,
    case_sensitive: bool,
    text_match_idx: Option<usize>,
    info: &mut BuildInfo,
) -> Result<Atom, Error> {
    let mut atoms: Vec<Atom> = Vec::new();

    let case_sensitive = match attribs.case_sensitive().as_deref() {
        Some("yes") => true,
        Some("no") => false,
        None => case_sensitive,
        x => panic!("unknown case_sensitive value {:?}", x),
    };

    let inflected = match attribs.inflected().as_deref() {
        Some("yes") => true,
        Some("no") => false,
        None => false,
        x => panic!("unknown inflected value {:?}", x),
    };

    let is_regex = match attribs.regexp().as_deref() {
        Some("yes") => true,
        None => false,
        x => panic!("unknown regexp value {:?}", x),
    };

    let is_postag_regexp = match attribs.postag_regexp().as_deref() {
        Some("yes") => true,
        None => false,
        x => panic!("unknown postag_regexp value {:?}", x),
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

    if text.is_some() || text_match_idx.is_some() {
        let matcher = if is_regex {
            if let Some(text) = text {
                let regex = Regex::from_java_regex(text.trim(), true, case_sensitive);
                Matcher::new_regex(regex?, negate, inflected)
            } else {
                return Err(Error::Unexpected("`text` must be set if regex".into()));
            }
        } else {
            Matcher::new_string(
                text_match_idx.map_or_else(
                    || {
                        either::Left(
                            text.expect("either `text_match_idx` or `text` are set.")
                                .trim()
                                .to_string(),
                        )
                    },
                    // this is validated in Composition::new, otherwise creating a graph id is not valid!
                    |id| either::Right(GraphId(id)),
                ),
                negate,
                case_sensitive,
                inflected,
            )
        };

        if inflected {
            inflect_matcher = Some(TextMatcher::new(matcher, info)?);
        } else {
            atoms.push(
                (TextAtom {
                    matcher: TextMatcher::new(matcher, info)?,
                })
                .into(),
            );
        }
    }

    if let Some(postag) = attribs.postag() {
        let raw_matcher = if is_postag_regexp {
            let regex = Regex::from_java_regex(&postag.trim(), true, true);
            Matcher::new_regex(regex?, negate_pos, true)
        } else {
            Matcher::new_string(
                either::Left(postag.trim().to_string()),
                negate_pos,
                true,
                true,
            )
        };
        pos_matcher = Some(PosMatcher::new(raw_matcher, info)?);
    }

    if pos_matcher.is_some() || inflect_matcher.is_some() {
        let matcher = WordDataMatcher {
            pos_matcher,
            inflect_matcher,
        };
        atoms.push(
            (WordDataAtom {
                matcher,
                case_sensitive,
            })
            .into(),
        );
    }

    match (attribs.chunk(), attribs.chunk_re()) {
        (Some(chunk), None) => {
            let chunk_atom = ChunkAtom {
                matcher: Matcher::new_string(
                    either::Left(chunk.trim().to_string()),
                    false,
                    true,
                    true,
                ),
            };
            atoms.push(chunk_atom.into());
        }
        (None, Some(chunk_re)) => {
            let regex = Regex::from_java_regex(chunk_re.trim(), true, true)?;
            let chunk_atom = ChunkAtom {
                matcher: Matcher::new_regex(regex, false, true),
            };
            atoms.push(chunk_atom.into());
        }
        (None, None) => {}
        _ => panic!("unexpected combination of chunk / chunk_re values."),
    }

    if let Some(chunk) = attribs.chunk() {
        let chunk_atom = ChunkAtom {
            matcher: Matcher::new_string(either::Left(chunk.trim().to_string()), false, true, true),
        };

        atoms.push(chunk_atom.into());
    }

    if let Some(space_before) = attribs.spacebefore() {
        let value = match space_before.as_str() {
            "yes" => true,
            "no" => false,
            _ => panic!("unknown spacebefore value {}", space_before),
        };

        atoms.push((SpaceBeforeAtom { value }).into());
    }

    Ok(AndAtom::and(atoms))
}

fn get_exceptions(
    token: &super::Token,
    case_sensitive: bool,
    only_shifted: bool,
    info: &mut BuildInfo,
) -> Result<Atom, Error> {
    if let Some(parts) = &token.parts {
        let exceptions: Vec<Atom> = parts
            .iter()
            .filter_map(|x| match x {
                super::TokenPart::Exception(x) => Some(x),
                _ => None,
            })
            .filter_map(|x| {
                let exception_text = if let Some(exception_text) = &x.text {
                    Some(exception_text.as_str())
                } else {
                    None
                };
                let mut atom =
                    match parse_match_attribs(x, exception_text, case_sensitive, None, info) {
                        Ok(atom) => atom,
                        Err(err) => return Some(Err(err)),
                    };

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
                    atom = OffsetAtom::new(atom, offset).into();
                }

                if !only_shifted || (offset != 0) {
                    Some(Ok(atom))
                } else {
                    None
                }
            })
            .collect::<Result<Vec<_>, Error>>()?;
        Ok(NotAtom::not(OrAtom::or(exceptions)))
    } else {
        Ok((TrueAtom {}).into())
    }
}

fn parse_token(
    token: &super::Token,
    case_sensitive: bool,
    info: &mut BuildInfo,
) -> Result<Vec<Part>, Error> {
    let mut parts = Vec::new();
    let text = if let Some(parts) = &token.parts {
        parts.iter().find_map(|x| match x {
            super::TokenPart::Text(text) => Some(text.as_str()),
            _ => None,
        })
    } else {
        None
    };

    let text_match_idx = if let Some(parts) = &token.parts {
        match parts.iter().find_map(|x| match x {
            super::TokenPart::Sub(sub) => Some(sub.no.parse::<usize>().map(|x| x + 1)),
            _ => None,
        }) {
            None => None,
            Some(Ok(x)) => Some(x),
            Some(Err(err)) => return Err(err.into()),
        }
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
    let mut max = token
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
    if min > 1 && max == 1 {
        max = max_matches();
    }

    let quantifier = Quantifier::new(min, max);
    let mut atom = parse_match_attribs(token, text, case_sensitive, text_match_idx, info)?;
    atom = AndAtom::and(vec![
        atom,
        get_exceptions(token, case_sensitive, false, info)?,
    ]);

    parts.push(Part {
        atom,
        quantifier,
        visible: true,
        greedy: true,
        unify: token.unify.as_ref().map(|x| x == "yes"),
    });

    if let Some(to_skip) = token.skip.clone() {
        let to_skip = if to_skip == "-1" {
            max_matches()
        } else {
            to_skip.parse().expect("can't parse skip as usize or -1")
        };

        parts.push(Part {
            atom: get_exceptions(token, case_sensitive, true, info)?,
            quantifier: Quantifier::new(0, to_skip),
            visible: false,
            greedy: false,
            unify: None,
        });
    }

    Ok(parts)
}

fn parse_match(m: super::Match, engine: &Engine, info: &mut BuildInfo) -> Result<Match, Error> {
    if m.postag.is_some()
        || m.postag_regex.is_some()
        || m.postag_replace.is_some()
        || m.text.is_some()
    {
        // this would need a fully functional PosReplacer to work
        return Err(Error::Unimplemented(
            "postag, postag_regex, postag_replace and text in `match` are not implemented.".into(),
        ));
    }

    if m.include_skipped.is_some() {
        return Err(Error::Unimplemented(
            "include_skipped in `match` is not implemented.".into(),
        ));
    }

    let id =
        m.no.parse::<usize>()
            .expect("no must be parsable as usize.");

    let case_conversion = if let Some(conversion) = &m.case_conversion {
        Some(conversion.as_str())
    } else {
        None
    };

    let pos_replacer = if let Some(postag) = m.postag {
        if postag.contains("+DT") || postag.contains("+INDT") {
            return Err(Error::Unimplemented(
                "+DT and +INDT determiners are not implemented.".into(),
            ));
        }

        let matcher = match m.postag_regex.as_deref() {
            Some("yes") => {
                let regex = Regex::from_java_regex(&postag, true, false)?;
                Matcher::new_regex(regex, false, true)
            }
            None => Matcher::new_string(either::Left(postag), false, false, true),
            x => panic!("unknown postag_regex value {:?}", x),
        };
        Some(PosReplacer {
            matcher: PosMatcher::new(matcher, info)?,
        })
    } else {
        None
    };

    let regex_replacer = match (m.regexp_match, m.regexp_replace) {
        (Some(regex_match), Some(regex_replace)) => Some((
            Regex::from_java_regex(&regex_match, false, true)?,
            regex_replace,
        )),
        _ => None,
    };

    Ok(Match {
        id: engine.to_graph_id(id)?,
        conversion: match case_conversion {
            Some("alllower") => Conversion::AllLower,
            Some("startlower") => Conversion::StartLower,
            Some("startupper") => Conversion::StartUpper,
            Some("allupper") => Conversion::AllUpper,
            Some(x) => {
                return Err(Error::Unimplemented(format!(
                    "case conversion {} not supported.",
                    x
                )))
            }
            None => Conversion::Nop,
        },
        pos_replacer,
        regex_replacer,
    })
}

fn parse_synthesizer_text(text: &str, engine: &Engine) -> Result<Vec<SynthesizerPart>, Error> {
    lazy_static! {
        static ref MATCH_REGEX: Regex = Regex::new(r"\\(\d)".into());
    }

    let mut parts = Vec::new();
    let mut end_index = 0;

    for capture in MATCH_REGEX.captures_iter(&text) {
        let mat = capture.get(0).expect("0th regex group exists");

        if end_index != mat.start() {
            parts.push(SynthesizerPart::Text(
                (&text[end_index..mat.start()]).to_string(),
            ))
        }

        let id = capture
            .get(1)
            .expect("1st regex group exists")
            .as_str()
            .parse::<usize>()
            .expect("match regex capture must be parsable as usize.");

        parts.push(SynthesizerPart::Match(
            Match {
                id: engine.to_graph_id(id)?,
                conversion: Conversion::Nop,
                pos_replacer: None,
                regex_replacer: None,
            }
            .into(),
        ));
        end_index = mat.end();
    }

    if end_index < text.len() {
        parts.push(SynthesizerPart::Text((&text[end_index..]).to_string()))
    }
    Ok(parts)
}

fn parse_suggestion(
    data: super::Suggestion,
    engine: &Engine,
    info: &mut BuildInfo,
) -> Result<Synthesizer, Error> {
    let mut parts = Vec::new();
    for part in data.parts {
        match part {
            super::SuggestionPart::Text(text) => {
                parts.extend(parse_synthesizer_text(text.as_str(), engine)?);
            }
            super::SuggestionPart::Match(m) => {
                parts.push(SynthesizerPart::Match(parse_match(m, engine, info)?.into()));
            }
        }
    }

    Ok(Synthesizer {
        parts,
        // use titlecase adjustment (i. e. make replacement title case if match is title case) if token rule
        use_titlecase_adjust: matches!(engine, Engine::Token(_)),
    })
}

fn get_last_id(parts: &[Part]) -> isize {
    parts.iter().fold(1, |a, x| a + x.visible as isize)
}

fn parse_parallel_tokens(
    tokens: &[super::Token],
    case_sensitive: bool,
    info: &mut BuildInfo,
) -> Result<Vec<Atom>, Error> {
    tokens
        .iter()
        .map(|x| {
            let mut parsed = parse_token(x, case_sensitive, info)?;

            if parsed.len() != 1 || parsed[0].quantifier.min != 1 || parsed[0].quantifier.max != 1 {
                return Err(Error::Unimplemented(
                    "control flow in parallel tokens is not implemented.".into(),
                ));
            }

            Ok(parsed.remove(0).atom)
        })
        .collect()
}

fn parse_tokens(
    tokens: &[super::TokenCombination],
    case_sensitive: bool,
    info: &mut BuildInfo,
) -> Result<Vec<Part>, Error> {
    let mut out = Vec::new();

    for token_combination in tokens {
        out.extend(match token_combination {
            super::TokenCombination::Token(token) => parse_token(token, case_sensitive, info)?,
            super::TokenCombination::And(tokens) => {
                let atom =
                    AndAtom::and(parse_parallel_tokens(&tokens.tokens, case_sensitive, info)?);
                vec![Part {
                    atom,
                    quantifier: Quantifier::new(1, 1),
                    greedy: true,
                    visible: true,
                    unify: tokens.tokens[0].unify.as_ref().map(|x| x == "yes"),
                }]
            }
            super::TokenCombination::Or(tokens) => {
                let atom = OrAtom::or(parse_parallel_tokens(&tokens.tokens, case_sensitive, info)?);
                vec![Part {
                    atom,
                    quantifier: Quantifier::new(1, 1),
                    greedy: true,
                    visible: true,
                    unify: tokens.tokens[0].unify.as_ref().map(|x| x == "yes"),
                }]
            }
            super::TokenCombination::Feature(_) => Vec::new(),
        });
    }

    Ok(out)
}

fn parse_pattern(
    pattern: super::Pattern,
    info: &mut BuildInfo,
) -> Result<(Composition, usize, usize), Error> {
    let mut start = None;
    let mut end = None;

    let mut composition_parts = Vec::new();
    let case_sensitive = match &pattern.case_sensitive {
        Some(string) => string == "yes",
        None => false,
    };

    for part in &pattern.parts {
        match part {
            super::PatternPart::Token(token) => {
                composition_parts.extend(parse_token(token, case_sensitive, info)?)
            }
            super::PatternPart::Marker(marker) => {
                start = Some(get_last_id(&composition_parts));

                composition_parts.extend(parse_tokens(&marker.tokens, case_sensitive, info)?);

                end = Some(get_last_id(&composition_parts));
            }
            super::PatternPart::And(tokens) => {
                let atom =
                    AndAtom::and(parse_parallel_tokens(&tokens.tokens, case_sensitive, info)?);

                composition_parts.push(Part {
                    atom,
                    quantifier: Quantifier::new(1, 1),
                    greedy: true,
                    visible: true,
                    unify: tokens.tokens[0].unify.as_ref().map(|x| x == "yes"),
                });
            }
            super::PatternPart::Or(tokens) => {
                let atom = OrAtom::or(parse_parallel_tokens(&tokens.tokens, case_sensitive, info)?);

                composition_parts.push(Part {
                    atom,
                    quantifier: Quantifier::new(1, 1),
                    greedy: true,
                    visible: true,
                    unify: tokens.tokens[0].unify.as_ref().map(|x| x == "yes"),
                });
            }
            super::PatternPart::Feature(_) => {}
        }
    }

    let start = start.unwrap_or(1) as usize;
    let end = end.unwrap_or_else(|| get_last_id(&composition_parts)) as usize - 1;

    let composition = Composition::new(composition_parts)?;

    Ok((composition, start, end))
}

fn parse_features(
    pattern: &super::Pattern,
    unifications: &Option<Vec<super::Unification>>,
    info: &mut BuildInfo,
) -> Result<Vec<Vec<PosFilter>>, Error> {
    let mut filters = Vec::new();
    let mut parse_feature = |id: &str| -> Result<Vec<PosFilter>, Error> {
        let unification = unifications
            .as_ref()
            .unwrap()
            .iter()
            .find(|x| x.feature == id)
            .unwrap();

        unification
            .equivalences
            .iter()
            .map(|equiv| {
                parse_pos_filter(
                    &equiv.token.postag,
                    equiv.token.postag_regexp.as_deref(),
                    info,
                )
            })
            .collect()
    };

    for part in &pattern.parts {
        match part {
            super::PatternPart::Feature(feature) => filters.push(parse_feature(&feature.id)?),
            super::PatternPart::Marker(marker) => {
                for token_combination in &marker.tokens {
                    if let super::TokenCombination::Feature(feature) = token_combination {
                        filters.push(parse_feature(&feature.id)?);
                    }
                }
            }
            _ => {}
        }
    }

    Ok(filters)
}

impl Rule {
    pub fn from_rule_structure(data: super::Rule, info: &mut BuildInfo) -> Result<Rule, Error> {
        if data.filter.is_some() {
            return Err(Error::Unimplemented(
                "rules with filter are not implemented.".into(),
            ));
        }

        let (engine, start, end) = match (&data.pattern, data.regex) {
            (Some(_), Some(_)) => Err(Error::Unexpected(
                "must not contain both `pattern` and `regexp`.".into(),
            )),
            (None, None) => Err(Error::Unexpected(
                "either `pattern` or `regexp` must be supplied.".into(),
            )),
            (Some(pattern), None) => {
                let (composition, start, end) = parse_pattern(pattern.clone(), info)?;
                let antipatterns = if let Some(antipatterns) = data.antipatterns {
                    antipatterns
                        .into_iter()
                        .map(|pattern| parse_pattern(pattern, info).map(|x| x.0))
                        .collect::<Result<Vec<_>, Error>>()?
                } else {
                    Vec::new()
                };

                if antipatterns
                    .iter()
                    .any(|pattern| pattern.parts.iter().any(|x| x.unify.is_some()))
                {
                    return Err(Error::Unimplemented(
                        "`unify` in antipattern is not supported.".into(),
                    ));
                }

                Ok((
                    Engine::Token(TokenEngine {
                        composition,
                        antipatterns,
                    }),
                    start,
                    end,
                ))
            }
            (None, Some(regex)) => {
                let case_sensitive = match regex.case_sensitive.as_deref() {
                    Some("yes") => true,
                    None => false,
                    x => panic!("unknown case_sensitive value {:?}", x),
                };
                let mark = regex.mark.map_or(Ok(0), |x| x.parse())?;
                let regex = Regex::from_java_regex(&regex.text, false, case_sensitive)?;
                let id_to_idx: DefaultHashMap<GraphId, usize> = (0..regex.captures_len() + 1)
                    .enumerate()
                    // the IDs in a regex rule are just the same as indices
                    .map(|(key, value)| (GraphId(key), value))
                    .collect();
                Ok((Engine::Text(regex.into(), id_to_idx), mark, mark))
            }
        }?;

        let maybe_composition = if let Engine::Token(engine) = &engine {
            Some(&engine.composition)
        } else {
            None
        };

        let unify_data = if let Some(pattern) = &data.pattern {
            let unify_filters = parse_features(&pattern, &data.unifications, info)?;
            let unify_mask: Vec<_> = maybe_composition
                .unwrap()
                .parts
                .iter()
                .map(|part| part.unify)
                .collect();
            Some((unify_filters, unify_mask))
        } else {
            None
        };

        let mut message_parts = Vec::new();
        let mut suggesters = Vec::new();

        for part in data.message.parts {
            match part {
                super::MessagePart::Suggestion(suggestion) => {
                    let suggester = parse_suggestion(suggestion.clone(), &engine, info)?;
                    // simpler to just parse a second time than cloning the result
                    message_parts.extend(parse_suggestion(suggestion, &engine, info)?.parts);
                    suggesters.push(suggester);
                }
                super::MessagePart::Text(text) => {
                    message_parts.extend(parse_synthesizer_text(text.as_str(), &engine)?);
                }
                super::MessagePart::Match(m) => {
                    message_parts.push(SynthesizerPart::Match(
                        parse_match(m, &engine, info)?.into(),
                    ));
                }
            }
        }

        if let Some(suggestions) = data.suggestions {
            for suggestion in suggestions {
                suggesters.push(parse_suggestion(suggestion, &engine, info)?);
            }
        }

        if suggesters.is_empty() {
            return Err(Error::Unimplemented(
                "rules with no suggestion are not implemented.".into(),
            ));
        }

        assert!(!message_parts.is_empty(), "Rules must have a message.");

        let mut examples = Vec::new();
        for example in &data.examples {
            if example.kind.is_some() {
                return Err(Error::Unimplemented(
                    "examples with `type` (i. e. 'triggers_error') are not implemented.".into(),
                ));
            }

            let mut texts = Vec::new();
            let mut suggestion: Option<Suggestion> = None;

            for part in &example.parts {
                match part {
                    super::ExamplePart::Text(text) => {
                        texts.push(text.as_str());
                    }
                    super::ExamplePart::Marker(marker) => {
                        let (bytes_before, chars_before) =
                            texts.iter().fold((0, 0), |acc, text| {
                                (acc.0 + text.len(), acc.1 + text.chars().count())
                            });

                        if suggestion.is_some() {
                            return Err(Error::Unexpected(
                                "example must have one or zero markers".into(),
                            ));
                        }

                        texts.push(marker.text.as_str());

                        if let Some(correction_text) = &example.correction {
                            let mut replacements: Vec<_> =
                                correction_text.split('|').map(|x| x.to_string()).collect();

                            replacements = if chars_before == 0 {
                                // title case if at start
                                replacements
                                    .into_iter()
                                    .map(|x| {
                                        utils::apply_to_first(&x, |c| c.to_uppercase().collect())
                                    })
                                    .collect()
                            } else {
                                replacements
                            };

                            suggestion = Some(Suggestion::new(
                                "_Test".into(),
                                "_Test".into(),
                                Span::new(
                                    bytes_before..bytes_before + marker.text.len(),
                                    chars_before..chars_before + marker.text.chars().count(),
                                ),
                                replacements,
                            ));
                        }
                    }
                }
            }

            examples.push(Example {
                text: texts.join(""),
                suggestion,
            });
        }

        let unification = if let Some((unify_filters, unify_mask)) = unify_data {
            if unify_filters.is_empty() {
                None
            } else {
                Some(Unification {
                    filters: unify_filters,
                    mask: unify_mask,
                })
            }
        } else {
            None
        };

        Ok(Rule {
            start: engine.to_graph_id(start)?,
            end: engine.to_graph_id(end)?,
            engine,
            unification,
            examples,
            suggesters,
            message: Synthesizer {
                parts: message_parts,
                use_titlecase_adjust: true,
            },
            url: data.url.map(|x| x.to_string()),
            short: data.short.map(|x| x.to_string()),
            // fields below need information from rule group / category, so are set later
            id: Index::default(),
            name: String::new(),
            category_name: String::new(),
            category_type: None,
            enabled: true,
        })
    }
}

fn parse_tag_form(
    form: &str,
    is_sentence_end: bool,
    info: &mut BuildInfo,
) -> Result<Tags<'static>, Error> {
    let tagger = info.tagger();

    lazy_static! {
        static ref REGEX: Regex = Regex::new(r"(.+?)\[(.+?)\]".into());
    }

    let captures = REGEX
        .captures(form)
        .ok_or_else(|| Error::Unexpected(format!("tag form must match regex, found '{}'", form)))?;
    // text can never be changed by disambiguation so we don't need to compare it
    let text = captures.get(1).expect("1st regex group exists").as_str();
    let tags = captures.get(2).expect("2nd regex group exists").as_str();

    let mut tags: Vec<_> = tags
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
                Some(WordData::new(
                    tagger.id_word(parts[0].to_owned().into()),
                    tagger.id_tag(parts[1]).into_static(),
                ))
            }
        })
        .collect();

    tags.push(
        WordData::new(
            tagger.id_word(text.to_owned().into()),
            PosId::special(SpecialPos::None),
        )
        .freeze(),
    );

    if is_sentence_end {
        tags.push(WordData::new(WordId::empty(), PosId::special(SpecialPos::SentEnd)).freeze());
    }

    Ok(Tags::new(WordId::empty(), tags))
}

impl WordData<'static> {
    fn from_structure(data: super::WordData, info: &mut BuildInfo) -> Result<Self, Error> {
        Ok(WordData::new(
            info.tagger()
                .id_word(data.lemma.unwrap_or_else(String::new).into()),
            info.tagger()
                .id_tag(data.pos.as_deref().unwrap_or("").trim())
                .into_static(),
        ))
    }
}

fn parse_pos_filter(
    postag: &str,
    postag_regexp: Option<&str>,
    info: &mut BuildInfo,
) -> Result<PosFilter, Error> {
    Ok(match postag_regexp.as_deref() {
        Some("yes") => PosFilter::new(PosMatcher::new(
            Matcher::new_regex(
                Regex::from_java_regex(&postag, true, true).unwrap(),
                false,
                true,
            ),
            info,
        )?),
        Some(_) | None => PosFilter::new(PosMatcher::new(
            Matcher::new_string(either::Left(postag.into()), false, false, true),
            info,
        )?),
    })
}

impl DisambiguationRule {
    pub fn from_rule_structure(
        data: super::DisambiguationRule,
        info: &mut BuildInfo,
    ) -> Result<DisambiguationRule, Error> {
        // might need the pattern later so clone it here
        let (composition, start, end) = parse_pattern(data.pattern.clone(), info)?;

        let unify_filters = parse_features(&data.pattern, &data.unifications, info)?;
        let unify_mask: Vec<_> = composition.parts.iter().map(|part| part.unify).collect();

        let antipatterns = if let Some(antipatterns) = data.antipatterns {
            antipatterns
                .into_iter()
                .map(|pattern| parse_pattern(pattern, info).map(|x| x.0))
                .collect::<Result<Vec<_>, Error>>()?
        } else {
            Vec::new()
        };

        if antipatterns
            .iter()
            .any(|pattern| pattern.parts.iter().any(|x| x.unify.is_some()))
        {
            return Err(Error::Unimplemented(
                "`unify` in antipattern is not supported.".into(),
            ));
        }

        let engine = Engine::Token(TokenEngine {
            composition,
            antipatterns,
        });

        let word_datas: Vec<_> = if let Some(wds) = data.disambig.word_datas {
            wds.into_iter()
                .map(|part| match part {
                    super::DisambiguationPart::WordData(x) => {
                        WordData::from_structure(x, info).map(either::Left)
                    }
                    super::DisambiguationPart::Match(x) => {
                        parse_pos_filter(&x.postag.unwrap(), x.postag_regexp.as_deref(), info)
                            .map(either::Right)
                    }
                })
                .collect::<Result<_, Error>>()?
        } else {
            Vec::new()
        };

        let tagger = info.tagger();
        let disambiguations = match data.disambig.action.as_deref() {
            Some("remove") => {
                if let Some(postag) = data.disambig.postag.as_ref() {
                    Ok(Disambiguation::Remove(vec![either::Right(
                        parse_pos_filter(postag, Some("yes"), info)?,
                    )]))
                } else {
                    Ok(Disambiguation::Remove(word_datas.into_iter().collect()))
                }
            }
            Some("add") => {
                if data.disambig.postag.is_some() {
                    return Err(Error::Unimplemented(
                        "postag not supported for `add`.".into(),
                    ));
                }

                Ok(Disambiguation::Add(
                    word_datas
                        .into_iter()
                        .map(|x| x.left().expect("match not supported for `add`"))
                        .collect(),
                ))
            }
            Some("replace") => Ok(Disambiguation::Replace(
                word_datas
                    .into_iter()
                    .map(|x| {
                        x.left()
                            .expect("match not supported for `replace` disambiguation")
                    })
                    .collect(),
            )),
            Some("ignore_spelling") => Ok(Disambiguation::Nop), // ignore_spelling can be ignored since we dont check spelling
            Some("immunize") => Ok(Disambiguation::Nop), // immunize can probably not be ignored
            Some("filterall") => {
                let mut disambig = Vec::new();
                let mut marker_disambig = Vec::new();
                let mut has_marker = false;

                for part in &data.pattern.parts {
                    match part {
                        super::PatternPart::Marker(marker) => {
                            has_marker = true;
                            for token in &marker.tokens {
                                let token = match token {
                                    super::TokenCombination::Token(token) => token,
                                    super::TokenCombination::And(tokens)
                                    | super::TokenCombination::Or(tokens) => &tokens.tokens[0],
                                    super::TokenCombination::Feature(_) => continue,
                                };

                                marker_disambig.push(
                                    token
                                        .postag
                                        .as_ref()
                                        .map(|x| {
                                            parse_pos_filter(
                                                x,
                                                token.postag_regexp.as_deref(),
                                                info,
                                            )
                                            .map(either::Right)
                                        })
                                        .transpose()?,
                                );
                            }
                        }
                        super::PatternPart::Token(token) => disambig.push(
                            token
                                .postag
                                .as_ref()
                                .map(|x| {
                                    parse_pos_filter(x, token.postag_regexp.as_deref(), info)
                                        .map(either::Right)
                                })
                                .transpose()?,
                        ),
                        super::PatternPart::And(tokens) | super::PatternPart::Or(tokens) => {
                            disambig.push(
                                tokens.tokens[0]
                                    .postag
                                    .as_ref()
                                    .map(|x| {
                                        parse_pos_filter(
                                            x,
                                            tokens.tokens[0].postag_regexp.as_deref(),
                                            info,
                                        )
                                        .map(either::Right)
                                    })
                                    .transpose()?,
                            )
                        }
                        super::PatternPart::Feature(_) => {}
                    }
                }

                let disambiguations = if has_marker {
                    marker_disambig
                } else {
                    disambig
                };

                Ok(Disambiguation::Filter(
                    disambiguations.into_iter().collect(),
                    tagger.lang_options().retain_last,
                ))
            }
            Some("filter") => {
                if let Some(postag) = data.disambig.postag.as_ref() {
                    Ok(Disambiguation::Filter(
                        vec![Some(either::Right(parse_pos_filter(
                            postag,
                            Some("yes"),
                            info,
                        )?))],
                        tagger.lang_options().retain_last,
                    ))
                } else {
                    Ok(Disambiguation::Filter(
                        word_datas.into_iter().map(Some).collect(),
                        tagger.lang_options().retain_last,
                    ))
                }
            }
            Some("unify") => {
                let mut mask = Vec::new();
                let mut marker_mask = Vec::new();

                let mut disambig = Vec::new();
                let mut marker_disambig = Vec::new();
                let mut has_marker = false;

                for part in &data.pattern.parts {
                    match part {
                        super::PatternPart::Marker(marker) => {
                            has_marker = true;
                            for token in &marker.tokens {
                                let token = match token {
                                    super::TokenCombination::Token(token) => token,
                                    super::TokenCombination::And(tokens)
                                    | super::TokenCombination::Or(tokens) => &tokens.tokens[0],
                                    super::TokenCombination::Feature(_) => continue,
                                };

                                marker_disambig.push(
                                    token
                                        .postag
                                        .as_ref()
                                        .map(|x| {
                                            parse_pos_filter(
                                                x,
                                                token.postag_regexp.as_deref(),
                                                info,
                                            )
                                        })
                                        .transpose()?,
                                );
                                marker_mask.push(token.unify.is_some())
                            }
                        }
                        super::PatternPart::Token(token) => {
                            disambig.push(
                                token
                                    .postag
                                    .as_ref()
                                    .map(|x| {
                                        parse_pos_filter(x, token.postag_regexp.as_deref(), info)
                                    })
                                    .transpose()?,
                            );
                            mask.push(token.unify.is_some());
                        }
                        super::PatternPart::And(tokens) | super::PatternPart::Or(tokens) => {
                            disambig.push(
                                tokens.tokens[0]
                                    .postag
                                    .as_ref()
                                    .map(|x| {
                                        parse_pos_filter(
                                            x,
                                            tokens.tokens[0].postag_regexp.as_deref(),
                                            info,
                                        )
                                    })
                                    .transpose()?,
                            );
                            mask.push(tokens.tokens[0].unify.is_some());
                        }
                        super::PatternPart::Feature(_) => {}
                    }
                }

                let (disambig, mask) = if has_marker {
                    (marker_disambig, marker_mask)
                } else {
                    (disambig, mask)
                };

                Ok(Disambiguation::Unify(unify_filters.clone(), disambig, mask))
            }
            None => {
                if let Some(postag) = data.disambig.postag.as_ref() {
                    Ok(Disambiguation::Filter(
                        vec![Some(either::Left(WordData::new(
                            tagger.id_word("".into()),
                            tagger.id_tag(postag).into_static(),
                        )))],
                        tagger.lang_options().retain_last,
                    ))
                } else {
                    Ok(Disambiguation::Filter(
                        word_datas.into_iter().map(Some).collect(),
                        tagger.lang_options().retain_last,
                    ))
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

            Some(super::impls::filters::get_filter(
                filter_data.class.split('.').next_back().unwrap(),
                args,
                &engine,
            )?)
        } else {
            None
        };

        let mut examples = Vec::new();

        if let Some(examples_structure) = data.examples.as_ref() {
            for example in examples_structure {
                let mut texts = Vec::new();
                let mut char_span: Option<Range<usize>> = None;
                let mut char_length = 0;

                for part in &example.parts {
                    match part {
                        super::ExamplePart::Text(text) => {
                            texts.push(text.as_str());
                            char_length += text.chars().count();
                        }
                        super::ExamplePart::Marker(marker) => {
                            if char_span.is_some() {
                                return Err(Error::Unexpected(
                                    "example must have one or zero markers".into(),
                                ));
                            }

                            texts.push(marker.text.as_str());
                            let length = marker.text.chars().count();

                            char_span = Some(char_length..char_length + length);
                            char_length += marker.text.chars().count();
                        }
                    }
                }

                let text = texts.join("");

                let test = match example.kind.as_str() {
                    "untouched" => DisambiguationExample::Unchanged(text),
                    "ambiguous" => {
                        let char_span = char_span.expect("must have marker when ambiguous example");
                        // `is_sentence_end` computation is problematic if there is whitespace at the end
                        // this is not the case in current LT files, and also not clear what LT would to in that case
                        let is_sentence_end = char_span.end == text.chars().count();

                        DisambiguationExample::Changed(DisambiguationChange {
                            text,
                            before: parse_tag_form(
                                example
                                    .inputform
                                    .as_ref()
                                    .expect("must have inputform when ambiguous example"),
                                is_sentence_end,
                                info,
                            )?,
                            after: parse_tag_form(
                                &example
                                    .outputform
                                    .as_ref()
                                    .expect("must have inputform when ambiguous example"),
                                is_sentence_end,
                                info,
                            )?,
                            char_span,
                        })
                    }
                    x => panic!("unknown disambiguation example type {}", x),
                };

                examples.push(test);
            }
        }

        Ok(DisambiguationRule {
            start: engine.to_graph_id(start)?,
            end: engine.to_graph_id(end)?,
            engine,
            unification: if unify_filters.is_empty() {
                None
            } else {
                Some(Unification {
                    filters: unify_filters,
                    mask: unify_mask,
                })
            },
            filter,
            disambiguations,
            examples,
            id: Index::default(),
        })
    }
}

macro_rules! flatten_group {
    ($rulegroup:expr, $category:expr) => {{
        let group_antipatterns = if let Some(antipatterns) = $rulegroup.antipatterns {
            antipatterns
        } else {
            Vec::new()
        };

        let group = super::Group {
            id: $rulegroup.id,
            default: $rulegroup.default,
            name: $rulegroup.name,
            n: 0,
        };

        $rulegroup
            .rules
            .into_iter()
            .enumerate()
            .map(|(i, mut rule)| {
                if let Some(antipatterns) = &mut rule.antipatterns {
                    antipatterns.extend(group_antipatterns.clone());
                } else {
                    rule.antipatterns = Some(group_antipatterns.clone());
                }

                let mut group = group.clone();
                group.n = i;
                (rule, Some(group), $category.clone())
            })
            .collect::<Vec<_>>()
    }};
}

type GrammarRuleReading = (super::Rule, Option<super::Group>, Option<super::Category>);
type DisambiguationRuleReading = (
    super::DisambiguationRule,
    Option<super::Group>,
    Option<super::Category>,
);

pub fn read_rules<P: AsRef<std::path::Path>>(
    path: P,
) -> Vec<Result<GrammarRuleReading, serde_xml_rs::Error>> {
    let file = File::open(path.as_ref()).unwrap();
    let file = BufReader::new(file);

    let sanitized = super::preprocess::sanitize(file, &["suggestion"]);
    let rules = super::preprocess::extract_rules(sanitized.as_bytes());

    let mut unifications = Vec::new();

    let rules: Vec<_> = rules
        .into_iter()
        .map(|(xml, category)| {
            let mut out = Vec::new();

            let deseralized = super::RuleContainer::deserialize(
                &mut serde_xml_rs::Deserializer::new(EventReader::new(xml.as_bytes())),
            );

            out.extend(match deseralized {
                Ok(rule_container) => match rule_container {
                    super::RuleContainer::Rule(rule) => {
                        vec![Ok((rule, None, category))]
                    }
                    super::RuleContainer::RuleGroup(rule_group) => {
                        flatten_group!(rule_group, category)
                            .into_iter()
                            .map(Ok)
                            .collect()
                    }
                    super::RuleContainer::Unification(unification) => {
                        unifications.push(unification);

                        vec![]
                    }
                },
                Err(err) => vec![Err(err)],
            });
            out
        })
        .flatten()
        .collect();

    rules
        .into_iter()
        .map(|result| match result {
            Ok(mut x) => {
                x.0.unifications = Some(unifications.clone());

                Ok(x)
            }
            Err(x) => Err(x),
        })
        .collect()
}

pub fn read_disambiguation_rules<P: AsRef<std::path::Path>>(
    path: P,
) -> Vec<Result<DisambiguationRuleReading, serde_xml_rs::Error>> {
    let file = File::open(path.as_ref()).unwrap();
    let file = BufReader::new(file);

    let sanitized = super::preprocess::sanitize(file, &[]);
    let rules = super::preprocess::extract_rules(sanitized.as_bytes());

    let mut unifications = Vec::new();

    let rules: Vec<_> = rules
        .into_iter()
        .map(|(xml, _)| {
            let mut out = Vec::new();

            let deseralized = super::DisambiguationRuleContainer::deserialize(
                &mut serde_xml_rs::Deserializer::new(EventReader::new(xml.as_bytes())),
            );

            let category: Option<Category> = None;

            out.extend(match deseralized {
                Ok(rule_container) => match rule_container {
                    super::DisambiguationRuleContainer::Rule(rule) => {
                        vec![Ok((rule, None, category))]
                    }
                    super::DisambiguationRuleContainer::RuleGroup(rule_group) => {
                        flatten_group!(rule_group, category)
                            .into_iter()
                            .map(Ok)
                            .collect()
                    }
                    super::DisambiguationRuleContainer::Unification(unification) => {
                        unifications.push(unification);

                        vec![]
                    }
                },
                Err(err) => vec![Err(err)],
            });
            out
        })
        .flatten()
        .collect();

    rules
        .into_iter()
        .map(|result| match result {
            Ok(mut x) => {
                x.0.unifications = Some(unifications.clone());

                Ok(x)
            }
            Err(x) => Err(x),
        })
        .collect()
}
