use std::sync::Arc;

use super::structure;
use crate::{filter::get_filter, utils, utils::regex::SerializeRegex, Error};
use crate::{tokenizer::tag::Tagger, types::*};
use lazy_static::lazy_static;
use onig::Regex;
use serde::{Deserialize, Serialize};

pub use structure::{read_disambiguation_rules, read_rules};

use crate::rule::disambiguation::*;
use crate::rule::engine::composition::concrete::*;
use crate::rule::engine::composition::*;
use crate::rule::engine::*;
use crate::rule::grammar::*;
use crate::rule::{DisambiguationRule, Rule, Unification};

// TODO: should be an option in config OR restricted to one sentence
fn max_matches() -> usize {
    20
}

#[derive(Serialize, Deserialize, Debug)]
pub struct RegexCache {
    cache: DefaultHashMap<u64, Option<DefaultHashSet<u32>>>,
    // this is compared with the hash of the word store of the tagger
    word_hash: u64,
}

impl RegexCache {
    pub fn new(word_hash: u64) -> Self {
        RegexCache {
            cache: DefaultHashMap::default(),
            word_hash,
        }
    }

    pub fn word_hash(&self) -> &u64 {
        &self.word_hash
    }

    pub fn set_word_hash(&self) -> &u64 {
        &self.word_hash
    }

    pub fn get(&self, key: &u64) -> Option<&Option<DefaultHashSet<u32>>> {
        self.cache.get(key)
    }

    pub fn insert(&mut self, key: u64, value: Option<DefaultHashSet<u32>>) {
        self.cache.insert(key, value);
    }
}

pub struct BuildInfo {
    tagger: Arc<Tagger>,
    regex_cache: RegexCache,
}

impl BuildInfo {
    pub fn new(tagger: Arc<Tagger>, regex_cache: RegexCache) -> Self {
        BuildInfo {
            tagger,
            regex_cache,
        }
    }

    pub fn tagger(&self) -> &Arc<Tagger> {
        &self.tagger
    }

    pub fn mut_regex_cache(&mut self) -> &mut RegexCache {
        &mut self.regex_cache
    }
}

fn parse_match_attribs(
    attribs: impl structure::MatchAttributes,
    text: Option<&str>,
    case_sensitive: bool,
    text_match_idx: Option<usize>,
    info: &mut BuildInfo,
) -> Result<Atom, Error> {
    let mut atoms: Vec<Atom> = Vec::new();

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

    if text.is_some() || text_match_idx.is_some() {
        let matcher = if is_regex && text_match_idx.is_none() {
            let regex = SerializeRegex::new(text.unwrap().trim(), true, case_sensitive);
            Matcher::new_regex(regex?, negate, inflected)
        } else {
            Matcher::new_string(
                text_match_idx.map_or_else(
                    || either::Left(text.unwrap().trim().to_string()),
                    either::Right,
                ),
                negate,
                case_sensitive,
                inflected,
            )
        };

        if inflected {
            inflect_matcher = Some(matcher);
        } else {
            atoms.push(
                (TextAtom {
                    matcher: TextMatcher::new(matcher, info),
                })
                .into(),
            );
        }
    }

    if let Some(postag) = attribs.postag() {
        let raw_matcher = if is_postag_regexp {
            let regex = SerializeRegex::new(&postag.trim(), true, true);
            Matcher::new_regex(regex?, negate_pos, true)
        } else {
            Matcher::new_string(
                either::Left(postag.trim().to_string()),
                negate_pos,
                true,
                true,
            )
        };
        pos_matcher = Some(PosMatcher::new(raw_matcher, info));
    }

    if pos_matcher.is_some() || inflect_matcher.is_some() {
        let matcher = WordDataMatcher {
            pos_matcher,
            inflect_matcher: inflect_matcher.map(|x| TextMatcher::new(x, info)),
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
            let regex = SerializeRegex::new(chunk_re.trim(), true, true)?;
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
    token: &structure::Token,
    case_sensitive: bool,
    only_shifted: bool,
    info: &mut BuildInfo,
) -> Result<Atom, Error> {
    if let Some(parts) = &token.parts {
        let exceptions: Vec<Atom> = parts
            .iter()
            .filter_map(|x| match x {
                structure::TokenPart::Exception(x) => Some(x),
                _ => None,
            })
            .filter_map(|x| {
                let exception_text = if let Some(exception_text) = &x.text {
                    Some(exception_text.as_str())
                } else {
                    None
                };
                let mut atom =
                    parse_match_attribs(x, exception_text, case_sensitive, None, info).unwrap();

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
                    Some(atom)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        Ok(NotAtom::not(OrAtom::or(exceptions)))
    } else {
        Ok((TrueAtom {}).into())
    }
}

fn parse_token(
    token: &structure::Token,
    case_sensitive: bool,
    info: &mut BuildInfo,
) -> Result<Vec<Part>, Error> {
    let mut parts = Vec::new();
    let text = if let Some(parts) = &token.parts {
        parts.iter().find_map(|x| match x {
            structure::TokenPart::Text(text) => Some(text.as_str()),
            _ => None,
        })
    } else {
        None
    };

    let text_match_idx = if let Some(parts) = &token.parts {
        parts.iter().find_map(|x| match x {
            structure::TokenPart::Sub(sub) => Some(sub.no.parse::<usize>().unwrap() + 1),
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

fn parse_match(
    m: structure::Match,
    composition: &Option<&Composition>,
    info: &mut BuildInfo,
) -> Result<Match, Error> {
    if m.postag.is_some()
        || m.postag_regex.is_some()
        || m.postag_replace.is_some()
        || m.text.is_some()
    {
        return Err(Error::Unimplemented(
            "postag, postag_regex, postag_replace and text in `match` are not implemented.".into(),
        ));
    }

    if m.include_skipped.is_some() {
        return Err(Error::Unimplemented(
            "include_skipped in `match` is not implemented.".into(),
        ));
    }

    let mut id =
        m.no.parse::<usize>()
            .expect("no must be parsable as usize.");

    if let Some(composition) = composition {
        let last_id = get_last_id(&composition.parts) as usize - 1;

        if id > last_id {
            id = last_id;
        }
    }

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
                let regex = SerializeRegex::new(&postag, true, false)?;
                Matcher::new_regex(regex, false, true)
            }
            None => Matcher::new_string(either::Left(postag), false, false, true),
            x => panic!("unknown postag_regex value {:?}", x),
        };
        Some(PosReplacer {
            matcher: PosMatcher::new(matcher, info),
        })
    } else {
        None
    };

    let regex_replacer = match (m.regexp_match, m.regexp_replace) {
        (Some(regex_match), Some(regex_replace)) => Some((
            SerializeRegex::new(&regex_match, false, true)?,
            regex_replace,
        )),
        _ => None,
    };

    Ok(Match {
        id,
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

fn parse_synthesizer_text(text: &str) -> Vec<SynthesizerPart> {
    lazy_static! {
        static ref MATCH_REGEX: Regex = Regex::new(r"\\(\d)").unwrap();
    }

    let mut parts = Vec::new();
    let mut end_index = 0;

    for capture in MATCH_REGEX.captures_iter(&text) {
        let (start, end) = capture.pos(0).unwrap();

        if end_index != start {
            parts.push(SynthesizerPart::Text((&text[end_index..start]).to_string()))
        }

        let index = capture
            .at(1)
            .unwrap()
            .parse::<usize>()
            .expect("match regex capture must be parsable as usize.");

        parts.push(SynthesizerPart::Match(Match {
            id: index,
            conversion: Conversion::Nop,
            pos_replacer: None,
            regex_replacer: None,
        }));
        end_index = end;
    }

    if end_index < text.len() {
        parts.push(SynthesizerPart::Text((&text[end_index..]).to_string()))
    }
    parts
}

fn parse_suggestion(
    data: structure::Suggestion,
    composition: &Option<&Composition>,
    info: &mut BuildInfo,
) -> Result<Synthesizer, Error> {
    let mut parts = Vec::new();
    for part in data.parts {
        match part {
            structure::SuggestionPart::Text(text) => {
                parts.extend(parse_synthesizer_text(text.as_str()));
            }
            structure::SuggestionPart::Match(m) => {
                parts.push(SynthesizerPart::Match(parse_match(m, composition, info)?));
            }
        }
    }

    Ok(Synthesizer {
        parts,
        // use titlecase adjustment (i. e. make replacement title case if match is title case) if token rule
        use_titlecase_adjust: composition.is_some(),
    })
}

fn get_last_id(parts: &[Part]) -> isize {
    parts.iter().fold(1, |a, x| a + x.visible as isize)
}

fn parse_parallel_tokens(
    tokens: &[structure::Token],
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
    tokens: &[structure::TokenCombination],
    case_sensitive: bool,
    info: &mut BuildInfo,
) -> Result<Vec<Part>, Error> {
    let mut out = Vec::new();

    for token_combination in tokens {
        out.extend(match token_combination {
            structure::TokenCombination::Token(token) => parse_token(token, case_sensitive, info)?,
            structure::TokenCombination::And(tokens) => {
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
            structure::TokenCombination::Or(tokens) => {
                let atom = OrAtom::or(parse_parallel_tokens(&tokens.tokens, case_sensitive, info)?);
                vec![Part {
                    atom,
                    quantifier: Quantifier::new(1, 1),
                    greedy: true,
                    visible: true,
                    unify: tokens.tokens[0].unify.as_ref().map(|x| x == "yes"),
                }]
            }
            structure::TokenCombination::Feature(_) => Vec::new(),
        });
    }

    Ok(out)
}

fn parse_pattern(
    pattern: structure::Pattern,
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
            structure::PatternPart::Token(token) => {
                composition_parts.extend(parse_token(token, case_sensitive, info)?)
            }
            structure::PatternPart::Marker(marker) => {
                start = Some(get_last_id(&composition_parts));

                composition_parts.extend(parse_tokens(&marker.tokens, case_sensitive, info)?);

                end = Some(get_last_id(&composition_parts));
            }
            structure::PatternPart::And(tokens) => {
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
            structure::PatternPart::Or(tokens) => {
                let atom = OrAtom::or(parse_parallel_tokens(&tokens.tokens, case_sensitive, info)?);

                composition_parts.push(Part {
                    atom,
                    quantifier: Quantifier::new(1, 1),
                    greedy: true,
                    visible: true,
                    unify: tokens.tokens[0].unify.as_ref().map(|x| x == "yes"),
                });
            }
            structure::PatternPart::Feature(_) => {}
        }
    }

    let start = start.unwrap_or(1) as usize;
    let end = end.unwrap_or_else(|| get_last_id(&composition_parts)) as usize;

    let composition = Composition::new(composition_parts);

    Ok((composition, start, end))
}

fn parse_features(
    pattern: &structure::Pattern,
    unifications: &Option<Vec<structure::Unification>>,
    info: &mut BuildInfo,
) -> Vec<Vec<POSFilter>> {
    let mut filters = Vec::new();
    let mut parse_feature = |id: &str| -> Vec<POSFilter> {
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
            structure::PatternPart::Feature(feature) => filters.push(parse_feature(&feature.id)),
            structure::PatternPart::Marker(marker) => {
                for token_combination in &marker.tokens {
                    if let structure::TokenCombination::Feature(feature) = token_combination {
                        filters.push(parse_feature(&feature.id));
                    }
                }
            }
            _ => {}
        }
    }

    filters
}

impl Rule {
    pub fn from_rule_structure(data: structure::Rule, info: &mut BuildInfo) -> Result<Rule, Error> {
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
                let mark = regex.mark.map_or(0, |x| x.parse().unwrap());
                let regex = SerializeRegex::new(&regex.text, false, case_sensitive)?;
                let id_to_idx: DefaultHashMap<usize, usize> =
                    (0..regex.captures_len() + 1).enumerate().collect();
                Ok((Engine::Text(regex, id_to_idx), mark, mark + 1))
            }
        }?;

        let maybe_composition = if let Engine::Token(engine) = &engine {
            Some(&engine.composition)
        } else {
            None
        };

        let unify_data = if let Some(pattern) = &data.pattern {
            let unify_filters = parse_features(&pattern, &data.unifications, info);
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
                structure::MessagePart::Suggestion(suggestion) => {
                    let suggester = parse_suggestion(suggestion.clone(), &maybe_composition, info)?;
                    // simpler to just parse a second time than cloning the result
                    message_parts
                        .extend(parse_suggestion(suggestion, &maybe_composition, info)?.parts);
                    suggesters.push(suggester);
                }
                structure::MessagePart::Text(text) => {
                    message_parts.extend(parse_synthesizer_text(text.as_str()));
                }
                structure::MessagePart::Match(m) => {
                    message_parts.push(SynthesizerPart::Match(parse_match(
                        m,
                        &maybe_composition,
                        info,
                    )?));
                }
            }
        }

        if let Some(suggestions) = data.suggestions {
            for suggestion in suggestions {
                suggesters.push(parse_suggestion(suggestion, &maybe_composition, info)?);
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
            let mut char_length = 0;
            let mut suggestion: Option<Suggestion> = None;

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
                            let mut replacements: Vec<_> =
                                correction_text.split('|').map(|x| x.to_string()).collect();

                            replacements = if char_length == 0 {
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

                            suggestion = Some(Suggestion {
                                source: "_Test".to_string(),
                                message: "_Test".to_string(),
                                start: char_length,
                                end: char_length + length,
                                replacements,
                            });
                        }

                        char_length += marker.text.chars().count();
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
            engine,
            unification,
            examples,
            start,
            end,
            suggesters,
            message: Synthesizer {
                parts: message_parts,
                use_titlecase_adjust: true,
            },
            url: data.url.map(|x| x.to_string()),
            short: data.short.map(|x| x.to_string()),
            // attributes below need information from rule group / category, so are set later
            id: String::new(),
            name: String::new(),
            on: true,
            category_id: String::new(),
            category_name: String::new(),
            category_type: None,
        })
    }
}

fn parse_tag_form(form: &str, info: &mut BuildInfo) -> owned::Word {
    lazy_static! {
        static ref REGEX: Regex = Regex::new(r"(.+?)\[(.+?)\]").unwrap();
    }

    let captures = REGEX.captures(form).unwrap();
    let text = captures.at(1).unwrap().to_string();
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
                Some(owned::WordData::new(
                    info.tagger.id_word(parts[0].into()).to_owned_id(),
                    info.tagger.id_tag(parts[1]).to_owned_id(),
                ))
            }
        })
        .collect();

    owned::Word {
        text: info.tagger.id_word(text.into()).to_owned_id(),
        tags,
    }
}

impl owned::WordData {
    fn from_structure(data: structure::WordData, info: &mut BuildInfo) -> Self {
        owned::WordData::new(
            info.tagger
                .id_word(data.lemma.unwrap_or_else(String::new).into())
                .to_owned_id(),
            info.tagger
                .id_tag(data.pos.as_ref().map_or("", |x| x.as_str().trim()))
                .to_owned_id(),
        )
    }
}

fn parse_pos_filter(postag: &str, postag_regexp: Option<&str>, info: &mut BuildInfo) -> POSFilter {
    match postag_regexp.as_deref() {
        Some("yes") => POSFilter::new(PosMatcher::new(
            Matcher::new_regex(
                SerializeRegex::new(&postag, true, true).unwrap(),
                false,
                true,
            ),
            info,
        )),
        Some(_) | None => POSFilter::new(PosMatcher::new(
            Matcher::new_string(either::Left(postag.into()), false, false, true),
            info,
        )),
    }
}

impl DisambiguationRule {
    pub fn from_rule_structure(
        data: structure::DisambiguationRule,
        info: &mut BuildInfo,
    ) -> Result<DisambiguationRule, Error> {
        // might need the pattern later so clone it here
        let (composition, start, end) = parse_pattern(data.pattern.clone(), info)?;

        let unify_filters = parse_features(&data.pattern, &data.unifications, info);
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

        let word_datas: Vec<_> = if let Some(wds) = data.disambig.word_datas {
            wds.into_iter()
                .map(|part| match part {
                    structure::DisambiguationPart::WordData(x) => {
                        either::Left(owned::WordData::from_structure(x, info))
                    }
                    structure::DisambiguationPart::Match(x) => either::Right(parse_pos_filter(
                        &x.postag.unwrap(),
                        x.postag_regexp.as_deref(),
                        info,
                    )),
                })
                .collect()
        } else {
            Vec::new()
        };

        let disambiguations = match data.disambig.action.as_deref() {
            Some("remove") => {
                if let Some(postag) = data.disambig.postag.as_ref() {
                    Ok(Disambiguation::Remove(vec![either::Right(
                        parse_pos_filter(postag, Some("yes"), info),
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
                        structure::PatternPart::Marker(marker) => {
                            has_marker = true;
                            for token in &marker.tokens {
                                let token = match token {
                                    structure::TokenCombination::Token(token) => token,
                                    structure::TokenCombination::And(tokens)
                                    | structure::TokenCombination::Or(tokens) => &tokens.tokens[0],
                                    structure::TokenCombination::Feature(_) => continue,
                                };

                                marker_disambig.push(token.postag.as_ref().map(|x| {
                                    either::Right(parse_pos_filter(
                                        x,
                                        token.postag_regexp.as_deref(),
                                        info,
                                    ))
                                }));
                            }
                        }
                        structure::PatternPart::Token(token) => {
                            disambig.push(token.postag.as_ref().map(|x| {
                                either::Right(parse_pos_filter(
                                    x,
                                    token.postag_regexp.as_deref(),
                                    info,
                                ))
                            }))
                        }
                        structure::PatternPart::And(tokens)
                        | structure::PatternPart::Or(tokens) => {
                            disambig.push(tokens.tokens[0].postag.as_ref().map(|x| {
                                either::Right(parse_pos_filter(
                                    x,
                                    tokens.tokens[0].postag_regexp.as_deref(),
                                    info,
                                ))
                            }))
                        }
                        structure::PatternPart::Feature(_) => {}
                    }
                }

                let disambiguations = if has_marker {
                    marker_disambig
                } else {
                    disambig
                };

                Ok(Disambiguation::Filter(
                    disambiguations.into_iter().collect(),
                ))
            }
            Some("filter") => {
                if let Some(postag) = data.disambig.postag.as_ref() {
                    Ok(Disambiguation::Filter(vec![Some(either::Right(
                        parse_pos_filter(postag, Some("yes"), info),
                    ))]))
                } else {
                    Ok(Disambiguation::Filter(
                        word_datas.into_iter().map(Some).collect(),
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
                        structure::PatternPart::Marker(marker) => {
                            has_marker = true;
                            for token in &marker.tokens {
                                let token = match token {
                                    structure::TokenCombination::Token(token) => token,
                                    structure::TokenCombination::And(tokens)
                                    | structure::TokenCombination::Or(tokens) => &tokens.tokens[0],
                                    structure::TokenCombination::Feature(_) => continue,
                                };

                                marker_disambig.push(token.postag.as_ref().map(|x| {
                                    parse_pos_filter(x, token.postag_regexp.as_deref(), info)
                                }));
                                marker_mask.push(token.unify.is_some())
                            }
                        }
                        structure::PatternPart::Token(token) => {
                            disambig.push(token.postag.as_ref().map(|x| {
                                parse_pos_filter(x, token.postag_regexp.as_deref(), info)
                            }));
                            mask.push(token.unify.is_some());
                        }
                        structure::PatternPart::And(tokens)
                        | structure::PatternPart::Or(tokens) => {
                            disambig.push(tokens.tokens[0].postag.as_ref().map(|x| {
                                parse_pos_filter(x, tokens.tokens[0].postag_regexp.as_deref(), info)
                            }));
                            mask.push(tokens.tokens[0].unify.is_some());
                        }
                        structure::PatternPart::Feature(_) => {}
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
                    Ok(Disambiguation::Filter(vec![Some(either::Left(
                        owned::WordData::new(
                            info.tagger.id_word("".into()).to_owned_id(),
                            info.tagger.id_tag(postag).to_owned_id(),
                        ),
                    ))]))
                } else {
                    Ok(Disambiguation::Filter(
                        word_datas.into_iter().map(Some).collect(),
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

            Some(get_filter(
                filter_data.class.split('.').next_back().unwrap(),
                args,
            )?)
        } else {
            None
        };

        let mut examples = Vec::new();

        if let Some(examples_structure) = data.examples.as_ref() {
            for example in examples_structure {
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
                    "untouched" => DisambiguationExample::Unchanged(text),
                    "ambiguous" => DisambiguationExample::Changed(DisambiguationChange {
                        text,
                        before: parse_tag_form(
                            example
                                .inputform
                                .as_ref()
                                .expect("must have inputform when ambiguous example"),
                            info,
                        ),
                        after: parse_tag_form(
                            &example
                                .outputform
                                .as_ref()
                                .expect("must have inputform when ambiguous example"),
                            info,
                        ),
                        char_span: char_span.expect("must have marker when ambiguous example"),
                    }),
                    x => panic!("unknown disambiguation example type {}", x),
                };

                examples.push(test);
            }
        }

        Ok(DisambiguationRule {
            engine: Engine::Token(TokenEngine {
                composition,
                antipatterns,
            }),
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
            start,
            end,
            examples,
            id: String::new(),
        })
    }
}
