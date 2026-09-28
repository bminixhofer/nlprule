use std::{ops::Range, sync::Arc};

use super::{structure, Error};
use crate::{tokenizer::tag::Tagger, types::*};
use crate::{utils, utils::regex::Regex};
use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};

pub use structure::{read_disambiguation_rules, read_rules};

use crate::rule::disambiguation::*;
use crate::rule::engine::composition::concrete::*;
use crate::rule::engine::composition::*;
use crate::rule::engine::*;
use crate::rule::grammar::*;
use crate::rule::post_filter::PostFilter;
use crate::rule::{id::Index, DisambiguationRule, Rule, Unification};

// this is set arbitrarily at the moment, could be an option
#[inline]
fn max_matches() -> usize {
    20
}

#[derive(Serialize, Deserialize, Debug)]
pub(crate) struct RegexCache {
    cache: DefaultHashMap<u64, Option<DefaultHashSet<WordIdInt>>>,
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

    pub(crate) fn get(&self, key: &u64) -> Option<&Option<DefaultHashSet<WordIdInt>>> {
        self.cache.get(key)
    }

    pub(crate) fn insert(&mut self, key: u64, value: Option<DefaultHashSet<WordIdInt>>) {
        self.cache.insert(key, value);
    }
}

pub(crate) struct BuildInfo {
    tagger: Arc<Tagger>,
    regex_cache: RegexCache,
    synthesizer: Option<Arc<crate::rule::synthesizer::Synthesizer>>,
    lang: String,
}

impl BuildInfo {
    pub fn new(tagger: Arc<Tagger>, regex_cache: RegexCache) -> Self {
        BuildInfo {
            tagger,
            regex_cache,
            synthesizer: None,
            lang: String::new(),
        }
    }

    pub fn set_lang(&mut self, lang: String) {
        self.lang = lang;
    }

    pub fn lang(&self) -> &str {
        &self.lang
    }

    pub fn tagger(&self) -> &Arc<Tagger> {
        &self.tagger
    }

    pub fn mut_regex_cache(&mut self) -> &mut RegexCache {
        &mut self.regex_cache
    }

    pub fn set_synthesizer(&mut self, synthesizer: Option<Arc<crate::rule::synthesizer::Synthesizer>>) {
        self.synthesizer = synthesizer;
    }

    pub fn synthesizer(&self) -> Option<&Arc<crate::rule::synthesizer::Synthesizer>> {
        self.synthesizer.as_ref()
    }
}

fn parse_match_attribs(
    attribs: impl structure::MatchAttributes,
    text: Option<&str>,
    case_sensitive: bool,
    text_match_idx: Option<usize>,
    sub_transform: Option<crate::rule::engine::composition::SubTransform>,
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
        // an explicit "no" behaves like no regexp
        None | Some("no") => false,
        x => panic!("unknown regexp value {:?}", x),
    };

    let is_postag_regexp = match attribs.postag_regexp().as_deref() {
        Some("yes") => true,
        None | Some("no") => false,
        x => panic!("unknown postag_regexp value {:?}", x),
    };

    let negate = match attribs.negate().as_deref() {
        Some("yes") => true,
        // an explicit "no" behaves like not negated
        Some("no") => false,
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

    // a `<match>` reference with its own regexp_match/replace transform
    // produces a literal text (the token's own regexp flag is overridden).
    // A plain `<match>` reference under `regexp="yes"` with no own text
    // (de DOPPELUNG_JAHRE, en REPEATED_VERBS) also behaves as a literal:
    // LT would compile the referenced text as a regex, but the reference
    // is a concrete word there, so matching it verbatim is equivalent.
    let is_regex = is_regex
        && !(text_match_idx.is_some() && (sub_transform.is_some() || text.is_none()));

    if text.is_some() || text_match_idx.is_some() {
        let matcher = if is_regex {
            if let Some(text) = text {
                let regex = Regex::from_java_regex(text.trim(), true, case_sensitive);
                Matcher::new_regex(regex?, negate, inflected)
            } else {
                return Err(Error::Unexpected("`text` must be set if regex".into()));
            }
        } else {
            let mut matcher = Matcher::new_string(
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
            );
            if text_match_idx.is_some() {
                matcher.sub_transform = sub_transform;
            }
            matcher
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
            // "ignore" behaves like not specifying the attribute
            "ignore" => {
                return Ok(AndAtom::and(atoms));
            }
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
                    match parse_match_attribs(x, exception_text, case_sensitive, None, None, info) {
                        Ok(atom) => atom,
                        Err(err) => return Some(Err(err)),
                    };

                // LT: an exception whose postag lists SENT_END as an
                // alternative (e.g. `[^M]*M:.*|SENT_END`) is the idiom to
                // stop the token from matching the last real word of a
                // sentence without terminal punctuation - the token that
                // carries the merged SENT_END reading (plain postag
                // alternatives are literal, so SENT_END is the only live
                // alternative; br depends on it). OR BEFORE the offset wrap
                // so scoped exceptions check the scoped position.
                if x.postag
                    .as_ref()
                    .map_or(false, |p| p.split('|').any(|alt| alt.trim() == "SENT_END"))
                {
                    atom = OrAtom::or(vec![atom, SentEndAtom::default().into()]);
                }

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

    let mut token = token.clone();
    let mut sub_transform: Option<crate::rule::engine::composition::SubTransform> = None;
    let text_match_idx = if let Some(parts) = &token.parts {
        let mut idx = None;
        for part in parts {
            if let structure::TokenPart::Sub(sub) = part {
                match sub.no.parse::<usize>() {
                    Ok(no) => {
                        idx = Some(no + 1);
                        // `<match no="N" postag="..."/>`: the current token
                        // must also match the match element's postag (the
                        // text comes from the referenced token)
                        if sub.postag.is_some() {
                            token.postag = sub.postag.clone();
                        }
                        if sub.postag_regexp.is_some() {
                            token.postag_regexp = sub.postag_regexp.clone();
                        }
                        // `<match no="N" regexp_match="..." regexp_replace="..."
                        // case_conversion="..."/>`: transform the referenced
                        // token's text before comparing
                        if (sub.regexp_match.is_some() || sub.case_conversion.is_some())
                            && std::env::var("NLPRULE_NO_SUBTRANSFORM").is_err()
                        {
                            let regex = match &sub.regexp_match {
                                Some(pattern) => Some(
                                    crate::utils::regex::Regex::from_java_regex(
                                        pattern.trim(),
                                        true,
                                        true,
                                    )?,
                                ),
                                None => None,
                            };
                            sub_transform = Some(
                                crate::rule::engine::composition::SubTransform {
                                    regex,
                                    replace: sub.regexp_replace.clone(),
                                    case: sub.case_conversion.clone(),
                                },
                            );
                        }
                        break;
                    }
                    Err(err) => return Err(err.into()),
                }
            }
        }
        idx
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
    let mut atom = parse_match_attribs(&token, text, case_sensitive, text_match_idx, sub_transform, info)?;
    atom = AndAtom::and(vec![
        atom,
        get_exceptions(&token, case_sensitive, false, info)?,
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
            atom: get_exceptions(&token, case_sensitive, true, info)?,
            quantifier: Quantifier::new(0, to_skip),
            visible: false,
            greedy: false,
            unify: None,
        });
    }

    Ok(parts)
}

/// Converts a Java regex replacement string (`$1`, `${name}`) to the syntax
/// understood by the onig / fancy-regex backends (`\1`).
fn java_replacement(replacement: &str) -> String {
    let mut out = String::with_capacity(replacement.len());
    let mut chars = replacement.chars().peekable();

    while let Some(c) = chars.next() {
        if c == '$' {
            if chars.peek() == Some(&'{') {
                chars.next();
                for c in chars.by_ref() {
                    if c == '}' {
                        break;
                    }
                    out.push(c);
                }
                out = format!("\\{}", out);
            } else {
                let mut digits = String::new();
                while let Some(&d) = chars.peek() {
                    if d.is_ascii_digit() {
                        digits.push(d);
                        chars.next();
                    } else {
                        break;
                    }
                }
                if digits.is_empty() {
                    out.push('$');
                } else {
                    out.push('\\');
                    out.push_str(&digits);
                }
            }
        } else {
            out.push(c);
        }
    }

    out
}

fn parse_match(m: structure::Match, engine: &Engine, info: &mut BuildInfo) -> Result<Match, Error> {
    let id = m
        .no
        .parse::<usize>()
        .expect("no must be parsable as usize.");

    let postag = if let Some(postag) = m.postag {
        match m.postag_regex.as_deref() {
            Some("yes") => Some(PosTagSelector::Regex {
                regex: Regex::from_java_regex(&postag, true, true)?,
                raw: postag.clone(),
                replace: m.postag_replace
                    .as_deref()
                    .map(java_replacement)
                    .map(Some)
                    .unwrap_or(None),
            }),
            None | Some("no") => Some(PosTagSelector::Exact(postag)),
            x => panic!("unknown postag_regex value {:?}", x),
        }
    } else {
        None
    };

    let regex_replacer = match (m.regexp_match, m.regexp_replace) {
        (Some(regex_match), Some(regex_replace)) => Some((
            Regex::from_java_regex(&regex_match, false, true)?,
            java_replacement(&regex_replace),
        )),
        _ => None,
    };

    let include_skipped = match m.include_skipped.as_deref() {
        None | Some("none") => None,
        Some("all") => Some(IncludeSkipped::All),
        Some("following") => Some(IncludeSkipped::Following),
        x => panic!("unknown include_skipped value {:?}", x),
    };

    Ok(Match {
        id: engine.to_graph_id(id)?,
        conversion: match m.case_conversion.as_deref() {
            Some("alllower") => Conversion::AllLower,
            Some("startlower") => Conversion::StartLower,
            Some("startupper") => Conversion::StartUpper,
            Some("allupper") => Conversion::AllUpper,
            Some("preserve") => Conversion::Preserve,
            Some("firstupper") => Conversion::FirstUpper,
            Some("notashkeel") => Conversion::NotAshkeel,
            Some(x) => {
                return Err(Error::Unimplemented(format!(
                    "case conversion {} not supported.",
                    x
                )))
            }
            None => Conversion::Nop,
        },
        postag,
        static_lemma: m.content.map(|x| x.to_string()),
        text: m.text.map(|x| x.to_string()),
        regex_replacer,
        include_skipped,
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
                postag: None,
                static_lemma: None,
                text: None,
                regex_replacer: None,
                include_skipped: None,
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
    data: structure::Suggestion,
    engine: &Engine,
    info: &mut BuildInfo,
) -> Result<Synthesizer, Error> {
    let mut parts = Vec::new();
    for part in data.parts {
        match part {
            structure::SuggestionPart::Text(text) => {
                parts.extend(parse_synthesizer_text(text.as_str(), engine)?);
            }
            structure::SuggestionPart::Match(m) => {
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

/// Parses the tokens of an `<and>`/`<or>` group. Tokens with a `skip`
/// attribute produce a second, invisible gap part in `parse_token`; LT
/// applies that skip after the group matched, so the gaps are hoisted out
/// of the group and merged (or-combined exception atoms, maximum skip
/// distance) into a single gap part appended after the group.
/// A member with `min="0"` makes the whole group optional (LT treats the
/// or-group as one element whose alternatives include the zero-width case).
/// Returns the member atoms, the group quantifier and the merged gap part.
fn parse_group_tokens(
    tokens: &[structure::Token],
    case_sensitive: bool,
    info: &mut BuildInfo,
) -> Result<(Vec<Atom>, Quantifier, Vec<Part>), Error> {
    let mut atoms = Vec::new();
    let mut gap_atom: Option<Atom> = None;
    let mut gap_max = 0usize;
    let mut group_min = 1usize;
    let mut group_max = 1usize;
    // members with their own repetition (min/max, fr A_INFINITIF
    // `<token min="0" max="3" ...>` inside and/or): LT evaluates them as
    // parallel subpatterns; we hoist them out of the group as sequential
    // parts after it — the closest approximation that keeps the rule
    // loadable and firing
    let mut hoisted: Vec<Part> = Vec::new();

    for x in tokens {
        let mut parsed = parse_token(x, case_sensitive, info)?;

        if parsed.len() > 2 {
            return Err(Error::Unimplemented(
                "control flow in parallel tokens is not implemented.".into(),
            ));
        }

        let part = parsed.remove(0);
        if part.quantifier.max > 1 {
            hoisted.push(part);
        } else {
            if part.quantifier.min == 0 {
                group_min = 0;
            }
            atoms.push(part.atom);
        }

        if let Some(gap) = parsed.pop() {
            gap_max = gap_max.max(gap.quantifier.max);
            let atom = match gap_atom {
                Some(existing) => OrAtom::or(vec![existing, gap.atom]),
                None => gap.atom,
            };
            gap_atom = Some(atom);
        }
    }

    let mut extras: Vec<Part> = Vec::new();
    if let Some(atom) = gap_atom {
        extras.push(Part {
            atom,
            quantifier: Quantifier::new(0, gap_max),
            visible: false,
            greedy: false,
            unify: None,
        });
    }
    extras.extend(hoisted);

    Ok((atoms, Quantifier::new(group_min, group_max), extras))
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
                let (atoms, quantifier, extras) =
                    parse_group_tokens(&tokens.tokens, case_sensitive, info)?;
                let atom = AndAtom::and(atoms);
                let mut parts = vec![Part {
                    atom,
                    quantifier,
                    greedy: true,
                    visible: true,
                    unify: tokens.tokens[0].unify.as_ref().map(|x| x == "yes"),
                }];
                parts.extend(extras);
                parts
            }
            structure::TokenCombination::Or(tokens) => {
                let (atoms, quantifier, extras) =
                    parse_group_tokens(&tokens.tokens, case_sensitive, info)?;
                let atom = OrAtom::or(atoms);
                let mut parts = vec![Part {
                    atom,
                    quantifier,
                    greedy: true,
                    visible: true,
                    unify: tokens.tokens[0].unify.as_ref().map(|x| x == "yes"),
                }];
                parts.extend(extras);
                parts
            }
            structure::TokenCombination::Feature(_) => Vec::new(),
        });
    }

    Ok(out)
}


fn parse_antipatterns(
    antipatterns: Option<Vec<structure::Pattern>>,
    unifications: &Option<Vec<structure::Unification>>,
    info: &mut BuildInfo,
) -> Result<Vec<crate::rule::engine::AntiPattern>, Error> {
    let mut out = Vec::new();
    for pattern in antipatterns.into_iter().flatten() {
        let unify_filters = parse_features(&pattern, unifications, info);
        let (composition, ..) = parse_pattern(pattern.clone(), info)?;
        let unify_mask: Vec<_> = composition.parts.iter().map(|part| part.unify).collect();
        let unification = if unify_filters.is_empty() {
            None
        } else {
            Some(crate::rule::Unification {
                element_filters: Vec::new(),
                mask: unify_mask,
                filters: unify_filters,
            })
        };
        out.push(crate::rule::engine::AntiPattern {
            composition,
            unification,
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
                let (atoms, quantifier, gap) =
                    parse_group_tokens(&tokens.tokens, case_sensitive, info)?;
                let atom = AndAtom::and(atoms);

                composition_parts.push(Part {
                    atom,
                    quantifier,
                    greedy: true,
                    visible: true,
                    unify: tokens.tokens[0].unify.as_ref().map(|x| x == "yes"),
                });
                composition_parts.extend(gap);
            }
            structure::PatternPart::Or(tokens) => {
                let (atoms, quantifier, gap) =
                    parse_group_tokens(&tokens.tokens, case_sensitive, info)?;
                let atom = OrAtom::or(atoms);

                composition_parts.push(Part {
                    atom,
                    quantifier,
                    greedy: true,
                    visible: true,
                    unify: tokens.tokens[0].unify.as_ref().map(|x| x == "yes"),
                });
                composition_parts.extend(gap);
            }
            structure::PatternPart::Feature(_) => {}
            structure::PatternPart::Example(_) => {}
        }
    }

    let start = start.unwrap_or(1) as usize;
    let end = end.unwrap_or_else(|| get_last_id(&composition_parts)) as usize - 1;

    let composition = Composition::new(composition_parts)?;

    Ok((composition, start, end))
}

fn parse_features(
    pattern: &structure::Pattern,
    unifications: &Option<Vec<structure::Unification>>,
    info: &mut BuildInfo,
) -> Vec<Vec<PosFilter>> {
    let mut filters = Vec::new();
    let mut parse_feature = |id: &str| -> Vec<PosFilter> {
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
                match &equiv.token.postag {
                    // `<token/>` (sr): the equivalence applies to any token
                    None => parse_pos_filter(".*", Some("yes"), info),
                    Some(postag) => parse_pos_filter(
                        postag,
                        equiv.token.postag_regexp.as_deref(),
                        info,
                    ),
                }
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
    pub(crate) fn from_rule_structure(
        data: structure::Rule,
        info: &mut BuildInfo,
    ) -> Result<Rule, Error> {
        let post_filter = data
            .filter
            .as_ref()
            .map(|filter| parse_post_filter(filter, info))
            .transpose()?;

        let (engine, start, end) = match (&data.pattern, data.regex) {
            (Some(_), Some(_)) => Err(Error::Unexpected(
                "must not contain both `pattern` and `regexp`.".into(),
            )),
            (None, None) => Err(Error::Unexpected(
                "either `pattern` or `regexp` must be supplied.".into(),
            )),
            (Some(pattern), None) => {
                let (composition, start, end) = parse_pattern(pattern.clone(), info)?;
                let antipatterns =
                    parse_antipatterns(data.antipatterns, &data.unifications, info)?;

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
            let unify_filters = parse_features(&pattern, &data.unifications, info);
            let unify_mask: Vec<_> = maybe_composition
                .unwrap()
                .parts
                .iter()
                .map(|part| part.unify)
                .collect();
            let element_filters = pattern_element_filters(&pattern, unify_mask.len(), info);
            Some((unify_filters, unify_mask, element_filters))
        } else {
            None
        };

        let mut message_parts = Vec::new();
        let mut suggesters = Vec::new();

        for part in data.message.parts {
            match part {
                structure::MessagePart::Suggestion(suggestion) => {
                    let suggester = parse_suggestion(suggestion.clone(), &engine, info)?;
                    // simpler to just parse a second time than cloning the result
                    message_parts.extend(parse_suggestion(suggestion, &engine, info)?.parts);
                    suggesters.push(suggester);
                }
                structure::MessagePart::Text(text) => {
                    message_parts.extend(parse_synthesizer_text(text.as_str(), &engine)?);
                }
                structure::MessagePart::Match(m) => {
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
            // rules without a suggestion only report an error without replacement —
            // these are valid, they just never correct anything
            log::debug!("Rule without suggestion: this rule only reports errors.");
        }

        assert!(!message_parts.is_empty(), "Rules must have a message.");

        let mut examples = Vec::new();
        for example in &data.examples {
            // examples with `type` (e.g. 'triggers_error') only affect rule testing;
            // they are valid and can be treated like untyped examples
            let _ = &example.kind;

            let mut texts = Vec::new();
            let mut suggestion: Option<Suggestion> = None;

            for part in &example.parts {
                match part {
                    structure::ExamplePart::Text(text) => {
                        texts.push(text.as_str());
                    }
                    structure::ExamplePart::Marker(marker) => {
                        let (bytes_before, chars_before) =
                            texts.iter().fold((0, 0), |acc, text| {
                                (acc.0 + text.len(), acc.1 + text.chars().count())
                            });

                        if suggestion.is_some() {
                            // LT tolerates examples with several markers
                            // (fr A_A_ACCENT2, CROASSE_COASSE): keep the
                            // first marker as the error span, treat the
                            // rest as plain text
                            texts.push(marker.text.as_str());
                            continue;
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
                kind: example.kind.clone(),
            });
        }

        let unification = if let Some((unify_filters, unify_mask, element_filters)) = unify_data {
            if unify_filters.is_empty() {
                None
            } else {
                Some(Unification {
                    filters: unify_filters,
                    mask: unify_mask,
                    element_filters,
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
            post_filter,
            // fields below need information from rule group / category, so are set later
            id: Index::default(),
            name: String::new(),
            category_name: String::new(),
            category_type: None,
            enabled: true,
        })
    }
}

/// Maps an LT `<filter>` (class + args) to a runtime [PostFilter].
/// Ported Java filter classes become [PostFilter::Java]; unsupported classes
/// return an error so the rule is skipped, as before.

/// Per composition part: the pattern element's own postag constraint
/// (None for invisible skip-gap parts), so runtime unification only
/// considers readings the element actually matched, like LT's Unifier.
fn pattern_element_filters(
    pattern: &structure::Pattern,
    expected_len: usize,
    info: &mut BuildInfo,
) -> Vec<Option<PosFilter>> {
    let mut element_filters: Vec<Option<PosFilter>> = Vec::new();
    let mut push_element = |token: &structure::Token| {
        let filter = token
            .postag
            .as_ref()
            .map(|x| parse_pos_filter(x, token.postag_regexp.as_deref(), info));
        element_filters.push(filter);
        if token.skip.is_some() {
            element_filters.push(None);
        }
    };
    for part in &pattern.parts {
        match part {
            structure::PatternPart::Token(token) => push_element(token),
            structure::PatternPart::Marker(marker) => {
                for token in &marker.tokens {
                    match token {
                        structure::TokenCombination::Token(t) => push_element(t),
                        structure::TokenCombination::And(ts)
                        | structure::TokenCombination::Or(ts) => push_element(&ts.tokens[0]),
                        structure::TokenCombination::Feature(_) => {}
                    }
                }
            }
            structure::PatternPart::And(ts)
            | structure::PatternPart::Or(ts) => push_element(&ts.tokens[0]),
            structure::PatternPart::Feature(_) => {}
            structure::PatternPart::Example(_) => {}
        }
    }
    if element_filters.len() != expected_len {
        // layout mismatch (unexpected pattern shape): fall back to
        // unrestricted unification
        element_filters = vec![None; expected_len];
    }
    element_filters
}

fn parse_post_filter(filter: &structure::Filter, info: &mut BuildInfo) -> Result<PostFilter, Error> {
    use crate::rule::filter_java::{DateLang, FindSuggVariant, JClass, JavaFilter};
    use crate::rule::post_filter::{PostFilter, UnderlineMode};

    let name = filter
        .class
        .split('.')
        .next_back()
        .unwrap_or_else(|| filter.class.as_str());

    // language module of the class, e.g. `en` in
    // `org.languagetool.rules.en.FutureDateFilter`
    let package = filter
        .class
        .trim_start_matches("org.languagetool.rules.")
        .split('.')
        .next()
        .unwrap_or("")
        .to_string();
    let _lang = info.lang().trim().to_string();

    let date_lang = |package: &str| -> Option<DateLang> {
        match package {
            "en" => Some(DateLang::En),
            "de" => Some(DateLang::De),
            "fr" => Some(DateLang::Fr),
            "es" => Some(DateLang::Es),
            "ca" => Some(DateLang::Ca),
            "it" => Some(DateLang::It),
            "nl" => Some(DateLang::Nl),
            "pt" => Some(DateLang::Pt),
            "ru" => Some(DateLang::Ru),
            "pl" => Some(DateLang::Pl),
            "uk" => Some(DateLang::Uk),
            "sr" => Some(DateLang::Sr),
            "br" => Some(DateLang::Br),
            "eo" => Some(DateLang::Eo),
            "ar" => Some(DateLang::Ar),
            _ => None,
        }
    };

    let args: Vec<(String, String)> = filter
        .args
        .split(' ')
        .filter_map(|x| {
            x.find(':').map(|idx| {
                (
                    x[..idx].to_string(),
                    x[(idx + ':'.len_utf8())..].to_string(),
                )
            })
        })
        .collect();

    fn make_java(
        class: JClass,
        args: Vec<(String, String)>,
        regex_keys: &[&str],
    ) -> Result<PostFilter, Error> {
        let mut regex_args = Vec::new();
        for key in regex_keys {
            if let Some((_, value)) = args.iter().find(|(k, _)| k == key) {
                let regex = Regex::from_java_regex(value, false, false)?;
                regex_args.push((key.to_string(), regex));
            }
        }
        Ok(PostFilter::Java(JavaFilter {
            class,
            args,
            regex_args,
        }))
    }
    let make_java = |class: JClass, regex_keys: &[&str]| {
        make_java(class, args.clone(), regex_keys)
    };
    let arg_value = |key: &str| -> Option<&str> {
        args.iter()
            .find(|(k, _)| k == key)
            .map(|(_, v)| v.as_str())
    };

    match name {
        "UnderlineSpacesFilter" => Ok(PostFilter::UnderlineSpaces {
            mode: match arg_value("underlineSpaces") {
                Some("before") => UnderlineMode::Before,
                Some("after") => UnderlineMode::After,
                Some("both") => UnderlineMode::Both,
                x => panic!("unknown underlineSpaces value {:?}", x),
            },
        }),
        "ApostropheTypeFilter" => Ok(PostFilter::ApostropheType {
            has_typographic: arg_value("hasTypographicalApostrophe") == Some("true"),
        }),
        "RegexAntiPatternFilter" => Ok(PostFilter::RegexAntiPattern {
            regexes: arg_value("antipatterns")
                .ok_or_else(|| {
                    Error::Unexpected("RegexAntiPatternFilter must have `antipatterns`".into())
                })?
                .split('|')
                .map(|x| Regex::from_java_regex(x, false, false))
                .collect::<Result<Vec<_>, Error>>()?,
        }),
        "AdaptSuggestionsFilter" => Ok(PostFilter::AdaptSuggestions { ca: package == "ca" }),
        "SuppressIfAnyRuleMatchesFilter" => Ok(PostFilter::SuppressIfAny {
            rule_ids: arg_value("ruleIDs")
                .unwrap_or("")
                .split(',')
                .map(|x| x.to_string())
                .collect(),
        }),
        "DateCheckFilter" => {
            let dl = date_lang(&package)
                .ok_or_else(|| Error::Unimplemented(format!("filter {} is not implemented.", name)))?;
            // languages whose DateCheckFilter extends AbstractDateCheckWithSuggestionsFilter
            let with_suggestions = matches!(package.as_str(), "en" | "de" | "fr" | "es" | "ca" | "nl" | "pt");
            make_java(JClass::DateCheck { lang: dl, with_suggestions }, &[])
        }
        "YMDDateCheckFilter" => {
            let dl = date_lang(&package)
                .ok_or_else(|| Error::Unimplemented(format!("filter {} is not implemented.", name)))?;
            make_java(JClass::YMDDateCheck { lang: dl }, &[])
        }
        "DMYDateCheckFilter" => {
            let dl = date_lang(&package)
                .ok_or_else(|| Error::Unimplemented(format!("filter {} is not implemented.", name)))?;
            make_java(JClass::DMYDateCheck { lang: dl }, &[])
        }
        "FutureDateFilter" => {
            let dl = date_lang(&package)
                .ok_or_else(|| Error::Unimplemented(format!("filter {} is not implemented.", name)))?;
            make_java(JClass::FutureDate { lang: dl }, &[])
        }
        "NewYearDateFilter" => make_java(JClass::NewYearDate { lang: date_lang(&package).ok_or_else(|| Error::Unimplemented(format!("filter {} is not implemented.", name)))?, ymd: false }, &[]),
        "YMDNewYearDateFilter" => make_java(JClass::NewYearDate { lang: date_lang(&package).ok_or_else(|| Error::Unimplemented(format!("filter {} is not implemented.", name)))?, ymd: true }, &[]),
        "RecentYearFilter" => make_java(JClass::RecentYear, &[]),
        "DateRangeChecker" => make_java(JClass::DateRange, &[]),
        "ShortenedYearRangeChecker" => make_java(JClass::ShortenedYearRange, &[]),
        "WhitespaceCheckFilter" => make_java(JClass::WhitespaceCheck, &[]),
        "MultitokenSpellerFilter" => make_java(JClass::MultitokenSpeller, &[]),
        "OrdinalSuffixFilter" => make_java(JClass::OrdinalSuffix, &[]),
        "CompoundCheckFilter" => make_java(JClass::CompoundCheck, &[]),
        "CompoundFilter" if package.as_str() == "nl" => make_java(JClass::NlCompound, &[]),
        "INNNumberFilter" => make_java(JClass::InnNumber, &[]),
        "DecadeSpellingFilter" => make_java(JClass::DecadeSpelling, &[]),
        "UppercaseNounReadingFilter" => make_java(JClass::UppercaseNounReading, &[]),
        "ConvertToSentenceCaseFilter" | "EnglishConvertToSentenceCaseFilter" => {
            make_java(JClass::ConvertToSentenceCase, &[])
        }
        "RomanNumeralFilter" => make_java(JClass::RomanNumeral, &[]),
        "RegularIrregularParticipleFilter" => make_java(JClass::RegularIrregularParticiple, &[]),
        "ValidWordFilter" => make_java(JClass::ValidWord, &[]),
        "RemoveUnknownCompoundsFilter" => make_java(JClass::RemoveUnknownCompounds, &[]),
        "ConfusionCheckFilter" => make_java(JClass::ConfusionCheck, &[]),
        "DiacriticsCheckFilter" => make_java(JClass::DiacriticsCheck, &[]),
        "AddCommasFilter" => make_java(JClass::AddCommas, &[]),
        "AdvancedSynthesizerFilter" | "ArabicAdvancedSynthesizerFilter" => {
            make_java(JClass::AdvancedSynthesizer, &["lemmaSelect", "postagSelect"])
        }
        "RussianPartialPosTagFilter"
        | "NoDisambiguationRussianPartialPosTagFilter"
        | "IrishPartialPosTagFilter"
        | "NoDisambiguationIrishPartialPosTagFilter"
        | "EnglishPartialPosTagFilter"
        | "NoDisambiguationEnglishPartialPosTagFilter"
        | "FrenchPartialPosTagFilter"
        | "NoDisambiguationFrenchPartialPosTagFilter" => {
            make_java(JClass::PartialPosTag, &["regexp", "postag_regexp"])
        }
        // ar syntax_numeric_0003: number/noun agreement
        "ArabicNumberPhraseFilter" => make_java(JClass::ArabicNumberPhrase, &[]),
        // ca grammar references the ES-variant class by its per-language
        // name (`org.languagetool.rules.ca.FindSuggestionsEsFilter`)
        "FindSuggestionsEsFilter" => {
            make_java(
                JClass::FindSuggestions { variant: FindSuggVariant::Es },
                &["desiredPostag", "priorityPostag", "removeSuggestionsRegexp"],
            )
        }
        // ca SPELLOUT_NUMBERS: suggestion = the number spelled in words
        "CatalanNumberSpellerFilter" => {
            make_java(JClass::CatalanNumberSpeller, &[])
        }
        "FindSuggestionsFilter" => {
            let variant = match package.as_str() {
                "en" => FindSuggVariant::En,
                "fr" => FindSuggVariant::Fr,
                "es" => FindSuggVariant::Es,
                "ca" => FindSuggVariant::Ca,
                _ => {
                    return Err(Error::Unimplemented(format!(
                        "filter {} is not implemented.",
                        name
                    )))
                }
            };
            make_java(
                JClass::FindSuggestions { variant },
                &["desiredPostag", "priorityPostag", "removeSuggestionsRegexp"],
            )
        }
        x if x.ends_with("SuppressMisspelledSuggestionsFilter") => {
            make_java(JClass::SuppressMisspelledSuggestions, &[])
        }
        "SuggestionsFilter" if package == "fr" => {
            make_java(JClass::SuggestionsRemove, &["RemoveSuggestionsRegexp"])
        }
        "MakeContractionsFilter" => make_java(JClass::MakeContractions, &[]),
        x if x.ends_with("NumberInWordFilter") => make_java(JClass::NumberInWord, &[]),
        "TextToNumberFilter" => match package.as_str() {
            "es" => make_java(JClass::TextToNumber { lang: crate::rule::filter_java::TextNumberLang::Es }, &[]),
            "ca" => make_java(JClass::TextToNumber { lang: crate::rule::filter_java::TextNumberLang::Ca }, &[]),
            _ => {
                return Err(Error::Unimplemented(
                    "filter TextToNumberFilter is not implemented.".into(),
                ))
            }
        },
        "InterrogativeVerbFilter" => make_java(JClass::InterrogativeVerb, &[]),
        "WordWithDeterminerFilter" => make_java(JClass::WordWithDeterminer, &[]),
        "PortugueseEnclisisFilter" => make_java(JClass::EnclisisPt, &[]),
        "PortugueseProclisisFilter" => make_java(JClass::ProclisisPt, &[]),
        "SynthesizeWithDeterminerFilter" => {
            make_java(JClass::SynthesizeWithDeterminer, &["lemmaSelect"])
        }
        "ConvertToGenderAndNumberFilter" => {
            make_java(JClass::ConvertToGenderAndNumber, &["lemmaSelect"])
        }
        "PossessiusRedundantsFilter" => make_java(JClass::PossessiusRedundants, &[]),
        "AdjustPronounsFilter" => make_java(JClass::AdjustPronouns, &[]),
        "AdjustVerbSuggestionsFilter" => make_java(JClass::AdjustVerbSuggestions, &[]),
        "AnarASuggestionsFilter" => make_java(JClass::AnarASuggestions, &[]),
        "DonarTempsSuggestionsFilter" => make_java(JClass::DonarTempsSuggestions, &[]),
        "OblidarseSugestionsFilter" => make_java(JClass::OblidarseSuggestions, &[]),
        "PortarGerundiSuggestionsFilter" => make_java(JClass::PortarGerundiSuggestions, &[]),
        "PortarTempsSuggestionsFilter" => make_java(JClass::PortarTempsSuggestions, &[]),
        "ArabicMasdarToVerbFilter" => make_java(JClass::MasdarToVerbAr, &[]),
        "ArabicVerbToMafoulMutlaqFilter" => make_java(JClass::VerbToMafoulMutlaqAr, &[]),
        "ArabicAdjectiveToExclamationFilter" => make_java(JClass::AdjectiveToExclamationAr, &[]),
        "AdverbFilter" => make_java(JClass::AdverbEn, &[]),
        "ArabicDateCheckFilter" => {
            make_java(JClass::DateCheck { lang: DateLang::Ar, with_suggestions: false }, &[])
        }
        "InsertCommaFilter" if package == "de" => make_java(JClass::InsertCommaDe, &[]),
        "PotentialCompoundFilter" => make_java(JClass::PotentialCompoundDe, &[]),
        "PostponedAdjectiveConcordanceFilter" => match package.as_str() {
            "fr" => make_java(JClass::PostponedAdjective { lang: crate::rule::filter_java::PostponedAdjLang::Fr }, &[]),
            "es" => make_java(JClass::PostponedAdjective { lang: crate::rule::filter_java::PostponedAdjLang::Es }, &[]),
            "ca" => make_java(JClass::PostponedAdjective { lang: crate::rule::filter_java::PostponedAdjLang::Ca }, &[]),
            _ => {
                return Err(Error::Unimplemented(format!(
                    "filter {} is not implemented.",
                    "PostponedAdjectiveConcordanceFilter"
                )))
            }
        },
        x => Err(Error::Unimplemented(format!("filter {} is not implemented.", x))),
    }
}

fn parse_tag_form(form: &str, info: &mut BuildInfo) -> Result<owned::Word, Error> {
    lazy_static! {
        static ref REGEX: Regex = Regex::new(r"(.+?)\[(.+?)\]".into());
    }

    let captures = REGEX
        .captures(form)
        .ok_or_else(|| Error::Unexpected(format!("tag form must match regex, found '{}'", form)))?;
    let text = captures.get(1).expect("1st regex group exists").as_str();
    let tags = captures.get(2).expect("2nd regex group exists").as_str();

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

    Ok(owned::Word {
        text: info.tagger.id_word(text.into()).to_owned_id(),
        tags,
    })
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

fn parse_pos_filter(postag: &str, postag_regexp: Option<&str>, info: &mut BuildInfo) -> PosFilter {
    match postag_regexp.as_deref() {
        Some("yes") => PosFilter::new(PosMatcher::new(
            Matcher::new_regex(
                Regex::from_java_regex(&postag, true, true).unwrap(),
                false,
                true,
            ),
            info,
        )),
        Some(_) | None => PosFilter::new(PosMatcher::new(
            Matcher::new_string(either::Left(postag.into()), false, false, true),
            info,
        )),
    }
}

impl DisambiguationRule {
    pub(crate) fn from_rule_structure(
        data: structure::DisambiguationRule,
        info: &mut BuildInfo,
    ) -> Result<DisambiguationRule, Error> {
        // might need the pattern later so clone it here
        let (composition, start, end) = parse_pattern(data.pattern.clone(), info)?;

        let unify_filters = parse_features(&data.pattern, &data.unifications, info);
        let unify_mask: Vec<_> = composition.parts.iter().map(|part| part.unify).collect();
        let element_filters = pattern_element_filters(&data.pattern, unify_mask.len(), info);

        let antipatterns = parse_antipatterns(data.antipatterns, &data.unifications, info)?;

        let engine = Engine::Token(TokenEngine {
            composition,
            antipatterns,
        });

        // raw chunk tags for action="addchunk" (needs the plain strings,
        // not interned PosIds)
        let raw_chunk_tags: Vec<String> = data
            .disambig
            .word_datas
            .as_ref()
            .map(|parts| {
                parts
                    .iter()
                    .filter_map(|part| match part {
                        structure::DisambiguationPart::WordData(w) => w.pos.clone(),
                        _ => None,
                    })
                    .collect()
            })
            .unwrap_or_default();

        let word_datas: Vec<_> = if let Some(wds) = data.disambig.word_datas {
            wds.into_iter()
                .map(|part| match part {
                    structure::DisambiguationPart::WordData(x) => Ok(either::Left(
                        owned::WordData::from_structure(x, info),
                    )),
                    structure::DisambiguationPart::Match(x) => {
                        if x.content.is_some() {
                            return Err(Error::Unimplemented(
                                "text content in `match` of a disambiguation is not supported."
                                    .into(),
                            ));
                        }

                        Ok(either::Right(parse_pos_filter(
                            &x.postag.clone().expect("match must have a postag"),
                            x.postag_regexp.as_deref(),
                            info,
                        )))
                    }
                })
                .collect::<Result<Vec<_>, Error>>()?
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
                if let Some(postag) = data.disambig.postag.as_ref() {
                    // add a reading with the given POS; the lemma defaults to the
                    // matched token text at runtime (see Disambiguation::Add)
                    let lemma = info.tagger.id_word("".into()).to_owned_id();
                    let pos = info.tagger.id_tag(postag).to_owned_id();
                    Ok(Disambiguation::Add(vec![owned::WordData::new(
                        lemma, pos,
                    )]))
                } else {
                    Ok(Disambiguation::Add(
                        word_datas
                            .into_iter()
                            .map(|x| x.left().expect("match not supported for `add`"))
                            .collect(),
                    ))
                }
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
            Some("addchunk") => {
                // the chunk tag comes from the postag attribute or the wd list
                let mut tags = raw_chunk_tags;
                if let Some(postag) = data.disambig.postag.as_ref() {
                    tags.push(postag.clone());
                }
                Ok(Disambiguation::AddChunk(tags))
            }
            Some("ignore_spelling") => Ok(Disambiguation::IgnoreSpelling),
            Some("immunize") => Ok(Disambiguation::Immunize),
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
                        structure::PatternPart::Example(_) => {}
            structure::PatternPart::Example(_) => {}
                    }
                }

                let disambiguations = if has_marker {
                    marker_disambig
                } else {
                    disambig
                };

                Ok(Disambiguation::Filter(
                    disambiguations.into_iter().collect(),
                    info.tagger().lang_options().retain_last,
                ))
            }
            Some("filter") => {
                if let Some(postag) = data.disambig.postag.as_ref() {
                    Ok(Disambiguation::Filter(
                        vec![Some(either::Right(parse_pos_filter(
                            postag,
                            Some("yes"),
                            info,
                        )))],
                        info.tagger().lang_options().retain_last,
                    ))
                } else {
                    Ok(Disambiguation::Filter(
                        word_datas.into_iter().map(Some).collect(),
                        info.tagger().lang_options().retain_last,
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
                        structure::PatternPart::Example(_) => {}
            structure::PatternPart::Example(_) => {}
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
                        vec![Some(either::Left(owned::WordData::new(
                            info.tagger.id_word("".into()).to_owned_id(),
                            info.tagger.id_tag(postag).to_owned_id(),
                        )))],
                        info.tagger().lang_options().retain_last,
                    ))
                } else {
                    Ok(Disambiguation::Filter(
                        word_datas.into_iter().map(Some).collect(),
                        info.tagger().lang_options().retain_last,
                    ))
                }
            }
            Some(x) => Err(Error::Unimplemented(format!("action {}", x))),
        }?;

        let filter = if let Some(filter_data) = data.filter {
            let args = filter_data
                .args
                .split(' ')
                .filter_map(|x| {
                    // tolerate arguments without a `key:value` shape
                    x.find(':').map(|idx| {
                        (
                            x[..idx].to_string(),
                            x[(idx + ':'.len_utf8())..].to_string(),
                        )
                    })
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
                        structure::ExamplePart::Text(text) => {
                            texts.push(text.as_str());
                            char_length += text.chars().count();
                        }
                        structure::ExamplePart::Marker(marker) => {
                            if char_span.is_some() {
                                // several markers: keep the first span,
                                // treat the rest as plain text
                                texts.push(marker.text.as_str());
                                char_length += marker.text.chars().count();
                                continue;
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
                    "ambiguous" => DisambiguationExample::Changed(DisambiguationChange {
                        text,
                        before: parse_tag_form(
                            example
                                .inputform
                                .as_ref()
                                .expect("must have inputform when ambiguous example"),
                            info,
                        )?,
                        after: parse_tag_form(
                            &example
                                .outputform
                                .as_ref()
                                .expect("must have inputform when ambiguous example"),
                            info,
                        )?,
                        char_span: char_span.expect("must have marker when ambiguous example"),
                    }),
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
                    element_filters,
                })
            },
            filter,
            disambiguations,
            examples,
            id: Index::default(),
        })
    }
}
