//! Runtime post-filters applied to grammar rule matches before they become
//! suggestions. These mirror LanguageTool's `RuleFilter` classes which operate
//! on a fired rule match (adjusting its span, its replacements, or rejecting it).

use crate::rule::engine::composition::MatchSentence;
use crate::rule::filter_java::{FilterCtx, JavaFilter};
use crate::rule::synthesizer::Synthesizer;
use crate::rule::filter_data::FilterData;
use crate::tokenizer::tag::Tagger;
use crate::types::Span;
use crate::utils::regex::Regex;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum PostFilter {
    /// Mirrors `UnderlineSpacesFilter`: extends the match span over adjacent whitespace.
    UnderlineSpaces { mode: UnderlineMode },
    /// Mirrors `ApostropheTypeFilter`: only accept the match if the matched text
    /// contains (or does not contain) a typographic apostrophe (’).
    ApostropheType { has_typographic: bool },
    /// Mirrors `RegexAntiPatternFilter`: rejects the match if any regex matches
    /// the sentence text overlapping the match span.
    RegexAntiPattern { regexes: Vec<Regex> },
    /// Mirrors `AdaptSuggestionsFilter` (`Language.adaptSuggestion`).
    /// The base implementation is the identity; Catalan rewrites
    /// contractions and apostrophes.
    AdaptSuggestions { ca: bool },
    /// Mirrors `*SuppressMisspelledSuggestionsFilter`: drops replacements that
    /// are not in the analyzer dictionary (approximating LT's spellchecker check).
    /// If all replacements are dropped, the match is suppressed entirely
    /// (unless `suppress_match` is false, in which case the original
    /// replacements are kept).
    SuppressMisspelled { suppress_match: bool },
    /// A ported Java `RuleFilter` class (dates, multitoken speller,
    /// find-suggestions, advanced synthesizer, ...).
    Java(JavaFilter),
    /// Mirrors `SuppressIfAnyRuleMatchesFilter`: suppresses the match if,
    /// after applying any suggested replacement, one of the given rule IDs
    /// matches the re-analyzed sentence overlapping the original span.
    /// Needs the whole rule set and the tokenizer, so the actual check runs
    /// in `Rules::suggest`; at the per-rule stage this is a pass-through.
    SuppressIfAny { rule_ids: Vec<String> },
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnderlineMode {
    Before,
    After,
    Both,
}

const TYPOGRAPHIC_APOSTROPHE: char = '\u{2019}';

/// Result of applying a post-filter to a fired rule match.
pub struct FilteredMatch {
    pub span: Span,
    pub replacements: Vec<String>,
    /// If set, replaces the rule's expanded message.
    pub message: Option<String>,
}

impl PostFilter {
    /// Applies this filter. Returns `None` if the match should be rejected.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn apply(
        &self,
        sentence: &MatchSentence,
        span: Span,
        replacements: Vec<String>,
        matched_text: &str,
        message: &str,
        matched_tokens: &[usize],
        synth: Option<&Synthesizer>,
        filter_data: &FilterData,
    ) -> Option<FilteredMatch> {
        if let PostFilter::Java(filter) = self {
            let mut ctx = FilterCtx {
                sentence,
                tagger: sentence.tagger(),
                synth,
                data: filter_data,
                message: message.to_string(),
                span,
                replacements,
                matched_text: matched_text.to_string(),
                matched_tokens: matched_tokens.to_vec(),
                filter_args: None,
                filter_regex_args: None,
            };
            return filter.apply(&mut ctx).map(|outcome| FilteredMatch {
                span: outcome.span,
                replacements: outcome.replacements,
                message: outcome.message,
            });
        }

        match self {
            PostFilter::UnderlineSpaces { mode } => {
                let text = sentence.text();
                let mut start = span.byte().start;
                let mut end = span.byte().end;

                if matches!(mode, UnderlineMode::Before | UnderlineMode::Both) {
                    while start > 0
                        && text[..start].chars().last().map_or(false, char::is_whitespace)
                    {
                        start -= text[..start].chars().last().unwrap().len_utf8();
                    }
                }
                if matches!(mode, UnderlineMode::After | UnderlineMode::Both) {
                    while end < text.len()
                        && text[end..].chars().next().map_or(false, char::is_whitespace)
                    {
                        end += text[end..].chars().next().unwrap().len_utf8();
                    }
                }

                // keep the char span consistent: recompute by counting chars
                let out = Span::new(
                    start..end,
                    text[..start].chars().count()..text[..end].chars().count(),
                );

                Some(FilteredMatch {
                    span: out,
                    replacements,
                    message: None,
                })
            }
            PostFilter::ApostropheType {
                has_typographic,
            } => {
                let contains = matched_text.contains(TYPOGRAPHIC_APOSTROPHE);
                if contains == *has_typographic {
                    Some(FilteredMatch {
                        span,
                        replacements,
                        message: None,
                    })
                } else {
                    None
                }
            }
            PostFilter::RegexAntiPattern { regexes } => {
                let text = sentence.text();
                let (from, to) = (span.byte().start, span.byte().end);

                for regex in regexes {
                    for mat in regex.find_iter(text) {
                        let (mstart, mend) = (mat.start(), mat.end());
                        if (mstart <= to && mend >= to) || (mstart <= from && mend >= from) {
                            return None;
                        }
                    }
                }

                Some(FilteredMatch {
                    span,
                    replacements,
                    message: None,
                })
            }
            PostFilter::AdaptSuggestions { ca } => {
                let replacements = if *ca {
                    replacements
                        .into_iter()
                        .map(|replacement| {
                            crate::rule::filter_java::ca_adapt_suggestion(&replacement)
                        })
                        .collect()
                } else {
                    replacements
                };

                Some(FilteredMatch {
                    span,
                    replacements,
                    message: None,
                })
            }
            PostFilter::SuppressMisspelled { suppress_match } => {
                let tagger = sentence.tagger();

                let known: Vec<String> = replacements
                    .iter()
                    .filter(|replacement| is_known_word(replacement, tagger))
                    .cloned()
                    .collect();

                if known.is_empty() {
                    if *suppress_match {
                        None
                    } else {
                        Some(FilteredMatch {
                            span,
                            replacements,
                            message: None,
                        })
                    }
                } else {
                    Some(FilteredMatch {
                        span,
                        replacements: known,
                        message: None,
                    })
                }
            }
            PostFilter::Java(_) => unreachable!("handled above"),
            PostFilter::SuppressIfAny { .. } => Some(FilteredMatch {
                span,
                replacements,
                message: None,
            }),
        }
    }
}

/// Copies the case of `sample` onto `input` (all-upper, first-upper or as-is),
/// mirroring `Language.adaptSuggestion`'s case adaptation.
fn adapt_case(input: &str, sample: &str) -> String {
    let first_upper = sample.chars().next().map_or(false, |c| c.is_uppercase());

    if first_upper {
        let has_lower = sample.chars().any(|c| c.is_lowercase());
        let char_count = sample.chars().count();

        if char_count > 1 && !has_lower {
            input.to_uppercase()
        } else {
            crate::utils::apply_to_first(input, |c| c.to_uppercase().collect())
        }
    } else {
        input.to_string()
    }
}

/// Whether a replacement is considered correctly spelled:
/// every alphabetic part is in the analyzer dictionary (as-is, lowercased
/// or title-cased). Approximates LT's spellchecker-based check.
fn is_known_word(replacement: &str, tagger: &Tagger) -> bool {
    let parts: Vec<&str> = replacement
        .split(|c: char| {
            c.is_whitespace() || crate::utils::splitting_chars().chars().any(|s| s == c)
        })
        .filter(|p| p.chars().any(char::is_alphabetic))
        .collect();

    if parts.is_empty() {
        return true;
    }

    parts.iter().all(|part| {
        tagger.id_word((*part).into()).1.is_some()
            || tagger.id_word(part.to_lowercase().into()).1.is_some()
            || tagger.id_word(part.to_uppercase().into()).1.is_some()
    })
}
