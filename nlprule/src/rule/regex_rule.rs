//! Rule-level `<regexp>` rules (LT's regex-on-sentence-text rules, e.g. de
//! `GENANT_SPELLING_RULE`, gl `UNITS_OF_MEASURE_SPACING`). The regex runs
//! against the raw sentence text (not tokens); each match becomes a
//! suggestion whose span is the whole match. `\N` backrefs and
//! `<match no="N"/>` elements refer to capture groups.

use crate::types::{Position, Span, Suggestion};
use crate::utils::regex::Regex;
use serde::{Deserialize, Serialize};

/// A rule-level regexp rule as compiled into the rules binary.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RegexRuleDef {
    /// Suggestion source id, `"CATEGORY/RULE_ID[/n]"` like pattern rules.
    pub source: String,
    /// The pattern, already converted from the Java dialect (and `(?i)`
    /// prefixed unless the rule is `case_sensitive="yes"`).
    pub regex: Regex,
    /// Replacement alternatives: the parts of each `<suggestion>`.
    pub suggestions: Vec<Vec<RegexSugPart>>,
    pub message: String,
    pub enabled: bool,
}

/// One part of a `<suggestion>` element.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RegexSugPart {
    /// Literal text.
    Lit(String),
    /// `\N` backref or plain `<match no="N"/>`: the text of capture group N.
    Group(usize),
    /// `<match no="N" regexp_match=".." regexp_replace=".."/>`: group N's
    /// text with the first inner-regex match replaced.
    GroupMatch {
        group: usize,
        regex: Regex,
        replace: Vec<ReplacePart>,
    },
}

/// `$k` backref or literal in a `regexp_replace`.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ReplacePart {
    Lit(String),
    Ref(usize),
}

impl RegexRuleDef {
    /// Run the rule over a sentence text: one suggestion per regex match.
    pub(crate) fn apply(&self, text: &str) -> Vec<Suggestion> {
        let mut out = Vec::new();
        for caps in self.regex.captures_iter(text) {
            let whole = match caps.get(0) {
                Some(m) => m,
                None => continue,
            };
            // never emit empty-span suggestions (a pattern that can match
            // empty would otherwise fire at every position)
            if whole.as_str().is_empty() {
                continue;
            }
            let group_text =
                |n: usize| caps.get(n).map(|m| m.as_str()).unwrap_or_default().to_string();

            let mut replacements = Vec::with_capacity(self.suggestions.len());
            for parts in &self.suggestions {
                let mut rendered = String::new();
                for part in parts {
                    match part {
                        RegexSugPart::Lit(s) => rendered.push_str(s),
                        RegexSugPart::Group(n) => rendered.push_str(&group_text(*n)),
                        RegexSugPart::GroupMatch {
                            group,
                            regex,
                            replace,
                        } => {
                            let source_text = group_text(*group);
                            if let Some(c) = regex.captures(&source_text) {
                                for part in replace {
                                    match part {
                                        ReplacePart::Lit(s) => rendered.push_str(s),
                                        ReplacePart::Ref(n) => rendered.push_str(
                                            &c.get(*n)
                                                .map(|m| m.as_str())
                                                .unwrap_or_default(),
                                        ),
                                    }
                                }
                            }
                        }
                    }
                }
                replacements.push(rendered);
            }

            let char_start = text[..whole.start()].chars().count();
            let span = Span::from_positions(
                Position {
                    byte: whole.start(),
                    char: char_start,
                },
                Position {
                    byte: whole.end(),
                    char: char_start + whole.as_str().chars().count(),
                },
            );
            out.push(Suggestion::new(
                self.source.clone(),
                self.message.clone(),
                span,
                replacements,
            ));
        }
        out
    }
}
