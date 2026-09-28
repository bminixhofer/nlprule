use super::engine::composition::{GraphId, MatchGraph, MatchSentence};
use super::synthesizer::Synthesizer as MorphSynthesizer;
use crate::types::*;
use crate::utils::{self, regex::Regex};
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum Conversion {
    Nop,
    AllLower,
    StartLower,
    AllUpper,
    StartUpper,
    /// Copy the case of a sample string.
    Preserve,
    /// Lowercase everything, then uppercase the first char.
    FirstUpper,
    /// Remove Arabic diacritics (tashkeel).
    NotAshkeel,
}

impl Conversion {
    /// Converts the case of `input` using `sample` as the case sample
    /// (only used by `Preserve`). Mirrors `CaseConversionHelper.convertCase`.
    fn convert(&self, input: &str, sample: &str) -> String {
        if input.is_empty() {
            return input.to_string();
        }
        match &self {
            Conversion::Nop => input.to_string(),
            Conversion::AllLower => input.to_lowercase(),
            Conversion::StartLower => utils::apply_to_first(input, |c| c.to_lowercase().collect()),
            Conversion::AllUpper => input.to_uppercase(),
            Conversion::StartUpper => utils::apply_to_first(input, |c| c.to_uppercase().collect()),
            Conversion::Preserve => {
                let starts_upper = sample.chars().next().map_or(false, |c| c.is_uppercase());

                if starts_upper {
                    // StringTools.isAllUppercase: all cased chars are uppercase and length > 1
                    let has_lowercase = sample.chars().any(|c| c.is_lowercase());
                    let len = sample.chars().count();

                    if len > 1 && !has_lowercase {
                        input.to_uppercase()
                    } else {
                        utils::apply_to_first(input, |c| c.to_uppercase().collect())
                    }
                } else {
                    input.to_string()
                }
            }
            Conversion::FirstUpper => {
                utils::apply_to_first(&input.to_lowercase(), |c| c.to_uppercase().collect())
            }
            Conversion::NotAshkeel => strip_tashkeel(input),
        }
    }
}

fn strip_tashkeel(input: &str) -> String {
    // Arabic diacritics (tashkeel) Unicode block
    input
        .chars()
        .filter(|c| !('\u{064B}'..='\u{0652}').contains(c))
        .collect()
}

/// An example associated with a [Rule][crate::rule::Rule].
#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Example {
    pub(crate) text: String,
    pub(crate) suggestion: Option<Suggestion>,
    /// The `type` attribute: 'triggers_error' examples expect the rule to match
    /// (without checking the replacement).
    pub(crate) kind: Option<String>,
}

impl Example {
    /// Gets the text of this example.
    pub fn text(&self) -> &str {
        &self.text
    }

    /// Gets the suggestion for this example.
    /// * If this is `None`, the associated rule should not trigger for this example.
    /// * If it is `Some`, the associated rule should return a suggestion with equivalent range and suggestions.
    pub fn suggestion(&self) -> Option<&Suggestion> {
        self.suggestion.as_ref()
    }
}

/// A POS tag (or POS tag regular expression) a [Match] synthesizes forms for.
/// Mirrors the `postag`, `postag_regexp` and `postag_replace` attributes
/// of LanguageTool's `match` element.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum PosTagSelector {
    /// An exact POS tag.
    Exact(String),
    /// A regular expression over POS tags (full match), with an optional replacement
    /// applied to matched tags (Java `replaceAll` semantics).
    Regex {
        regex: Regex,
        /// The raw postag string; used as the synthesis target when no reading
        /// matches (mirroring `MatchState.getTargetPosTag`'s empty fallback).
        raw: String,
        replace: Option<String>,
    },
}

impl PosTagSelector {
    /// Mirrors `MatchState.getTargetPosTag`: computes the POS tag used as the
    /// synthesis target from the POS tags of the matched token's readings.
    fn target_pos_tag(
        &self,
        pos_tags: &[String],
        synth: &MorphSynthesizer,
        static_lemma: bool,
    ) -> String {
        match self {
            PosTagSelector::Exact(tag) => tag.clone(),
            PosTagSelector::Regex {
                regex,
                raw,
                replace,
            } => {
                let matching: Vec<&str> = pos_tags
                    .iter()
                    .filter(|tag| regex.is_match(tag))
                    .map(|tag| tag.as_str())
                    .collect();

                if matching.is_empty() {
                    // mirror Java: the raw postag string is used as the target,
                    // later interpreted as a regular expression for the synthesis
                    return raw.clone();
                }

                let target = synth.get_target_pos_tag(&matching, "");

                match replace {
                    Some(replace) if static_lemma => {
                        // only the (last) target tag is replaced
                        regex.replace_all(&target, replace)
                    }
                    Some(replace) => {
                        // each matched tag is replaced, joined as an alternation
                        matching
                            .iter()
                            .map(|tag| regex.replace_all(tag, replace))
                            .collect::<Vec<_>>()
                            .join("|")
                    }
                    _ => target,
                }
            }
        }
    }
}

/// Mirrors LanguageTool's `Match` (the XML `match` element inside suggestions and messages):
/// formats a matched token into the string(s) used in a suggestion.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Match {
    pub(crate) id: GraphId,
    pub(crate) conversion: Conversion,
    pub(crate) regex_replacer: Option<(Regex, String)>,
    /// `postag` (+ `postag_regexp`, `postag_replace`): synthesize a form of the matched
    /// token with the target POS tag.
    pub(crate) postag: Option<PosTagSelector>,
    /// Element content used as a static lemma which is synthesized
    /// (e.g. `<match no="1" postag="VBN">word</match>`).
    pub(crate) static_lemma: Option<String>,
    /// The `text` attribute: a literal replacement text.
    pub(crate) text: Option<String>,
    /// Whether skipped tokens (between this element and the next pattern element) are
    /// appended to the output (`include_skipped="all"`), or whether *only* the skipped
    /// tokens are output (`include_skipped="following"`).
    pub(crate) include_skipped: Option<IncludeSkipped>,
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq)]
pub enum IncludeSkipped {
    /// Output only the skipped tokens.
    Following,
    /// Output the token text followed by the skipped tokens.
    All,
}

/// POS tags marking special pseudo-readings. Synthesis is skipped for these
/// (mirroring `toFinalString`'s `oneForm` handling).
const SPECIAL_TAGS: [&str; 3] = ["SENT_START", "SENT_END", "PARAGRAPH_END"];


// ---------------------------------------------------------------------------
// English a/an determiner synthesis (LT `EnglishSynthesizer`'s +DT / +INDT tags)
// ---------------------------------------------------------------------------

mod avsan {
    use once_cell::sync::Lazy;
    use std::collections::HashSet;

    struct Sets {
        requiring_a: HashSet<String>,
        requiring_an: HashSet<String>,
    }

    static SETS: Lazy<Sets> = Lazy::new(|| {
        fn load(text: &'static str) -> HashSet<String> {
            let mut set = HashSet::new();
            for line in text.lines() {
                let line = line.trim();
                if line.is_empty() || line.starts_with('#') {
                    continue;
                }
                if let Some(word) = line.strip_prefix('*') {
                    // case-sensitive entry
                    set.insert(word.trim().to_string());
                } else {
                    set.insert(line.to_lowercase());
                }
            }
            set
        }
        Sets {
            requiring_a: load(include_str!("../../configs/en/det_a.txt")),
            requiring_an: load(include_str!("../../configs/en/det_an.txt")),
        }
    });

    fn lowercase_first_if_capitalized(word: &str) -> String {
        let mut chars = word.chars();
        match chars.next() {
            Some(c) if c.is_uppercase() => {
                let rest: String = chars.collect();
                // only lowercase when the word is not all-uppercase
                if !rest.chars().all(|x| !x.is_lowercase()) {
                    c.to_lowercase().collect::<String>() + &rest
                } else {
                    word.to_string()
                }
            }
            _ => word.to_string(),
        }
    }

    /// Mirrors `AvsAnRule.suggestAorAn`: returns "a word" / "an word", or the
    /// plain word when the determiner is unknown.
    pub(crate) fn lowercase_first_pub(word: &str) -> String {
        lowercase_first_if_capitalized(word)
    }

    pub(crate) fn suggest_a_or_an(orig_word: &str) -> String {
        let mut word = orig_word.to_string();
        let parts: Vec<&str> = word
            .split(|c: char| matches!(c, '-' | '"' | '\u{201C}' | '\'' | '\u{2018}' | '(' | ')' | '[' | ']'))
            .collect();
        if let Some(first) = parts.first() {
            if !first.eq_ignore_ascii_case("a") {
                word = first.to_string();
            }
        }
        let cleaned: String = word
            .chars()
            .filter(|c| c.is_ascii_alphanumeric() || "α.;,:'".contains(*c))
            .collect();
        if !cleaned.is_empty() {
            word = cleaned;
        } else {
            return orig_word.to_string();
        }

        let in_a = SETS.requiring_a.contains(&word.to_lowercase())
            || SETS.requiring_a.contains(&word);
        let in_an = SETS.requiring_an.contains(&word.to_lowercase())
            || SETS.requiring_an.contains(&word);

        let use_a = if in_a && in_an {
            true // A_OR_AN -> "a"
        } else if in_a {
            true
        } else if in_an {
            false
        } else {
            let first = word.chars().next().unwrap_or('a');
            let is_vowel = matches!(
                first.to_lowercase().next(),
                Some('a') | Some('e') | Some('i') | Some('o') | Some('u')
            );
            let all_upper =
                word.chars().any(char::is_alphabetic) && word.chars().all(|c| !c.is_lowercase());
            let mixed = word.chars().any(char::is_uppercase)
                && word.chars().any(char::is_lowercase);
            if all_upper || mixed {
                return orig_word.to_string();
            }
            !is_vowel
        };

        if use_a {
            format!("a {}", lowercase_first_if_capitalized(orig_word))
        } else {
            format!("an {}", lowercase_first_if_capitalized(orig_word))
        }
    }
}

impl Match {
    /// Formats the referenced token into the string(s) to use in a suggestion.
    /// Mirrors `MatchState.toFinalString`. Returns possibly multiple forms
    /// (synthesis can produce more than one word form).
    pub(crate) fn apply(
        &self,
        sentence: &MatchSentence,
        graph: &MatchGraph,
        synth: Option<&MorphSynthesizer>,
    ) -> Vec<String> {
        let token_text = graph.by_id(self.id).text(sentence).to_string();

        // the base text: a literal text, the static lemma or the token text
        let original = match (&self.text, &self.static_lemma) {
            (Some(text), _) => text.clone(),
            (None, Some(lemma)) => lemma.clone(),
            (None, None) => token_text.clone(),
        };

        let mut forms = vec![original.clone()];

        if let Some((regex, replacement)) = &self.regex_replacer {
            forms = forms
                .into_iter()
                .map(|form| regex.replace_all(&form, replacement))
                .collect();
        }

        if let Some(postag) = &self.postag {
            // EnglishSynthesizer's special determiner tags
            match postag {
                PosTagSelector::Exact(tag) if tag == "+DT" => {
                    let word = forms.first().cloned().unwrap_or_default();
                    return vec![
                        avsan::suggest_a_or_an(&word),
                        format!("the {}", avsan::lowercase_first_pub(&word)),
                    ];
                }
                PosTagSelector::Exact(tag) if tag == "+INDT" => {
                    let word = forms.first().cloned().unwrap_or_default();
                    return vec![avsan::suggest_a_or_an(&word)];
                }
                PosTagSelector::Regex { raw, .. } if raw.ends_with("\\+DT") => {
                    if let Some(synth) = synth {
                        let target = raw.trim_end_matches("\\+DT").to_string();
                        let re = crate::utils::regex::Regex::new(target);
                        let mut out = Vec::new();
                        let readings: Vec<String> = graph
                            .by_id(self.id)
                            .tokens(sentence)
                            .flat_map(|token| {
                                token
                                    .word()
                                    .tags()
                                    .iter()
                                    .map(|tag| tag.lemma().as_str().to_string())
                            })
                            .collect();
                        for lemma in readings {
                            for form in synth.synthesize_regex(&lemma, &re) {
                                let prefixed = format!("the {}", form);
                                if !out.contains(&prefixed) {
                                    out.push(prefixed);
                                }
                            }
                        }
                        if !out.is_empty() {
                            return out;
                        }
                    }
                }
                PosTagSelector::Regex { raw, .. } if raw.ends_with("\\+INDT") => {
                    if let Some(synth) = synth {
                        let target = raw.trim_end_matches("\\+INDT").to_string();
                        let re = crate::utils::regex::Regex::new(target);
                        let mut out = Vec::new();
                        let readings: Vec<String> = graph
                            .by_id(self.id)
                            .tokens(sentence)
                            .flat_map(|token| {
                                token
                                    .word()
                                    .tags()
                                    .iter()
                                    .map(|tag| tag.lemma().as_str().to_string())
                            })
                            .collect();
                        for lemma in readings {
                            for form in synth.synthesize_regex(&lemma, &re) {
                                let prefixed = avsan::suggest_a_or_an(&form);
                                if !out.contains(&prefixed) {
                                    out.push(prefixed);
                                }
                            }
                        }
                        if !out.is_empty() {
                            return out;
                        }
                    }
                }
                _ => {}
            }

            if let Some(synth) = synth {
                // the POS tags of the matched token's (real) readings
                let pos_tags: Vec<String> = graph
                    .by_id(self.id)
                    .tokens(sentence)
                    .flat_map(|token| {
                        token
                            .word()
                            .tags()
                            .iter()
                            .map(|tag| tag.pos().as_str().to_string())
                    })
                    .filter(|pos| !SPECIAL_TAGS.contains(&pos.as_str()))
                    .collect();

                // the readings used for synthesis: the static lemma (single pseudo-reading),
                // or the token's readings
                let synth_readings: Vec<String> = match &self.static_lemma {
                    Some(lemma) => vec![lemma.clone()],
                    None => graph
                        .by_id(self.id)
                        .tokens(sentence)
                        .flat_map(|token| {
                            token
                                .word()
                                .tags()
                                .iter()
                                .map(|tag| tag.lemma().as_str().to_string())
                        })
                        .collect(),
                };

                let mut word_forms: Vec<String> = Vec::new();

                match postag {
                    PosTagSelector::Exact(tag) => {
                        let mut set: Vec<String> = synth_readings
                            .iter()
                            .flat_map(|lemma| synth.lookup(lemma, tag))
                            .collect();
                        set.sort();
                        set.dedup();
                        word_forms = set;
                    }
                    PosTagSelector::Regex { .. } => {
                        {
                            let target = postag.target_pos_tag(
                                &pos_tags,
                                synth,
                                self.static_lemma.is_some(),
                            );
                            if !target.is_empty() {
                                // the target tag is used as a regular expression itself,
                                // mirroring synthesize(token, target, true).
                                // it can contain syntax not valid for the regex backend
                                // (e.g. leftover backreferences); fall back instead of panicking
                                let regex = Regex::new(format!("^(?:{})$", target));
                                if regex.try_compile().is_ok() {
                                    let mut set: Vec<String> = synth_readings
                                        .iter()
                                        .flat_map(|lemma| synth.synthesize_regex(lemma, &regex))
                                        .collect();
                                    set.sort();
                                    set.dedup();
                                    word_forms = set;
                                }
                            }
                        }

                        if word_forms.is_empty() {
                            // no form could be synthesized: LT marks this by parenthesizing the token
                            word_forms = vec![format!("({})", token_text)];
                        }
                    }
                }

                if word_forms.is_empty() {
                    // exact synthesis found nothing: fall back to the plain token text
                    word_forms = vec![token_text.clone()];
                }

                forms = word_forms;
            }
        }

        // apply the case conversion using the original text as the sample
        forms = forms
            .into_iter()
            .map(|form| self.conversion.convert(&form, &original))
            .collect();

        // append skipped tokens if requested
        if self.include_skipped.is_some() {
            let skipped = skipped_tokens_text(self.id, sentence, graph);

            if !skipped.is_empty() {
                forms = forms
                    .into_iter()
                    .map(|form| match self.include_skipped {
                        Some(IncludeSkipped::Following) => skipped.clone(),
                        _ => format!("{}{}", form, skipped),
                    })
                    .collect();
            }
        }

        forms
    }

    fn has_conversion(&self) -> bool {
        !matches!(self.conversion, Conversion::Nop)
    }
}

/// Gets the text of the tokens skipped between the graph element `id` and the next element.
fn skipped_tokens_text(id: GraphId, sentence: &MatchSentence, graph: &MatchGraph) -> String {
    let index = graph.get_index(id);

    let this_end = graph
        .by_index(index)
        .tokens(sentence)
        .last()
        .map(|token| token.span().end());

    let next_start = graph
        .groups()
        .get(index + 1)
        .and_then(|group| group.tokens(sentence).next())
        .map(|token| token.span().start());

    match (this_end, next_start) {
        (Some(end), Some(start)) if start > end => {
            sentence.slice(Span::from_positions(end, start)).to_string()
        }
        _ => String::new(),
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum SynthesizerPart {
    Text(String),
    // Regex with the `fancy_regex` backend is large on the stack
    Match(Box<Match>),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Synthesizer {
    pub(crate) use_titlecase_adjust: bool,
    pub(crate) parts: Vec<SynthesizerPart>,
}

/// Maximum number of replacements generated from one suggestion element.
/// Guards against combinatorial explosion when multiple matches synthesize
/// several word forms each.
const MAX_REPLACEMENTS: usize = 40;

impl Synthesizer {
    /// Applies this synthesizer: builds the replacement strings by concatenating its parts.
    /// Each `match` part may expand to multiple word forms, in which case all
    /// combinations are generated (mirroring how LanguageTool expands suggestions).
    pub fn apply(
        &self,
        sentence: &MatchSentence,
        graph: &MatchGraph,
        start: GraphId,
        _end: GraphId,
        synth: Option<&MorphSynthesizer>,
    ) -> Vec<String> {
        let mut outputs: Vec<String> = vec![String::new()];

        for part in &self.parts {
            match part {
                SynthesizerPart::Text(t) => {
                    for output in outputs.iter_mut() {
                        output.push_str(t);
                    }
                }
                SynthesizerPart::Match(m) => {
                    let forms = m.apply(sentence, graph, synth);

                    if forms.is_empty() {
                        return Vec::new();
                    }

                    let mut next: Vec<String> = Vec::with_capacity(outputs.len() * forms.len());

                    'outer: for output in &outputs {
                        for form in &forms {
                            let mut combined = output.clone();
                            combined.push_str(form);
                            next.push(combined);

                            if next.len() >= MAX_REPLACEMENTS {
                                break 'outer;
                            }
                        }
                    }

                    outputs = next;
                }
            }
        }

        let starts_with_conversion = match &self.parts[..] {
            [SynthesizerPart::Match(m), ..] => m.has_conversion(),
            _ => false,
        };

        // if the suggestion does not start with a case conversion match, make it title case if:
        // * at sentence start
        // * the replaced text is title case
        let make_uppercase = !starts_with_conversion
            && graph.groups()[graph.get_index(start)..]
                .iter()
                .find_map(|x| x.tokens(sentence).next())
                .map(|first_token| {
                    (self.use_titlecase_adjust
                        && first_token
                            .word()
                            .as_str()
                            .chars()
                            .next() // a word is expected to always have at least one char, but be defensive here
                            .map_or(false, char::is_uppercase))
                        || first_token.span().start() == sentence.span().start()
                })
                .unwrap_or(false);

        let outputs: Vec<String> = outputs
            .into_iter()
            .map(|x| utils::normalize_whitespace(&x))
            .collect();

        if make_uppercase {
            outputs
                .into_iter()
                .map(|x| utils::apply_to_first(&x, |c| c.to_uppercase().collect()))
                .collect()
        } else {
            outputs
        }
    }
}
