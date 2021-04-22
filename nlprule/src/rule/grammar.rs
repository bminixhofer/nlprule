use super::engine::composition::{GraphId, MatchGraph, MatchSentence, PosMatcher};
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
}

impl Conversion {
    fn convert(&self, input: &str) -> String {
        match &self {
            Conversion::Nop => input.to_string(),
            Conversion::AllLower => input.to_lowercase(),
            Conversion::StartLower => utils::apply_to_first(input, |c| c.to_lowercase().collect()),
            Conversion::AllUpper => input.to_uppercase(),
            Conversion::StartUpper => utils::apply_to_first(input, |c| c.to_uppercase().collect()),
        }
    }
}

/// An example associated with a [Rule][crate::rule::Rule].
#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Example {
    pub(crate) text: String,
    pub(crate) suggestion: Option<Suggestion>,
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

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct PosReplacer {
    pub(crate) matcher: PosMatcher,
}

impl PosReplacer {
    fn apply(&self, _text: &str, _sentence: &MatchSentence) -> Option<String> {
        // TODO: needs to be implemented with correct ordering, currently rules which would need this are disabled
        unimplemented!()
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Match {
    pub(crate) id: GraphId,
    pub(crate) conversion: Conversion,
    pub(crate) pos_replacer: Option<PosReplacer>,
    pub(crate) regex_replacer: Option<(Regex, String)>,
}

impl Match {
    fn apply(&self, sentence: &MatchSentence, graph: &MatchGraph) -> Option<String> {
        let text = graph.by_id(self.id).text(sentence);

        let mut text = if let Some(replacer) = &self.pos_replacer {
            replacer.apply(text, sentence)?
        } else {
            text.to_string()
        };

        text = if let Some((regex, replacement)) = &self.regex_replacer {
            regex.replace_all(&text, replacement)
        } else {
            text
        };

        // TODO: maybe return a vector here and propagate accordingly
        Some(self.conversion.convert(&text))
    }

    fn has_conversion(&self) -> bool {
        !matches!(self.conversion, Conversion::Nop)
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

impl Synthesizer {
    pub fn apply(
        &self,
        sentence: &MatchSentence,
        graph: &MatchGraph,
        start: GraphId,
        _end: GraphId,
    ) -> Option<String> {
        let mut output = Vec::new();

        let starts_with_conversion = match &self.parts[..] {
            [SynthesizerPart::Match(m), ..] => m.has_conversion(),
            _ => false,
        };

        for part in &self.parts {
            match part {
                SynthesizerPart::Text(t) => output.push(t.clone()),
                SynthesizerPart::Match(m) => {
                    output.push(m.apply(sentence, graph)?);
                }
            }
        }

        let suggestion = utils::normalize_whitespace(&output.join(""));

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

        if make_uppercase {
            Some(utils::apply_to_first(&suggestion, |x| {
                x.to_uppercase().collect()
            }))
        } else {
            Some(suggestion)
        }
    }
}
