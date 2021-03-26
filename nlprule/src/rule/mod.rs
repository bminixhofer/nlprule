//! Implementations related to single rules.

use crate::types::*;
use crate::{
    filter::{Filter, Filterable},
    tokenizer::Tokenizer,
    utils,
};
use itertools::Itertools;
use log::{error, info, warn};
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::fmt;

pub(crate) mod disambiguation;
pub(crate) mod engine;
pub(crate) mod grammar;
pub mod id;

use engine::Engine;

pub(crate) use engine::composition::MatchGraph;
pub use grammar::Example;

use self::{
    disambiguation::POSFilter,
    engine::{composition::GraphId, EngineMatches},
    id::Index,
};

/// A *Unification* makes an otherwise matching pattern invalid if no combination of its filters
/// matches all tokens marked with "unify".
/// Can also be negated.
#[derive(Serialize, Deserialize, Debug)]
pub(crate) struct Unification {
    pub(crate) mask: Vec<Option<bool>>,
    pub(crate) filters: Vec<Vec<POSFilter>>,
}

impl Unification {
    pub fn keep(&self, graph: &MatchGraph, sentence: &MatchSentence) -> bool {
        let filters: Vec<_> = self.filters.iter().multi_cartesian_product().collect();

        let mut filter_mask: Vec<_> = filters.iter().map(|_| true).collect();
        let negate = self.mask.iter().all(|x| x.map_or(true, |x| !x));

        for (group, maybe_mask_val) in graph.groups()[1..].iter().zip(self.mask.iter()) {
            if maybe_mask_val.is_some() {
                for token in group.tokens(sentence) {
                    for (mask_val, filter) in filter_mask.iter_mut().zip(filters.iter()) {
                        *mask_val = *mask_val && POSFilter::and(filter, &token.word);
                    }
                }
            }
        }

        let result = filter_mask.iter().any(|x| *x);
        if negate {
            !result
        } else {
            result
        }
    }
}

/// A disambiguation rule.
/// Changes the information associcated with one or more tokens if it matches.
/// Sourced from LanguageTool. An example of how a simple rule might look in the original XML format:
///
/// ```xml
/// <rule id="NODT_HAVE" name="no determiner + have as verb/noun ->have/verb">
///    <pattern>
///        <token>
///             <exception postag="PRP$"></exception>
///             <exception regexp="yes">the|a</exception>
///        </token>
///        <marker>
///            <token case_sensitive="yes" regexp="yes">[Hh]ave|HAVE</token>
///        </marker>
///    </pattern>
///    <disambig action="replace"><wd lemma="have" pos="VB"></wd></disambig>
/// </rule>
/// ```
#[derive(Serialize, Deserialize)]
pub struct DisambiguationRule {
    pub(crate) id: Index,
    pub(crate) engine: Engine,
    pub(crate) disambiguations: disambiguation::Disambiguation,
    pub(crate) filter: Option<Filter>,
    pub(crate) start: GraphId,
    pub(crate) end: GraphId,
    pub(crate) examples: Vec<disambiguation::DisambiguationExample>,
    pub(crate) unification: Option<Unification>,
}

#[derive(Default)]
pub(crate) struct Changes(Vec<Vec<HashSet<(usize, usize)>>>);

impl Changes {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl DisambiguationRule {
    /// Get a unique identifier of this rule.
    pub fn id(&self) -> &Index {
        &self.id
    }

    pub(crate) fn apply<'t>(
        &'t self,
        tokens: &MatchSentence<'t>,
        tokenizer: &Tokenizer,
    ) -> Changes {
        if matches!(self.disambiguations, disambiguation::Disambiguation::Nop) {
            return Changes::default();
        }

        let mut all_byte_spans = Vec::new();

        for graph in self.engine.get_matches(tokens, self.start, self.end) {
            if let Some(unification) = &self.unification {
                if !unification.keep(&graph, tokens) {
                    continue;
                }
            }

            if let Some(filter) = &self.filter {
                if !filter.keep(&graph, tokenizer) {
                    continue;
                }
            }

            let mut byte_spans = Vec::new();

            for group_idx in GraphId::range(&self.start, &self.end) {
                let group = graph.by_id(group_idx);

                let group_byte_spans: HashSet<_> = group
                    .tokens(graph.sentence())
                    .map(|x| x.byte_span)
                    .collect();

                byte_spans.push(group_byte_spans);
            }

            all_byte_spans.push(byte_spans);
        }

        Changes(all_byte_spans)
    }

    pub(crate) fn change<'t>(
        &'t self,
        sentence: &mut IncompleteSentence<'t>,
        tokenizer: &'t Tokenizer,
        changes: Changes,
    ) {
        log::info!("applying {}", self.id);

        for byte_spans in changes.0 {
            let mut groups = Vec::new();
            let mut refs = sentence.iter_mut().collect::<Vec<_>>();

            for group_byte_spans in byte_spans {
                let mut group = Vec::new();

                while let Some(i) = refs
                    .iter()
                    .position(|x| group_byte_spans.contains(&x.byte_span))
                {
                    group.push(refs.remove(i));
                }

                groups.push(group);
            }

            self.disambiguations.apply(
                groups,
                tokenizer.lang_options().retain_last,
                tokenizer.tagger(),
            );
        }
    }

    /// Often there are examples associated with a rule.
    /// This method checks whether the correct action is taken in the examples.
    pub fn test(&self, tokenizer: &Tokenizer) -> bool {
        let mut passes = Vec::new();

        for (i, test) in self.examples.iter().enumerate() {
            let text = match test {
                disambiguation::DisambiguationExample::Unchanged(x) => x.as_str(),
                disambiguation::DisambiguationExample::Changed(x) => x.text.as_str(),
            };

            // by convention examples are always considered as one sentence even if the sentencizer would split
            let sentence_before =
                tokenizer.disambiguate_up_to_id(tokenizer.tokenize(text), Some(&self.id));
            let sentence_before_complete = Sentence::new(sentence_before.clone());
            let changes = self.apply(&MatchSentence::new(&sentence_before_complete), tokenizer);

            let mut sentence_after = sentence_before.clone();

            if !changes.is_empty() {
                self.change(&mut sentence_after, tokenizer, changes);
            }

            info!("Tokens: {:#?}", sentence_before);

            let pass = match test {
                disambiguation::DisambiguationExample::Unchanged(_) => {
                    sentence_before == sentence_after
                }
                disambiguation::DisambiguationExample::Changed(change) => {
                    let _before = sentence_before
                        .iter()
                        .find(|x| x.char_span == change.char_span)
                        .unwrap();

                    let after = sentence_after
                        .iter()
                        .find(|x| x.char_span == change.char_span)
                        .unwrap();

                    let unordered_tags = after
                        .word
                        .tags
                        .iter()
                        .map(|x| x.to_owned_word_data())
                        .collect::<HashSet<owned::WordData>>();
                    // need references to compare
                    let unordered_tags: HashSet<_> = unordered_tags.iter().collect();
                    let unordered_tags_change = change
                        .after
                        .tags
                        .iter()
                        .collect::<HashSet<&owned::WordData>>();

                    after.word.text == change.after.text.as_ref_id()
                        && unordered_tags == unordered_tags_change
                }
            };

            if !pass {
                let error_str = format!(
                    "Rule {}: Test \"{:#?}\" failed. Before: {:#?}. After: {:#?}.",
                    self.id, test, sentence_before, sentence_after,
                );

                if tokenizer
                    .lang_options()
                    .known_failures
                    .contains(&format!("{}:{}", self.id, i))
                {
                    warn!("{}", error_str)
                } else {
                    error!("{}", error_str)
                }
            }

            passes.push(pass);
        }

        passes.iter().all(|x| *x)
    }
}

/// An iterator over [Suggestion][crate::types::Suggestion]s.
pub struct Suggestions<'a, 't> {
    rule: &'a Rule,
    tokenizer: &'a Tokenizer,
    matches: EngineMatches<'a, 't>,
    sentence: &'t MatchSentence<'t>,
}

impl<'a, 't> Iterator for Suggestions<'a, 't> {
    type Item = Suggestion;

    fn next(&mut self) -> Option<Self::Item> {
        let rule = self.rule;
        let tokenizer = self.tokenizer;
        let sentence = self.sentence;
        let (start, end) = (self.rule.start, self.rule.end);

        self.matches.find_map(|graph| {
            if let Some(unification) = &rule.unification {
                if !unification.keep(&graph, sentence) {
                    return None;
                }
            }

            let start_group = graph.by_id(start);
            let end_group = graph.by_id(end);

            let replacements: Vec<String> = rule
                .suggesters
                .iter()
                .filter_map(|x| x.apply(&graph, tokenizer, start, end))
                .collect();

            let start = if replacements
                .iter()
                .all(|x| utils::no_space_chars().chars().any(|c| x.starts_with(c)))
            {
                let first_token = graph.groups()[graph.get_index(start)..]
                    .iter()
                    .find_map(|x| x.tokens(graph.sentence()).next())
                    .unwrap();

                let idx = sentence
                    .iter()
                    .position(|x| std::ptr::eq(x, first_token))
                    .unwrap_or(0);

                if idx > 0 {
                    sentence.index(idx - 1).char_span.1
                } else {
                    start_group.char_span.0
                }
            } else {
                start_group.char_span.0
            };
            let end = end_group.char_span.1;

            // this should never happen, but just return None instead of raising an Error
            // `end` COULD be equal to `start` if the suggestion is to insert text at this position
            if end < start {
                return None;
            }
            let text_before: String = sentence
                .text()
                .chars()
                .skip(start)
                .take(end - start)
                .collect();

            // fix e. g. "Super , dass"
            let replacements: Vec<String> = replacements
                .into_iter()
                .filter(|suggestion| *suggestion != text_before)
                .map(|x| utils::fix_nospace_chars(&x))
                .collect();

            if !replacements.is_empty() {
                Some(Suggestion {
                    message: rule
                        .message
                        .apply(&graph, tokenizer, rule.start, rule.end)
                        .expect("Rules must have a message."),
                    source: rule.id.to_string(),
                    start,
                    end,
                    replacements,
                })
            } else {
                None
            }
        })
    }
}

/// A grammar rule.
/// Returns a [Suggestion][crate::types::Suggestion] for change if it matches.
/// Sourced from LanguageTool. An example of how a simple rule might look in the original XML format:
///
/// ```xml
/// <rule id="DOSNT" name="he dosn't (doesn't)">
///     <pattern>
///         <token regexp="yes">do[se]n|does|dosan|doasn|dosen</token>
///         <token regexp="yes">['’`´‘]</token>
///         <token>t</token>
///     </pattern>
///     <message>Did you mean <suggestion>doesn\2t</suggestion>?</message>
///     <example correction="doesn't">He <marker>dosn't</marker> know about it.</example>
/// </rule>
/// ```
#[derive(Serialize, Deserialize, Debug)]
pub struct Rule {
    pub(crate) id: Index,
    pub(crate) engine: Engine,
    pub(crate) examples: Vec<Example>,
    pub(crate) suggesters: Vec<grammar::Synthesizer>,
    pub(crate) message: grammar::Synthesizer,
    pub(crate) start: GraphId,
    pub(crate) end: GraphId,
    pub(crate) url: Option<String>,
    pub(crate) short: Option<String>,
    pub(crate) name: String,
    pub(crate) category_name: String,
    pub(crate) category_type: Option<String>,
    pub(crate) unification: Option<Unification>,
    pub(crate) enabled: bool,
}

impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.id)
    }
}

impl Rule {
    /// Hints that this rule should be enabled.
    pub fn enable(&mut self) {
        self.enabled = true;
    }

    /// Hints that this rule should be disabled.
    pub fn disable(&mut self) {
        self.enabled = false;
    }

    /// Hints whether the rule should be enabled in a rule set.
    pub fn enabled(&self) -> bool {
        self.enabled
    }

    /// Get a unique identifier of this rule.
    pub fn id(&self) -> &Index {
        &self.id
    }

    /// Gets a short text describing this rule e.g. "Possible typo" if there is one.
    pub fn short(&self) -> Option<&str> {
        self.short.as_deref()
    }

    /// Gets an url with more information about this rule if there is one.
    pub fn url(&self) -> Option<&str> {
        self.url.as_deref()
    }

    /// Gets the examples associated with this rule.
    pub fn examples(&self) -> &[Example] {
        &self.examples
    }

    /// Gets a human-readable name of this rule.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Gets a human-readable name of the category this rule is in.
    pub fn category_name(&self) -> &str {
        &self.category_name
    }

    /// Gets the type of the category this rule is in e. g. "style" or "grammar".
    pub fn category_type(&self) -> Option<&str> {
        self.category_type.as_deref()
    }

    pub(crate) fn apply<'a, 't>(
        &'a self,
        sentence: &'t MatchSentence<'t>,
        tokenizer: &'a Tokenizer,
    ) -> Suggestions<'a, 't> {
        Suggestions {
            matches: self.engine.get_matches(sentence, self.start, self.end),
            rule: &self,
            tokenizer,
            sentence,
        }
    }

    /// Grammar rules always have at least one example associated with them.
    /// This method checks whether the correct action is taken in the examples.
    pub fn test(&self, tokenizer: &Tokenizer) -> bool {
        let mut passes = Vec::new();

        for test in self.examples.iter() {
            // by convention examples are always considered as one sentence even if the sentencizer would split
            let sentence = Sentence::new(tokenizer.disambiguate(tokenizer.tokenize(&test.text())));
            info!("Sentence: {:#?}", sentence);
            let suggestions: Vec<_> = self
                .apply(&MatchSentence::new(&sentence), tokenizer)
                .collect();

            let pass = if suggestions.len() > 1 {
                false
            } else {
                match test.suggestion() {
                    Some(correct_suggestion) => {
                        suggestions.len() == 1 && correct_suggestion == &suggestions[0]
                    }
                    None => suggestions.is_empty(),
                }
            };

            if !pass {
                warn!(
                    "Rule {}: test \"{}\" failed. Expected: {:#?}. Found: {:#?}.",
                    self.id,
                    test.text(),
                    test.suggestion(),
                    suggestions
                );
            }

            passes.push(pass);
        }

        passes.iter().all(|x| *x)
    }
}
