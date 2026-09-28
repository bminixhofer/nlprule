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

pub mod filter_data;
pub mod filter_java;
pub(crate) mod disambiguation;
pub(crate) mod engine;
pub(crate) mod grammar;
pub mod id;
pub(crate) mod post_filter;
pub(crate) mod priorities;
pub mod regex_rule;
pub mod synthesizer;

use engine::Engine;

pub(crate) use engine::composition::{MatchGraph, MatchSentence};
pub use grammar::Example;
pub use synthesizer::Synthesizer as MorphSynthesizer;

use self::{
    disambiguation::PosFilter,
    engine::{composition::GraphId, EngineMatches},
    id::Index,
};

/// A *Unification* makes an otherwise matching pattern invalid if no combination of its filters
/// matches all tokens marked with "unify".
/// Can also be negated.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub(crate) struct Unification {
    pub(crate) mask: Vec<Option<bool>>,
    pub(crate) filters: Vec<Vec<PosFilter>>,
    /// Per marked element, its own postag constraint: LT's Unifier derives
    /// feature values only from readings that matched the element, so e.g.
    /// a token matched as N.* contributes only its noun readings.
    #[serde(default)]
    pub(crate) element_filters: Vec<Option<PosFilter>>,
}

impl Unification {
    pub fn keep(&self, graph: &MatchGraph, sentence: &MatchSentence) -> bool {
        let filters: Vec<_> = self.filters.iter().multi_cartesian_product().collect();

        let mut filter_mask: Vec<_> = filters.iter().map(|_| true).collect();
        let negate = self.mask.iter().all(|x| x.map_or(true, |x| !x));

        for ((group, maybe_mask_val), element_filter) in graph.groups()[1..]
            .iter()
            .zip(self.mask.iter())
            .zip(self.element_filters.iter().chain(std::iter::repeat(&None)))
        {
            if maybe_mask_val.is_some() {
                for token in group.tokens(sentence) {
                    for (mask_val, filter) in filter_mask.iter_mut().zip(filters.iter()) {
                        // restrict to readings that satisfy the element's
                        // own postag constraint before checking the feature
                        let restricted = crate::rule::PosFilter::and_restricted(
                            filter,
                            token.word(),
                            element_filter.as_ref(),
                        );
                        *mask_val = *mask_val && restricted;
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
#[derive(Debug, Serialize, Deserialize, Clone)]
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
pub(crate) struct Changes(Vec<Vec<HashSet<Span>>>);

// This is only used in tests at the moment.
// Could maybe be made generic.
impl Changes {
    fn lshift(self, position: Position) -> Self {
        Changes(
            self.0
                .into_iter()
                .map(|spans| {
                    spans
                        .into_iter()
                        .map(|group_spans| {
                            group_spans
                                .into_iter()
                                .map(|span| span.lshift(position))
                                .collect()
                        })
                        .collect()
                })
                .collect(),
        )
    }
}

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

    pub(crate) fn apply<'t>(&'t self, sentence: &MatchSentence<'t>) -> Changes {
        if matches!(self.disambiguations, disambiguation::Disambiguation::Nop) {
            return Changes::default();
        }

        let mut all_spans = Vec::new();

        for graph in self.engine.get_matches(sentence, self.start, self.end) {
            if let Some(unification) = &self.unification {
                if !unification.keep(&graph, sentence) {
                    continue;
                }
            }

            if let Some(filter) = &self.filter {
                if !filter.keep(sentence, &graph) {
                    continue;
                }
            }

            let mut spans = Vec::new();

            for group_idx in GraphId::range(&self.start, &self.end) {
                let group = graph.by_id(group_idx);

                let group_spans: HashSet<_> =
                    group.tokens(sentence).map(|x| x.span().clone()).collect();

                spans.push(group_spans);
            }

            all_spans.push(spans);
        }

        Changes(all_spans)
    }

    pub(crate) fn change<'t>(&'t self, sentence: &mut IncompleteSentence<'t>, changes: Changes) {
        log::info!("applying {}", self.id);

        for spans in changes.0 {
            let mut groups = Vec::new();
            let mut refs = sentence.iter_mut().collect::<Vec<_>>();

            for group_spans in spans {
                let mut group = Vec::new();

                while let Some(i) = refs.iter().position(|x| group_spans.contains(&x.span())) {
                    group.push(refs.remove(i));
                }

                groups.push(group);
            }

            self.disambiguations.apply(groups);
        }
    }

    /// Often there are examples associated with a rule.
    /// This method checks whether the correct action is taken in the examples.
    pub fn test(&self, tokenizer: &Tokenizer) -> bool {
        self.test_with_synth(tokenizer, None)
    }

    /// Like [Rule::test], but with access to the morphological synthesizer
    /// used by `match` elements with a `postag`.
    pub fn test_with_synth(
        &self,
        tokenizer: &Tokenizer,
        synth: Option<&MorphSynthesizer>,
    ) -> bool {
        self.test_with_synth_and_data(tokenizer, synth, None)
    }

    /// Like [Rule::test_with_synth], with the shared filter data (speller etc.).
    pub fn test_with_synth_and_data(
        &self,
        tokenizer: &Tokenizer,
        synth: Option<&MorphSynthesizer>,
        filter_data: Option<&crate::rule::filter_data::FilterData>,
    ) -> bool {
        let mut passes = Vec::new();

        for (i, test) in self.examples.iter().enumerate() {
            let text = match test {
                disambiguation::DisambiguationExample::Unchanged(x) => x.as_str(),
                disambiguation::DisambiguationExample::Changed(x) => x.text.as_str(),
            };

            // by convention examples are always considered as one sentence even if the sentencizer would split
            let sentence_before = tokenizer.disambiguate_up_to_id(
                tokenizer
                    .tokenize(text)
                    .expect("test text must not be empty"),
                Some(&self.id),
            );

            // shift the sentence to the right before matching to make sure
            // nothing assumes the sentene starts from absolute index zero
            let shift_delta = Position { byte: 1, char: 1 };
            let sentence_before_complete =
                sentence_before.clone().rshift(shift_delta).into_sentence();
            let changes = self
                .apply(&MatchSentence::new(&sentence_before_complete))
                .lshift(shift_delta);

            let mut sentence_after = sentence_before.clone();

            if !changes.is_empty() {
                self.change(&mut sentence_after, changes);
            }

            info!("Tokens: {:#?}", sentence_before);

            let pass = match test {
                disambiguation::DisambiguationExample::Unchanged(_) => {
                    sentence_before == sentence_after
                }
                disambiguation::DisambiguationExample::Changed(change) => {
                    let _before = sentence_before
                        .iter()
                        .find(|x| *x.span().char() == change.char_span)
                        .unwrap();

                    let after = sentence_after
                        .iter()
                        .find(|x| *x.span().char() == change.char_span)
                        .unwrap();

                    let unordered_tags = after
                        .word()
                        .tags()
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

                    after.word().as_str() == change.after.text.as_ref_id().as_str()
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
    synth: Option<&'a MorphSynthesizer>,
    filter_data: Option<&'a crate::rule::filter_data::FilterData>,
    matches: EngineMatches<'a, 't>,
    sentence: &'t MatchSentence<'t>,
}

impl<'a, 't> Iterator for Suggestions<'a, 't> {
    type Item = Suggestion;

    fn next(&mut self) -> Option<Self::Item> {
        let rule = self.rule;
        let sentence = self.sentence;
        let synth = self.synth;
        let filter_data = self.filter_data;
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
                .flat_map(|x| x.apply(sentence, &graph, start, end, synth))
                .collect();

            let mut start = if replacements
                .iter()
                .all(|x| utils::no_space_chars().chars().any(|c| x.starts_with(c)))
                && replacements.iter().any(|x| !x.is_empty())
            {
                let first_token = graph.groups()[graph.get_index(start)..]
                    .iter()
                    .find_map(|x| x.tokens(sentence).next())
                    .unwrap();

                let idx = sentence
                    .iter()
                    .position(|x| std::ptr::eq(x, first_token))
                    .unwrap_or(0);

                if idx > 0 {
                    sentence.index(idx - 1).span().end()
                } else {
                    start_group.span.start()
                }
            } else {
                start_group.span.start()
            };
            let mut end = end_group.span.end();

            // this should never happen, but just return None instead of raising an Error
            // `end` COULD be equal to `start` if the suggestion is to insert text at this position
            if end < start {
                return None;
            }

            // apply the rule's post-filter (LT `<filter>`) if any
            let (replacements, filter_message) = if let Some(filter) = &rule.post_filter {
                let matched_text = sentence.slice(Span::from_positions(start, end)).to_string();
                let message = rule
                    .message
                    .apply(sentence, &graph, rule.start, rule.end, synth)
                    .into_iter()
                    .next()
                    .unwrap_or_default();

                // indices of the sentence tokens covered by the WHOLE pattern
                // (LT filter `\N` backreferences refer to all matched elements,
                // not just the marker range)
                // LT passes `Arrays.copyOfRange(sentenceTokens, first, last + 1)`
                // to its filters: the CONTIGUOUS sentence-token range of the
                // match, which includes the SENT_START token (zero char span)
                // when a leading wildcard consumed it, and any skipped tokens
                // between matched elements
                let gstart = graph
                    .groups()
                    .first()
                    .map(|g| g.span.char().start)
                    .unwrap_or(0);
                let gend = graph
                    .groups()
                    .last()
                    .map(|g| g.span.char().end)
                    .unwrap_or(0);
                // groups[0] is always an empty artifact group; the first
                // pattern element consumed the SENT_START token exactly when
                // the group AFTER the artifact is zero-length at position 0
                // (SENT_START is the only zero-char-span token)
                let starts_at_sent_start = graph
                    .groups()
                    .get(1)
                    .map(|g| g.span.char().start == 0 && g.span.char().end == 0)
                    .unwrap_or(false);
                let matched_tokens: Vec<usize> = (0..sentence.len())
                    .filter(|&i| {
                        let span = sentence.index(i).span().char();
                        let real_token = span.end > span.start;
                        span.start >= gstart
                            && span.end <= gend
                            && (real_token || (starts_at_sent_start && span.start == 0))
                    })
                    .collect::<Vec<usize>>();

                let mut seen = std::collections::HashSet::new();
                let matched_tokens: Vec<usize> = matched_tokens
                    .into_iter()
                    .filter(|idx| seen.insert(*idx))
                    .collect();

                let empty_data = crate::rule::filter_data::FilterData::default();
                let data = filter_data.unwrap_or(&empty_data);

                match filter.apply(
                    sentence,
                    Span::from_positions(start, end),
                    replacements,
                    &matched_text,
                    &message,
                    &matched_tokens,
                    synth,
                    data,
                ) {
                    Some(filtered) => {
                        start = filtered.span.start();
                        end = filtered.span.end();
                        (filtered.replacements, filtered.message)
                    }
                    None => return None,
                }
            } else {
                (replacements, None)
            };

            let text_before = sentence.slice(Span::from_positions(start, end));

            // fix e. g. "Super , dass"
            let replacements: Vec<String> = replacements
                .into_iter()
                .filter(|suggestion| *suggestion != text_before)
                .map(|x| utils::fix_nospace_chars(&x))
                .collect();

            if !replacements.is_empty() || rule.suggesters.is_empty() {
                // rules without suggestion elements report an error without replacements
                let message = filter_message.unwrap_or_else(|| {
                    rule.message
                        .apply(sentence, &graph, rule.start, rule.end, synth)
                        .into_iter()
                        .next()
                        .unwrap_or_default()
                });
                Some(Suggestion::new(
                    rule.id.to_string(),
                    message,
                    Span::from_positions(start, end),
                    replacements,
                ))
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
#[derive(Serialize, Deserialize, Debug, Clone)]
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
    /// Runtime post-filter applied to fired matches (LT `<filter>` element).
    pub(crate) post_filter: Option<post_filter::PostFilter>,
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

    pub(crate) fn apply<'a, 't>(&'a self, sentence: &'t MatchSentence<'t>) -> Suggestions<'a, 't> {
        self.apply_with_synth(sentence, None, None)
    }

    pub(crate) fn apply_with_synth<'a, 't>(
        &'a self,
        sentence: &'t MatchSentence<'t>,
        synth: Option<&'a MorphSynthesizer>,
        filter_data: Option<&'a crate::rule::filter_data::FilterData>,
    ) -> Suggestions<'a, 't> {
        Suggestions {
            synth,
            filter_data,
            matches: self.engine.get_matches(sentence, self.start, self.end),
            rule: &self,
            sentence,
        }
    }

    /// Grammar rules always have at least one example associated with them.
    /// This method checks whether the correct action is taken in the examples.
    pub fn test(&self, tokenizer: &Tokenizer) -> bool {
        self.test_with_synth(tokenizer, None)
    }

    /// Like [Rule::test], but with access to the morphological synthesizer
    /// used by `match` elements with a `postag`.
    pub fn test_with_synth(
        &self,
        tokenizer: &Tokenizer,
        synth: Option<&MorphSynthesizer>,
    ) -> bool {
        self.test_with_synth_and_data(tokenizer, synth, None)
    }

    /// Like [Rule::test_with_synth], with the shared filter data (speller etc.).
    pub fn test_with_synth_and_data(
        &self,
        tokenizer: &Tokenizer,
        synth: Option<&MorphSynthesizer>,
        filter_data: Option<&crate::rule::filter_data::FilterData>,
    ) -> bool {
        let mut passes = Vec::new();

        // make sure relative position is handled correctly
        // shifting the entire sentence must be a no-op as far as the matcher is concerned
        // if the suggestions are shifted back
        let shift_delta = Position { byte: 1, char: 1 };

        for test in self.examples.iter() {
            // by convention examples are always considered as one sentence even if the sentencizer would split
            let sentence = tokenizer
                .disambiguate(
                    tokenizer
                        .tokenize(&test.text())
                        .expect("test text must not be empty."),
                )
                .rshift(shift_delta)
                .into_sentence();

            info!("Sentence: {:#?}", sentence);
            let suggestions: Vec<_> = self
                .apply_with_synth(&MatchSentence::new(&sentence), synth, filter_data)
                .map(|mut s| {
                    // rules without a suggestion report an error without replacements;
                    // the expected replacement is the empty string in that case
                    if s.replacements().iter().all(|x| x.is_empty()) {
                        s.set_replacements(vec![String::new()]);
                    }
                    s.lshift(shift_delta)
                })
                .collect();

            let expected: Option<Suggestion> = test.suggestion().cloned();

            let pass = if test.kind.as_deref() == Some("triggers_error") {
                // the rule must trigger; the replacement is not checked
                !suggestions.is_empty()
            } else if suggestions.len() > 1 {
                false
            } else {
                match &expected {
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
