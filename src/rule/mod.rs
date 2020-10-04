use crate::composition::{Composition, Group, MatchGraph};
use crate::filter::Filter;
use crate::tokenizer::{
    disambiguate_up_to_id, finalize, tokenize, IncompleteToken, Token, Word, WordData,
};
use crate::utils;
use log::{info, warn};
use onig::Regex;
use std::collections::HashSet;

pub mod from_structure;

#[derive(Debug)]
pub struct Suggestion {
    pub start: usize,
    pub end: usize,
    pub text: Vec<String>,
}

impl std::cmp::PartialEq for Suggestion {
    fn eq(&self, other: &Suggestion) -> bool {
        let a: HashSet<&String> = self.text.iter().collect();
        let b: HashSet<&String> = other.text.iter().collect();

        a.intersection(&b).count() > 0 && other.start == self.start && other.end == self.end
    }
}

#[derive(Debug)]
pub struct Test {
    text: String,
    suggestion: Option<Suggestion>,
}

pub struct Match {
    id: usize,
    conversion: Box<dyn Fn(&str) -> String>,
    regex_replacer: Option<(Regex, String)>,
    has_conversion: bool,
}

impl std::fmt::Debug for Match {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(fmt, "Match {{ index: {:?} }}", self.id)?;
        Ok(())
    }
}

impl Match {
    fn apply(&self, graph: &MatchGraph) -> String {
        let text = graph
            .by_id(self.id)
            .unwrap_or_else(|| panic!("group must exist in graph: {}", self.id))
            .tokens
            .get(0)
            .map(|x| x.word.text.as_str())
            .unwrap_or("");

        if let Some((regex, replacement)) = &self.regex_replacer {
            let replaced = regex.replace_all(text, replacement.as_str());
            (self.conversion)(replaced.as_ref())
        } else {
            (self.conversion)(text)
        }
    }

    fn new(
        id: usize,
        conversion: Option<Box<dyn Fn(&str) -> String>>,
        regex_replacer: Option<(Regex, String)>,
    ) -> Self {
        Match {
            id,
            has_conversion: conversion.is_some(),
            conversion: conversion.unwrap_or_else(|| Box::new(|x: &str| x.to_string())),
            regex_replacer,
        }
    }

    fn has_conversion(&self) -> bool {
        self.has_conversion
    }
}

#[derive(Debug)]
pub enum SuggesterPart {
    Text(String),
    Match(Match),
}

#[derive(Debug)]
pub struct Suggester {
    parts: Vec<SuggesterPart>,
}

impl Suggester {
    fn apply(&self, groups: &MatchGraph, start_group: &Group, _end_group: &Group) -> String {
        let mut output = Vec::new();
        let mut matchers_have_conversion = false;

        for part in &self.parts {
            match part {
                SuggesterPart::Text(t) => output.push(t.clone()),
                SuggesterPart::Match(m) => {
                    if m.has_conversion() {
                        matchers_have_conversion = true;
                    };
                    output.push(m.apply(groups));
                }
            }
        }

        let suggestion = utils::normalize_whitespace(&output.join(""));

        // if the suggestion contains no case conversion matches, make it title case if:
        // * at sentence start
        // * the replaced text is title case
        if !matchers_have_conversion
            && !start_group.tokens.is_empty()
            && (start_group.tokens[0]
                .word
                .text
                .chars()
                .next()
                .expect("token must have at least one char")
                .is_uppercase())
        {
            utils::apply_to_first(&suggestion, |x| x.to_uppercase().collect())
        } else {
            suggestion
        }
    }
}

trait RuleMatch {
    fn composition(&self) -> &Composition;
    fn antipatterns(&self) -> &Vec<Composition>;
    fn id(&self) -> &str;
    fn start(&self) -> usize;
    fn end(&self) -> usize;

    fn get_match<'a>(
        &self,
        tokens: &'a [&Token],
        i: usize,
        mask: Option<&mut Vec<bool>>,
    ) -> Option<MatchGraph<'a>> {
        if let Some(graph) = self.composition().apply(tokens, i) {
            let start_group = graph.by_id(self.start()).unwrap_or_else(|| {
                panic!("{} group must exist in graph: {}", self.id(), self.start())
            });
            let end_group = graph.by_id(self.end() - 1).unwrap_or_else(|| {
                panic!(
                    "{} group must exist in graph: {}",
                    self.id(),
                    self.end() - 1
                )
            });

            let start = start_group.char_start;
            let end = end_group.char_end;

            // only add the suggestion if we don't have any yet from this rule in its range
            if mask
                .as_ref()
                .map_or(true, |x| !x[start..end].iter().any(|x| *x))
            {
                let mut blocked = false;

                // TODO: cache / move to outer loop
                for i in 0..tokens.len() {
                    for antipattern in self.antipatterns() {
                        if let Some(anti_graph) = antipattern.apply(tokens, i) {
                            let anti_start = anti_graph.by_index(0).char_start;
                            let anti_end = anti_graph.by_index(anti_graph.len() - 1).char_end;

                            let rule_start = graph.by_index(0).char_start;
                            let rule_end = graph.by_index(graph.len() - 1).char_end;

                            if anti_start <= rule_end && rule_start <= anti_end {
                                blocked = true;
                                break;
                            }
                        }
                    }
                    if blocked {
                        break;
                    }
                }

                if !blocked {
                    if let Some(mask) = mask {
                        mask[start..end].iter_mut().for_each(|x| *x = true);
                    }

                    return Some(graph);
                }
            }
        }

        None
    }
}

macro_rules! impl_rule_match {
    ($e:ty) => {
        impl RuleMatch for $e {
            fn antipatterns(&self) -> &Vec<Composition> {
                &self.antipatterns
            }

            fn composition(&self) -> &Composition {
                &self.composition
            }

            fn start(&self) -> usize {
                self.start
            }

            fn end(&self) -> usize {
                self.end
            }

            fn id(&self) -> &str {
                self.id.as_str()
            }
        }
    };
}

impl_rule_match!(Rule);
impl_rule_match!(DisambiguationRule);

pub enum POSFilter {
    Regex(Regex),
    String(String),
}

impl POSFilter {
    pub fn regex(regex: Regex) -> Self {
        POSFilter::Regex(regex)
    }

    pub fn string(string: String) -> Self {
        POSFilter::String(string)
    }

    fn keep(&self, data: &mut Word) {
        data.tags.retain(|x| match self {
            POSFilter::String(string) => &x.pos == string,
            POSFilter::Regex(regex) => regex.is_match(&x.pos),
        })
    }

    fn is_match(&self, data: &Word) -> bool {
        data.tags.iter().any(|x| match self {
            POSFilter::String(string) => &x.pos == string,
            POSFilter::Regex(regex) => regex.is_match(&x.pos),
        })
    }

    fn remove(&self, data: &mut Word) {
        data.tags.retain(|x| match self {
            POSFilter::String(string) => &x.pos != string,
            POSFilter::Regex(regex) => !regex.is_match(&x.pos),
        })
    }
}

pub enum Disambiguation {
    Remove(Vec<either::Either<WordData, POSFilter>>),
    Add(Vec<WordData>),
    Replace(Vec<WordData>),
    Filter(Vec<Option<either::Either<WordData, POSFilter>>>),
    Unify(Vec<Vec<POSFilter>>),
    Nop,
}

impl Disambiguation {
    fn apply(&self, groups: Vec<Vec<&mut IncompleteToken>>) {
        match self {
            Disambiguation::Remove(data_or_filters) => {
                for (group, data_or_filter) in groups.into_iter().zip(data_or_filters) {
                    for token in group.into_iter() {
                        match data_or_filter {
                            either::Left(data) => {
                                token.word.tags.retain(|x| {
                                    !(x.pos == data.pos
                                        && (data.lemma.is_empty() || x.lemma == data.lemma))
                                });
                            }
                            either::Right(filter) => {
                                filter.remove(&mut token.word);
                            }
                        }
                    }
                }
            }
            Disambiguation::Filter(filters) => {
                for (group, maybe_filter) in groups.into_iter().zip(filters) {
                    if let Some(data_or_filter) = maybe_filter {
                        match data_or_filter {
                            either::Left(limit) => {
                                for token in group.into_iter() {
                                    let last = token
                                        .word
                                        .tags
                                        .get(0)
                                        .map_or(token.word.text.to_string(), |x| {
                                            x.lemma.to_string()
                                        });

                                    token.word.tags.retain(|x| x.pos == limit.pos);

                                    if token.word.tags.is_empty() {
                                        token
                                            .word
                                            .tags
                                            .push(WordData::new(last, limit.pos.to_string()));
                                    }
                                }
                            }
                            either::Right(filter) => {
                                for token in group.into_iter() {
                                    filter.keep(&mut token.word)
                                }
                            }
                        }
                    }
                }
            }
            Disambiguation::Add(datas) => {
                for (group, data) in groups.into_iter().zip(datas) {
                    for token in group.into_iter() {
                        let mut data = data.clone();
                        if data.lemma.is_empty() {
                            data.lemma = token.word.text.to_string();
                        }

                        token.word.tags.push(data);
                        token.word.tags.retain(|x| !x.pos.is_empty());
                    }
                }
            }
            Disambiguation::Replace(datas) => {
                for (group, data) in groups.into_iter().zip(datas) {
                    for token in group.into_iter() {
                        let mut data = data.clone();
                        if data.lemma.is_empty() {
                            data.lemma = token.word.text.to_string();
                        }

                        token.word.tags.clear();
                        token.word.tags.push(data);
                    }
                }
            }
            Disambiguation::Unify(filters) => {
                let mut apply_filters: Option<Vec<&POSFilter>> = None;

                for group in groups.iter() {
                    for token in group.iter() {
                        if let Some(apply_filters) = &apply_filters {
                            // need to finalize here to also match SENT_END etc.
                            let finalized: Token = (*token).clone().into();

                            for filter in apply_filters {
                                if !filter.is_match(&finalized.word) {
                                    return;
                                }
                            }
                        } else {
                            let maybe_apply_filters = filters
                                .iter()
                                .map(|equivs| equivs.iter().find(|x| x.is_match(&token.word)))
                                .collect::<Option<Vec<_>>>();

                            if maybe_apply_filters.is_none() {
                                return;
                            }

                            apply_filters = maybe_apply_filters;
                        }
                    }
                }

                for group in groups.into_iter() {
                    for token in group.into_iter() {
                        if let Some(apply_filters) = &apply_filters {
                            for filter in apply_filters {
                                filter.keep(&mut token.word);
                            }
                        }
                    }
                }
            }
            Disambiguation::Nop => {}
        }
    }
}

#[derive(Debug)]
pub struct DisambiguationChange {
    text: String,
    char_span: (usize, usize),
    before: Word,
    after: Word,
}

#[derive(Debug)]
pub enum DisambiguationTest {
    Unchanged(String),
    Changed(DisambiguationChange),
}

pub struct DisambiguationRule {
    pub id: String,
    composition: Composition,
    antipatterns: Vec<Composition>,
    disambiguations: Disambiguation,
    filter: Option<Box<dyn Filter>>,
    start: usize,
    end: usize,
    tests: Vec<DisambiguationTest>,
}

impl DisambiguationRule {
    pub fn set_id(&mut self, id: String) {
        self.id = id;
    }

    pub fn apply(&self, mut tokens: Vec<IncompleteToken>) -> Vec<IncompleteToken> {
        let complete_tokens = finalize(tokens.clone());
        let refs: Vec<&Token> = complete_tokens.iter().collect();

        let mut all_byte_spans = Vec::new();

        for i in 0..tokens.len() {
            if let Some(graph) = self.get_match(&refs, i, None) {
                if let Some(filter) = &self.filter {
                    if !filter.keep(&graph) {
                        continue;
                    }
                }

                let mut byte_spans = Vec::new();

                for group_idx in self.start..self.end {
                    let group = graph.by_id(group_idx).unwrap_or_else(|| {
                        panic!("{} group must exist in graph: {}", self.id, self.start)
                    });

                    let group_byte_spans: HashSet<_> =
                        group.tokens.iter().map(|x| x.byte_span).collect();

                    byte_spans.push(group_byte_spans);
                }

                all_byte_spans.push(byte_spans);
            }
        }

        for byte_spans in all_byte_spans {
            let mut groups = Vec::new();
            let mut refs = tokens.iter_mut().collect::<Vec<_>>();

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

            self.disambiguations.apply(groups);
        }

        tokens
    }

    pub fn test(&self) -> bool {
        let mut passes = Vec::new();

        for test in &self.tests {
            let text = match test {
                DisambiguationTest::Unchanged(x) => x.as_str(),
                DisambiguationTest::Changed(x) => x.text.as_str(),
            };

            let tokens_before = disambiguate_up_to_id(tokenize(text), &self.id);
            let mut tokens_after = tokens_before.clone();
            tokens_after = self.apply(tokens_after);

            info!("Tokens: {:#?}", tokens_before);

            let pass = match test {
                DisambiguationTest::Unchanged(_) => tokens_before == tokens_after,
                DisambiguationTest::Changed(change) => {
                    let _before = tokens_before
                        .iter()
                        .find(|x| x.char_span == change.char_span)
                        .unwrap();

                    let after = tokens_after
                        .iter()
                        .find(|x| x.char_span == change.char_span)
                        .unwrap();

                    let unordered_tags = after.word.tags.iter().collect::<HashSet<_>>();
                    let unordered_tags_change = change.after.tags.iter().collect::<HashSet<_>>();

                    after.word.text == change.after.text && unordered_tags == unordered_tags_change
                }
            };

            if !pass {
                warn!(
                    "Rule {}: Test \"{:#?}\" failed. Before: {:#?}. After: {:#?}.",
                    self.id,
                    test,
                    tokens_before.into_iter().collect::<Vec<_>>(),
                    tokens_after.into_iter().collect::<Vec<_>>(),
                );
            }

            passes.push(pass);
        }

        passes.iter().all(|x| *x)
    }
}

pub struct Rule {
    pub id: String,
    composition: Composition,
    antipatterns: Vec<Composition>,
    tests: Vec<Test>,
    suggesters: Vec<Suggester>,
    start: usize,
    end: usize,
}

impl Rule {
    pub fn set_id(&mut self, id: String) {
        self.id = id;
    }

    pub fn apply(&self, tokens: &[Token]) -> Vec<Suggestion> {
        let refs: Vec<&Token> = tokens.iter().collect();
        let mut suggestions = Vec::new();

        let mut mask: Vec<_> = vec![
            false;
            tokens
                .get(tokens.len() - 1)
                .map(|x| x.char_span.1)
                .unwrap_or(0)
        ];

        for i in 0..tokens.len() {
            if let Some(graph) = self.get_match(&refs, i, Some(&mut mask)) {
                let start_group = graph.by_id(self.start).unwrap_or_else(|| {
                    panic!("{} group must exist in graph: {}", self.id, self.start)
                });
                let end_group = graph.by_id(self.end - 1).unwrap_or_else(|| {
                    panic!("{} group must exist in graph: {}", self.id, self.end - 1)
                });

                let start = start_group.char_start;
                let end = end_group.char_end;

                suggestions.push(Suggestion {
                    start,
                    end,
                    text: self
                        .suggesters
                        .iter()
                        .map(|x| x.apply(&graph, start_group, end_group))
                        .collect(),
                });
            }
        }

        suggestions
    }

    pub fn test(&self) -> bool {
        let mut passes = Vec::new();

        for test in &self.tests {
            let tokens = finalize(tokenize(&test.text));
            info!("Tokens: {:#?}", tokens);
            let suggestions = self.apply(&tokens);

            let pass = if suggestions.len() > 1 {
                false
            } else {
                match &test.suggestion {
                    Some(correct_suggestion) => {
                        suggestions.len() == 1 && correct_suggestion == &suggestions[0]
                    }
                    None => suggestions.is_empty(),
                }
            };

            if !pass {
                warn!(
                    "Rule {}: test \"{}\" failed. Expected: {:#?}. Found: {:#?}.",
                    self.id, test.text, test.suggestion, suggestions
                );
            }

            passes.push(pass);
        }

        passes.iter().all(|x| *x)
    }
}
