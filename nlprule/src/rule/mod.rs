use crate::tokenizer::{finalize, IncompleteToken, OwnedWord, Token, Word, WordData};
use crate::utils::{self, SerializeRegex};
use crate::{
    composition::{Composition, MatchGraph},
    tokenizer::Tokenizer,
};
use crate::{
    filter::{Filter, Filterable},
    tokenizer::OwnedWordData,
};
use itertools::Itertools;
use log::{error, info, warn};
use onig::Captures;
use serde::{Deserialize, Serialize};
use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
};

#[cfg(feature = "compile")]
use crate::from_structure;

#[derive(Debug, Serialize, Deserialize)]
pub struct Suggestion {
    pub source: String,
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

#[derive(Debug, Serialize, Deserialize)]
pub struct Test {
    pub(crate) text: String,
    pub(crate) suggestion: Option<Suggestion>,
}

#[derive(Serialize, Deserialize)]
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

#[derive(Serialize, Deserialize)]
pub struct Match {
    id: usize,
    conversion: Conversion,
    regex_replacer: Option<(SerializeRegex, String)>,
}

impl std::fmt::Debug for Match {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(fmt, "Match {{ index: {:?} }}", self.id)?;
        Ok(())
    }
}

impl Match {
    fn apply(&self, graph: &MatchGraph) -> Option<String> {
        let text = graph
            .by_id(self.id)
            .unwrap_or_else(|| panic!("group must exist in graph: {}", self.id))
            .tokens
            .get(0)
            .map(|x| {
                if x.word.text.is_empty() {
                    None
                } else {
                    Some(x.word.text)
                }
            })
            .unwrap_or(Some(""))?;

        if let Some((regex, replacement)) = &self.regex_replacer {
            let replaced = regex.replace_all(text, |caps: &Captures| {
                utils::dollar_replace(replacement.to_string(), caps)
            });
            Some(self.conversion.convert(replaced.as_ref()))
        } else {
            Some(self.conversion.convert(text))
        }
    }

    pub fn new(
        id: usize,
        conversion: Conversion,
        regex_replacer: Option<(SerializeRegex, String)>,
    ) -> Self {
        Match {
            id,
            conversion,
            regex_replacer,
        }
    }

    fn has_conversion(&self) -> bool {
        !matches!(self.conversion, Conversion::Nop)
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub enum SuggesterPart {
    Text(String),
    Match(Match),
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Suggester {
    pub(crate) parts: Vec<SuggesterPart>,
}

impl Suggester {
    fn apply(&self, groups: &MatchGraph, start: usize, _end: usize) -> Option<String> {
        let mut output = Vec::new();

        let starts_with_conversion = match &self.parts[..] {
            [SuggesterPart::Match(m), ..] => m.has_conversion(),
            _ => false,
        };

        for part in &self.parts {
            match part {
                SuggesterPart::Text(t) => output.push(t.clone()),
                SuggesterPart::Match(m) => {
                    output.push(m.apply(groups)?);
                }
            }
        }

        let suggestion = utils::normalize_whitespace(&output.join(""));

        // if the suggestion does not start with a case conversion match, make it title case if:
        // * at sentence start
        // * the replaced text is title case
        let first_token = groups.groups()[groups.get_index(start).unwrap()..]
            .iter()
            .find(|x| !x.tokens.is_empty())
            .unwrap()
            .tokens[0];

        if !starts_with_conversion
            && (first_token
                .word
                .text
                .chars()
                .next()
                .expect("token must have at least one char")
                .is_uppercase()
                || first_token.byte_span.0 == 0)
        {
            Some(utils::apply_to_first(&suggestion, |x| {
                x.to_uppercase().collect()
            }))
        } else {
            Some(suggestion)
        }
    }
}

#[derive(Serialize, Deserialize)]
pub enum POSFilter {
    Regex(SerializeRegex),
    String(String),
}

impl POSFilter {
    pub fn regex(regex: SerializeRegex) -> Self {
        POSFilter::Regex(regex)
    }

    pub fn string(string: String) -> Self {
        POSFilter::String(string)
    }

    fn is_word_data_match(&self, data: &WordData) -> bool {
        match self {
            POSFilter::String(string) => data.pos == string,
            POSFilter::Regex(regex) => regex.is_match(&data.pos),
        }
    }

    fn keep(&self, data: &mut Word) {
        data.tags.retain(|x| self.is_word_data_match(x))
    }

    fn remove(&self, data: &mut Word) {
        data.tags.retain(|x| !self.is_word_data_match(x))
    }

    fn and(filters: &[&Self], data: &Word) -> bool {
        data.tags
            .iter()
            .any(|x| filters.iter().all(|filter| filter.is_word_data_match(x)))
    }

    fn apply(filters: &[Vec<&Self>], data: &mut Word) {
        data.tags.retain(|x| {
            filters
                .iter()
                .any(|filter| filter.iter().all(|f| f.is_word_data_match(x)))
        })
    }
}

#[derive(Serialize, Deserialize)]
pub enum Disambiguation {
    Remove(Vec<either::Either<OwnedWordData, POSFilter>>),
    Add(Vec<OwnedWordData>),
    Replace(Vec<OwnedWordData>),
    Filter(Vec<Option<either::Either<OwnedWordData, POSFilter>>>),
    Unify(Vec<Vec<POSFilter>>, Vec<Option<POSFilter>>, Vec<bool>),
    Nop,
}

impl Disambiguation {
    fn apply<'t>(&'t self, groups: Vec<Vec<&mut IncompleteToken<'t>>>, retain_last: bool) {
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
                                        .map_or(token.word.text, |x| x.lemma.as_ref())
                                        .to_string();

                                    token.word.tags.retain(|x| x.pos == limit.pos);

                                    if token.word.tags.is_empty() {
                                        token.word.tags.push(WordData::new(
                                            if retain_last {
                                                Cow::Owned(last)
                                            } else {
                                                token.word.text.into()
                                            },
                                            limit.pos.as_str(),
                                        ));
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
                        let data = WordData::new(
                            if data.lemma.is_empty() {
                                token.word.text
                            } else {
                                data.lemma.as_str()
                            },
                            data.pos.as_str(),
                        );

                        token.word.tags.push(data);
                        token.word.tags.retain(|x| !x.pos.is_empty());
                    }
                }
            }
            Disambiguation::Replace(datas) => {
                for (group, data) in groups.into_iter().zip(datas) {
                    for token in group.into_iter() {
                        let data = WordData::new(
                            if data.lemma.is_empty() {
                                token.word.text
                            } else {
                                data.lemma.as_str()
                            },
                            data.pos.as_str(),
                        );

                        token.word.tags.clear();
                        token.word.tags.push(data);
                    }
                }
            }
            Disambiguation::Unify(filters, disambigs, mask) => {
                let filters: Vec<_> = filters.iter().multi_cartesian_product().collect();

                let mut filter_mask: Vec<_> = filters.iter().map(|_| true).collect();

                for (group, use_mask_val) in groups.iter().zip(mask) {
                    for token in group.iter() {
                        if *use_mask_val {
                            let finalized: Token = (*token).clone().into();

                            for (mask_val, filter) in filter_mask.iter_mut().zip(filters.iter()) {
                                *mask_val = *mask_val && POSFilter::and(filter, &finalized.word);
                            }
                        }
                    }
                }

                if !filter_mask.iter().any(|x| *x) {
                    return;
                }

                let to_apply: Vec<_> = filter_mask
                    .iter()
                    .zip(filters)
                    .filter_map(
                        |(mask_val, filter)| {
                            if *mask_val {
                                Some(filter)
                            } else {
                                None
                            }
                        },
                    )
                    .collect();

                for ((group, disambig), use_mask_val) in groups.into_iter().zip(disambigs).zip(mask)
                {
                    if *use_mask_val {
                        for token in group.into_iter() {
                            let before = token.word.clone();

                            POSFilter::apply(&to_apply, &mut token.word);

                            if let Some(disambig) = disambig {
                                disambig.keep(&mut token.word);
                            }

                            if token.word.tags.is_empty() {
                                token.word = before;
                            }
                        }
                    }
                }
            }
            Disambiguation::Nop => {}
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct DisambiguationChange {
    pub(crate) text: String,
    pub(crate) char_span: (usize, usize),
    pub(crate) before: OwnedWord,
    pub(crate) after: OwnedWord,
}

#[derive(Debug, Serialize, Deserialize)]
pub enum DisambiguationTest {
    Unchanged(String),
    Changed(DisambiguationChange),
}

#[derive(Serialize, Deserialize)]
pub struct DisambiguationRule {
    pub(crate) id: String,
    pub(crate) engine: Engine,
    pub(crate) disambiguations: Disambiguation,
    pub(crate) filter: Option<Filter>,
    pub(crate) start: usize,
    pub(crate) end: usize,
    pub(crate) tests: Vec<DisambiguationTest>,
}

impl DisambiguationRule {
    pub fn id(&self) -> &str {
        self.id.as_str()
    }

    pub fn set_id(&mut self, id: String) {
        self.id = id;
    }

    pub fn apply<'t>(
        &'t self,
        mut tokens: Vec<IncompleteToken<'t>>,
        tokenizer: &Tokenizer,
        mut skip_mask: Vec<bool>,
        complete_tokens: Option<Vec<Token<'t>>>,
    ) -> (Vec<IncompleteToken<'t>>, Option<Vec<Token<'t>>>) {
        if matches!(self.disambiguations, Disambiguation::Nop) {
            return (tokens, None);
        }

        for (i, val) in skip_mask.iter_mut().enumerate().filter(|(_, x)| !**x) {
            *val = if let Engine::Token(engine) = &self.engine {
                engine.composition.can_not_match(tokens[i].as_ref())
            } else {
                false
            };
        }

        if skip_mask.iter().all(|x| *x) {
            return (tokens, None);
        }

        let complete_tokens = if let Some(complete_tokens) = complete_tokens {
            complete_tokens
        } else {
            finalize(tokens.clone())
        };
        // this assumes that finalizing only ever inserts the SENT_START token
        // works at the moment but not very clean
        skip_mask.insert(0, false);
        let refs: Vec<&Token> = complete_tokens.iter().collect();

        let mut all_byte_spans = Vec::new();

        for graph in self.engine.get_matches(&refs, None, Some(&skip_mask)) {
            if let Some(filter) = &self.filter {
                if !filter.keep(&graph, tokenizer) {
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

        if all_byte_spans.is_empty() {
            return (tokens, Some(complete_tokens));
        }

        log::info!("applying {}", self.id);

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

            self.disambiguations
                .apply(groups, tokenizer.options().retain_last);
        }

        (tokens, None)
    }

    pub fn test(&self, tokenizer: &Tokenizer) -> bool {
        let mut passes = Vec::new();

        for (i, test) in self.tests.iter().enumerate() {
            let text = match test {
                DisambiguationTest::Unchanged(x) => x.as_str(),
                DisambiguationTest::Changed(x) => x.text.as_str(),
            };

            let tokens_before = tokenizer.disambiguate_up_to_id(tokenizer.tokenize(text), &self.id);
            let mut tokens_after = tokens_before.clone();
            tokens_after = self
                .apply(
                    tokens_after,
                    tokenizer,
                    vec![false; tokens_before.len()],
                    None,
                )
                .0;

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

                    let unordered_tags = after
                        .word
                        .tags
                        .iter()
                        .map(|x| x.to_owned_word_data())
                        .collect::<HashSet<OwnedWordData>>();
                    // need references to compare
                    let unordered_tags: HashSet<_> = unordered_tags.iter().collect();
                    let unordered_tags_change = change
                        .after
                        .tags
                        .iter()
                        .collect::<HashSet<&OwnedWordData>>();

                    after.word.text == change.after.text && unordered_tags == unordered_tags_change
                }
            };

            if !pass {
                let error_str = format!(
                    "Rule {}: Test \"{:#?}\" failed. Before: {:#?}. After: {:#?}.",
                    self.id,
                    test,
                    tokens_before.into_iter().collect::<Vec<_>>(),
                    tokens_after.into_iter().collect::<Vec<_>>(),
                );

                if tokenizer
                    .options()
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

#[derive(Serialize, Deserialize)]
pub struct TokenEngine {
    pub(crate) composition: Composition,
    pub(crate) antipatterns: Vec<Composition>,
    pub(crate) start: usize,
    pub(crate) end: usize,
}

impl TokenEngine {
    fn get_match<'t>(
        &'t self,
        tokens: &'t [&'t Token],
        i: usize,
        mask: &mut Option<Vec<bool>>,
    ) -> Option<MatchGraph<'t>> {
        if let Some(graph) = self.composition.apply(tokens, i) {
            let start_group = graph
                .by_id(self.start)
                .unwrap_or_else(|| panic!("group must exist in graph: {}", self.start));
            let end_group = graph
                .by_id(self.end - 1)
                .unwrap_or_else(|| panic!("group must exist in graph: {}", self.end - 1));

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
                    for antipattern in &self.antipatterns {
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

#[derive(Serialize, Deserialize)]
pub enum Engine {
    Token(TokenEngine),
    Text(SerializeRegex),
}

impl Engine {
    fn get_matches<'t>(
        &'t self,
        tokens: &'t [&'t Token],
        mut mask: Option<Vec<bool>>,
        skip_mask: Option<&[bool]>,
    ) -> Vec<MatchGraph<'t>> {
        let mut graphs = Vec::new();

        match &self {
            Engine::Token(engine) => {
                for i in 0..tokens.len() {
                    if let Some(skip_mask) = skip_mask {
                        if skip_mask[i] {
                            continue;
                        }
                    }

                    graphs.extend(engine.get_match(&tokens, i, &mut mask));
                }
            }
            Engine::Text(_regex) => {
                // for cap in regex.captures_iter(text) {

                // }
                unimplemented!()
            }
        }

        graphs
    }
}

#[derive(Serialize, Deserialize)]
pub struct Rule {
    pub(crate) id: String,
    pub(crate) engine: Engine,
    pub(crate) tests: Vec<Test>,
    pub(crate) suggesters: Vec<Suggester>,
    pub(crate) start: usize,
    pub(crate) end: usize,
    pub(crate) on: bool,
}

impl Rule {
    pub fn id(&self) -> &str {
        self.id.as_str()
    }

    pub fn set_id(&mut self, id: String) {
        self.id = id;
    }

    pub fn on(&self) -> bool {
        self.on
    }

    pub fn set_on(&mut self, on: bool) {
        self.on = on;
    }

    pub fn apply(&self, tokens: &[Token], skip_mask: Option<&[bool]>) -> Vec<Suggestion> {
        let refs: Vec<&Token> = tokens.iter().collect();
        let mut suggestions = Vec::new();

        let mask: Option<Vec<_>> = Some(vec![
            false;
            tokens
                .get(tokens.len() - 1)
                .map(|x| x.char_span.1)
                .unwrap_or(0)
        ]);

        for graph in self.engine.get_matches(&refs, mask, skip_mask) {
            let start_group = graph
                .by_id(self.start)
                .unwrap_or_else(|| panic!("{} group must exist in graph: {}", self.id, self.start));
            let end_group = graph.by_id(self.end - 1).unwrap_or_else(|| {
                panic!("{} group must exist in graph: {}", self.id, self.end - 1)
            });

            let text: Vec<String> = self
                .suggesters
                .iter()
                .filter_map(|x| x.apply(&graph, self.start, self.end))
                .collect();

            let start = if text
                .iter()
                .all(|x| utils::no_space_chars().chars().any(|c| x.starts_with(c)))
            {
                let first_token = graph.groups()[graph.get_index(self.start).unwrap()..]
                    .iter()
                    .find(|x| !x.tokens.is_empty())
                    .unwrap()
                    .tokens[0];

                let idx = tokens
                    .iter()
                    .position(|x| std::ptr::eq(x, first_token))
                    .unwrap_or(0);

                if idx > 0 {
                    tokens[idx - 1].char_span.1
                } else {
                    start_group.char_start
                }
            } else {
                start_group.char_start
            };
            let end = end_group.char_end;

            // fix e. g. "Super , dass"
            let text: Vec<String> = text
                .into_iter()
                .map(|x| utils::fix_nospace_chars(&x))
                .collect();

            if !text.is_empty() {
                suggestions.push(Suggestion {
                    source: self.id.to_string(),
                    start,
                    end,
                    text,
                });
            }
        }

        suggestions
    }

    pub fn test(&self, tokenizer: &Tokenizer) -> bool {
        let mut passes = Vec::new();

        for test in self.tests.iter() {
            let tokens = finalize(tokenizer.disambiguate(tokenizer.tokenize(&test.text)));
            info!("Tokens: {:#?}", tokens);
            let suggestions = self.apply(&tokens, None);

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

#[derive(Serialize, Deserialize, Clone)]
pub struct RulesOptions {
    pub allow_errors: bool,
    #[serde(default)]
    pub ids: Vec<String>,
    #[serde(default)]
    pub ignore_ids: Vec<String>,
}

impl Default for RulesOptions {
    fn default() -> Self {
        RulesOptions {
            allow_errors: true,
            ids: Vec::new(),
            ignore_ids: Vec::new(),
        }
    }
}

#[derive(Serialize, Deserialize, Default)]
pub struct Cache {
    cache: HashMap<String, Vec<bool>>,
}

impl Cache {
    pub fn get_skip_mask<S: AsRef<str>>(&self, texts: &[S], i: usize) -> Vec<bool> {
        texts
            .iter()
            .map(|x| {
                self.cache
                    .get(x.as_ref())
                    .map(|mask| mask[i])
                    .unwrap_or(false)
            })
            .collect()
    }

    pub fn populate(&mut self, common_words: &HashSet<String>, engines: &[&Engine]) {
        for engine in engines {
            for word in common_words {
                let can_not_match = if let Engine::Token(engine) = engine {
                    engine.composition.can_not_match(&word)
                } else {
                    false
                };

                self.cache
                    .entry(word.to_string())
                    .or_insert_with(Vec::new)
                    .push(can_not_match);
            }
        }
    }
}

#[derive(Serialize, Deserialize, Default)]
pub struct Rules {
    rules: Vec<Rule>,
    cache: Cache,
}

impl Rules {
    #[cfg(feature = "compile")]
    pub fn from_xml<P: AsRef<std::path::Path>>(path: P, options: RulesOptions) -> Self {
        use std::convert::TryFrom;

        let rules = from_structure::structure::read_rules(path);
        let mut errors: HashMap<String, usize> = HashMap::new();

        let rules: Vec<_> = rules
            .into_iter()
            .filter_map(|x| match x {
                Ok((rule_structure, id, on)) => match Rule::try_from(rule_structure) {
                    Ok(mut rule) => {
                        if (options.ids.is_empty() || options.ids.contains(&id))
                            && !options.ignore_ids.contains(&id)
                        {
                            rule.set_id(id);
                            rule.set_on(on);
                            Some(rule)
                        } else {
                            None
                        }
                    }
                    Err(x) => {
                        *errors.entry(format!("[Rule] {}", x)).or_insert(0) += 1;
                        None
                    }
                },
                Err(x) => {
                    *errors.entry(format!("[Structure] {}", x)).or_insert(0) += 1;
                    None
                }
            })
            .collect();

        if !errors.is_empty() {
            let mut errors: Vec<(String, usize)> = errors.into_iter().collect();
            errors.sort_by_key(|x| -(x.1 as i32));

            warn!("Errors constructing Rules: {:#?}", &errors);
        }

        Rules {
            rules,
            cache: Cache::default(),
        }
    }

    pub fn populate_cache(&mut self, common_words: &HashSet<String>) {
        self.cache.populate(
            common_words,
            &self.rules.iter().map(|x| &x.engine).collect::<Vec<_>>(),
        );
    }

    pub fn rules(&self) -> &Vec<Rule> {
        &self.rules
    }

    pub fn apply(&self, tokens: &[Token]) -> Vec<Suggestion> {
        if tokens.is_empty() {
            return Vec::new();
        }

        let mut output = Vec::new();
        let mut mask = vec![false; tokens[tokens.len() - 1].char_span.1];

        for (i, rule) in self.rules.iter().enumerate().filter(|(_, x)| x.on()) {
            let skip_mask = self.cache.get_skip_mask(tokens, i);
            for suggestion in rule.apply(tokens, Some(&skip_mask)) {
                if mask[suggestion.start..suggestion.end].iter().all(|x| !x) {
                    mask[suggestion.start..suggestion.end]
                        .iter_mut()
                        .for_each(|x| *x = true);

                    output.push(suggestion);
                }
            }
        }

        output.sort_by(|a, b| a.start.cmp(&b.start));
        output
    }

    pub fn correct(text: &str, suggestions: &[Suggestion]) -> String {
        let mut offset: isize = 0;
        let mut chars: Vec<_> = text.chars().collect();

        for suggestion in suggestions {
            let replacement: Vec<_> = suggestion.text[0].chars().collect();
            chars.splice(
                (suggestion.start as isize + offset) as usize
                    ..(suggestion.end as isize + offset) as usize,
                replacement.iter().cloned(),
            );
            offset =
                offset + replacement.len() as isize - (suggestion.end - suggestion.start) as isize;
        }

        chars.into_iter().collect()
    }
}
