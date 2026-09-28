//! Sets of grammatical error correction rules.

use crate::types::*;
use crate::utils::parallelism::MaybeParallelRefIterator;
use crate::{rule::id::Selector, rule::MatchSentence, rule::Rule, tokenizer::Tokenizer, Error};
use fs_err::File;
use serde::{Deserialize, Serialize};
use std::{
    io::{BufReader, Read, Write},
    iter::FromIterator,
    path::Path,
};

/// Language-dependent options for a rule set.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct RulesLangOptions {
    /// Whether to allow errors while constructing the rules.
    pub allow_errors: bool,
    /// Grammar Rule selectors to use in this set.
    #[serde(default)]
    pub ids: Vec<Selector>,
    /// Grammar Rule selectors to ignore in this set.
    #[serde(default)]
    pub ignore_ids: Vec<Selector>,
}

impl Default for RulesLangOptions {
    fn default() -> Self {
        RulesLangOptions {
            allow_errors: true,
            ids: Vec::new(),
            ignore_ids: Vec::new(),
        }
    }
}

/// A set of grammatical error correction rules.
#[derive(Serialize, Deserialize, Default)]
pub struct Rules {
    /// Shared auxiliary data for the ported Java rule filters.
    pub(crate) filter_data: crate::rule::filter_data::FilterData,
    pub(crate) rules: Vec<Rule>,
    /// Language code of this rule set, used by the built-in Java-only rules
    /// (e.g. `UPPERCASE_SENTENCE_START`). Not serialized: set it with
    /// [Rules::with_lang] (built-ins degrade to language-independent
    /// behavior when unset).
    #[serde(skip)]
    pub(crate) builtin_lang: Option<String>,
    /// Shared morphological synthesizer used by rules to inflect lemmas in suggestions.
    pub(crate) synth: Option<std::sync::Arc<crate::rule::synthesizer::Synthesizer>>,
    /// Rule-level `<regexp>` rules (regex-on-sentence-text), applied per
    /// sentence like the pattern rules.
    #[serde(default)]
    pub(crate) regex_rules: Vec<crate::rule::regex_rule::RegexRuleDef>,
}


/// Port of `CleanOverlappingFilter.filter` (LT 6.5). Priorities are all
/// treated as equal (no language overrides priority in the languages we
/// build; the `picky` tag only matters at picky level).
fn clean_overlapping(suggestions: Vec<Suggestion>, lang: Option<&str>) -> Vec<Suggestion> {
    let mut clean = Vec::new();
    let mut iter = suggestions.into_iter();
    let mut prev = match iter.next() {
        Some(first) => first,
        None => return clean,
    };
    let (mut prev_from, mut prev_to) =
        (prev.span().char().start, prev.span().char().end);

    for rule_match in iter {
        let (cur_from, cur_to) = (
            rule_match.span().char().start,
            rule_match.span().char().end,
        );
        if cur_from < prev_from {
            // cannot happen (the list is sorted); treat as non-overlapping
            clean.push(prev);
            prev = rule_match;
            let span = prev.span().char().clone();
            prev_from = span.start;
            prev_to = span.end;
            continue;
        }

        let mut is_duplicate_suggestion = false;
        if let (Some(suggestion), Some(prev_suggestion)) = (
            rule_match.replacements().first(),
            prev.replacements().first(),
        ) {
            let (pf, pt) = (prev_from, prev_to); let _ = pf;
            if !suggestion.is_empty() && !prev_suggestion.is_empty() {
                // juxtaposed errors adding a comma in the same place
                if cur_from == pt && prev_suggestion.ends_with(',') && suggestion.starts_with(", ") {
                    is_duplicate_suggestion = true;
                }
                // duplicate suggestion for the same position
                if suggestion.contains(' ')
                    && prev_suggestion.contains(' ')
                    && cur_from == pt + 1
                {
                    let parts: Vec<&str> = suggestion.split(' ').collect();
                    let parts_prev: Vec<&str> = prev_suggestion.split(' ').collect();
                    if parts_prev.len() > 1
                        && parts.len() > 1
                        && parts_prev[1] == parts[0]
                    {
                        is_duplicate_suggestion = true;
                    }
                }
            }
        }

        // no overlap (juxtaposed errors are kept)
        if cur_from >= prev_to && !is_duplicate_suggestion {
            clean.push(prev);
            prev = rule_match;
            let span = prev.span().char().clone();
            prev_from = span.start;
            prev_to = span.end;
            continue;
        }

        // LT's HTTP API reports every firing rule for a span; exact-same-span
        // suggestions are both kept ONLY when one of them is a builtin text
        // family (e.g. es COMILLAS + ES_UNPAIRED, DE_VERBAGREEMENT).  For two
        // XML rules the v7 keep-one semantics apply: the port tagger makes
        // some XML rules over-fire on spans where LT fires a different rule,
        // and surfacing both adds only-nlprule events (v8 regression).
        if cur_from == prev_from && cur_to == prev_to && rule_match.source() != prev.source() {
            let fam = crate::builtins::text_family_ids(lang);
            let either_builtin =
                fam.contains(&rule_match.source()) || fam.contains(&prev.source());
            if either_builtin {
                clean.push(prev);
                prev = rule_match;
                let span = prev.span().char().clone();
                prev_from = span.start;
                prev_to = span.end;
                continue;
            }
        }
        // overlapping: CleanOverlappingFilter semantics — language priority
        let priority_of = |sugg: &Suggestion| -> isize {
            crate::rule::priorities::priority_for_id(lang, sugg.source()) as isize
        };
        let mut cur_priority = priority_of(&rule_match);
        let mut prev_priority = priority_of(&prev);
        if std::env::var("NLPRULE_DEBUG_OVERLAP").is_ok() {
            eprintln!(
                "[OVL] cur={:?}[{},{}]p{} prev={:?}[{},{}]p{}",
                rule_match.source(),
                cur_from,
                cur_to,
                cur_priority,
                prev.source(),
                prev_from,
                prev_to,
                prev_priority
            );
        }
        if cur_priority == prev_priority {
            // take the longest error:
            cur_priority = (cur_to - cur_from) as isize;
            prev_priority = (prev_to - prev_from) as isize;
        }
        if cur_priority == prev_priority {
            cur_priority += 1; // take the last one (LT: keep web UI results)
        }
        if cur_priority > prev_priority {
            prev = rule_match;
            let span = prev.span().char().clone();
            prev_from = span.start;
            prev_to = span.end;
        }
    }
    clean.push(prev);
    clean
}

impl Rules {
    /// Creates a new rule set from a path to a binary.
    ///
    /// # Errors
    /// - If the file can not be opened.
    /// - If the file content can not be deserialized to a rules set.
    pub fn new<P: AsRef<Path>>(p: P) -> Result<Self, Error> {
        let reader = BufReader::new(File::open(p.as_ref())?);
        let mut rules: Rules = bincode::deserialize_from(reader)?;
        // A/B switch for the unifier element-filter restriction (LT Unifier
        // parity commit): strip at load to measure its effect per language.
        if std::env::var("NLPRULE_NO_UNIFIER_FILTER").is_ok() {
            for rule in &mut rules.rules {
                if let Some(u) = &mut rule.unification {
                    u.element_filters = Vec::new();
                }
            }
        }
        Ok(rules)
    }

    /// Creates a new rule set from a path to a binary, recording the language
    /// code so the built-in Java-only rules (`UPPERCASE_SENTENCE_START`, ...)
    /// can apply their language-specific behavior. The language is never
    /// serialized with the binary.
    pub fn with_lang<P: AsRef<Path>>(p: P, lang: &str) -> Result<Self, Error> {
        let mut rules = Self::new(p)?;
        rules.builtin_lang = Some(lang.to_string());
        // dictionaries without case conversion (pl: encoder=none) need
        // exact-case lookups — see SpellerDict::case_sensitive
        if lang == "pl" {
            if let Some(speller) = rules.filter_data.speller.as_mut() {
                speller.case_sensitive = true;
            }
        }
        Ok(rules)
    }

    /// Creates a new rules set from a reader.
    pub fn from_reader<R: Read>(reader: R) -> Result<Self, Error> {
        Ok(bincode::deserialize_from(reader)?)
    }

    /// Serializes this rules set to a writer.
    pub fn to_writer<W: Write>(&self, writer: W) -> Result<(), Error> {
        Ok(bincode::serialize_into(writer, &self)?)
    }

    /// All rules ordered by priority.
    pub fn rules(&self) -> &[Rule] {
        &self.rules
    }

    /// The shared morphological synthesizer used by rules to inflect lemmas
    /// in suggestions, if this language has a synthesis dictionary.
    pub fn synthesizer(&self) -> Option<&std::sync::Arc<crate::rule::synthesizer::Synthesizer>> {
        self.synth.as_ref()
    }

    /// The shared filter data (speller, multitoken tables, ...) if any.
    pub fn filter_data(&self) -> &crate::rule::filter_data::FilterData {
        &self.filter_data
    }

    /// All rules ordered by priority (mutable).
    pub fn rules_mut(&mut self) -> &mut [Rule] {
        &mut self.rules
    }

    /// Returns an iterator over all rules matching the selector.
    pub fn select<'a>(&'a self, selector: &'a Selector) -> RulesIter<'a> {
        RulesIter {
            inner: self.rules.iter(),
            selector: Some(selector),
        }
    }

    /// Returns an iterator over all rules matching the selector (mutable).
    pub fn select_mut<'a>(&'a mut self, selector: &'a Selector) -> RulesIterMut<'a> {
        RulesIterMut {
            inner: self.rules.iter_mut(),
            selector: Some(selector),
        }
    }

    /// Enable or disable every rule whose id (e.g. `CATEGORY/GROUP/N` or
    /// `CATEGORY/RULE_ID`) matches the selector — pattern rules and
    /// rule-level `<regexp>` rules. Used to activate `default="off"`
    /// rules on demand (task-3 probe: an explicit activation must fire a
    /// rule that stays silent by default).
    pub fn set_enabled_matching(&mut self, selector: &Selector, enabled: bool) -> usize {
        let mut n = 0;
        for rule in self.rules.iter_mut() {
            if selector.is_match(rule.id()) {
                rule.enabled = enabled;
                n += 1;
            }
        }
        for rule in self.regex_rules.iter_mut() {
            // sources look like `CATEGORY/GROUP/N`; parse manually into an
            // Index (id.rs has no FromStr for it)
            let parts: Vec<&str> = rule.source.split('/').collect();
            if parts.len() == 3 {
                if let Ok(index) = parts[2].parse::<usize>() {
                    let idx = crate::rule::id::Category::new(parts[0])
                        .join(parts[1])
                        .join(index);
                    if selector.is_match(&idx) {
                        rule.enabled = enabled;
                        n += 1;
                    }
                }
            }
        }
        n
    }

    /// Compute the suggestions for the given sentence by checking all rules.
    /// Builtin rules that are Java classes in LT (no XML): duplicated words,
    /// doubled punctuation and sentence-start casing.
    /// Inventory of the loaded rules for the coverage audit tool
    /// (`scripts/coverage_audit.py`): every pattern rule with its source and
    /// enabled flag, the rule-level `<regexp>` rules, and the builtin ids
    /// applicable for this language.
    pub fn dump_rules(&self) -> serde_json::Value {
        let pattern: Vec<serde_json::Value> = self
            .rules
            .iter()
            .map(|r| {
                serde_json::json!({
                    "source": r.id().to_string(),
                    "enabled": r.enabled(),
                })
            })
            .collect();
        let regex: Vec<String> = self
            .regex_rules
            .iter()
            .map(|r| r.source.clone())
            .collect();
        let builtins =
            crate::builtins::builtin_ids(self.builtin_lang.as_deref(), &self.filter_data);
        serde_json::json!({ "pattern": pattern, "regex": regex, "builtins": builtins })
    }

    fn builtin_suggestions(
        &self,
        sentence: &Sentence,
        ctx: &crate::builtins::TextRuleContext,
    ) -> Vec<Suggestion> {
        // the speller before the casing rule: at equal spans the later match
        // wins in clean_overlapping, so the casing rule replaces the speller
        // only when it is longer, and a pattern rule replaces both - as on
        // the LT server
        let mut out = crate::builtins::morfologik_spelling(
            sentence,
            self.builtin_lang.as_deref(),
            self.filter_data.speller.as_ref(),
        );
        out.extend(crate::builtins::uppercase_sentence_start(
            sentence,
            self.builtin_lang.as_deref(),
            ctx,
        ));
        if self.builtin_lang.as_deref() == Some("km") {
            out.extend(crate::builtins::khmer_space_before(sentence));
        }
        // text-level families ported from LT 6.5 (task-4)
        out.extend(crate::builtins::double_punctuation(
            sentence,
            self.builtin_lang.as_deref(),
        ));
        out.extend(crate::builtins::multiple_whitespace(
            sentence,
            self.builtin_lang.as_deref(),
        ));
        out.extend(crate::builtins::comma_parenthesis_whitespace(
            sentence,
            self.builtin_lang.as_deref(),
        ));
        out.extend(crate::builtins::unpaired_brackets(
            sentence,
            self.builtin_lang.as_deref(),
        ));
        out.extend(crate::builtins::a_vs_an(
            sentence,
            self.builtin_lang.as_deref(),
        ));
        out.extend(crate::builtins::arabic_punct_whitespace(
            sentence,
            self.builtin_lang.as_deref(),
        ));
        out.extend(crate::builtins::es_question_mark(
            sentence,
            self.builtin_lang.as_deref(),
        ));
        out.extend(crate::builtins::french_whitespace(
            sentence,
            self.builtin_lang.as_deref(),
        ));
        out.extend(crate::builtins::arabic_diacritics(
            sentence,
            self.builtin_lang.as_deref(),
        ));
        out.extend(crate::builtins::de_verbagreement(
            sentence,
            self.builtin_lang.as_deref(),
        ));
        out.extend(crate::builtins::de_subject_verb_agreement(
            sentence,
            self.builtin_lang.as_deref(),
        ));
        out.extend(crate::builtins::ru_verb_conjugation(
            sentence,
            self.builtin_lang.as_deref(),
        ));
        out.extend(crate::builtins::br_topo(
            sentence,
            self.builtin_lang.as_deref(),
        ));
        out.extend(crate::builtins::de_case(
            sentence,
            self.builtin_lang.as_deref(),
        ));
        out.extend(crate::builtins::ru_compounds(
            sentence,
            self.builtin_lang.as_deref(),
        ));
        let tokens: Vec<_> = sentence.iter().collect();
        // SimpleReplaceRule phrase table (nl): case-sensitive multiword
        // lookup with sub-rule ids derived from the wrong phrase
        if let Some(table) = self.filter_data.simple_replace.as_ref() {
            for (i, token) in tokens.iter().enumerate() {
                let candidates = match table.by_first_word.get(token.word().as_str()) {
                    Some(c) => c,
                    None => continue,
                };
                for (wrong, corrects) in candidates {
                    let words: Vec<&str> = wrong.split_whitespace().collect();
                    if words.len() > tokens.len() - i {
                        continue;
                    }
                    let matches = words.iter().enumerate().all(|(k, w)| {
                        tokens[i + k].word().as_str() == *w
                            && (k == 0 || tokens[i + k].has_space_before())
                    });
                    if !matches {
                        continue;
                    }
                    let span =
                        Span::from_positions(tokens[i].span().start(), tokens[i + words.len() - 1].span().end());
                    let sub_id = if table.sub_ids {
                        format!(
                            "{}_{}",
                            table.prefix,
                            wrong
                                .to_uppercase()
                                .chars()
                                .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
                                .collect::<String>()
                        )
                    } else {
                        table.prefix.clone()
                    };
                    out.push(Suggestion::new(
                        sub_id,
                        format!("Possible mistake: did you mean \"{}\"?", corrects[0]),
                        span,
                        corrects.clone(),
                    ));
                    break;
                }
            }
        }
        out.extend(crate::builtins::word_repeat(
            sentence,
            self.builtin_lang.as_deref(),
        ));
        out
    }

    pub fn apply(&self, sentence: &Sentence) -> Vec<Suggestion> {
        self.apply_with_context(sentence, &crate::builtins::TextRuleContext::default())
    }

    /// Like [Rules::apply], with the text-level context of the built-in rules
    /// (position of this sentence within the checked text).
    pub fn apply_with_context(
        &self,
        sentence: &Sentence,
        ctx: &crate::builtins::TextRuleContext,
    ) -> Vec<Suggestion> {
        let plain_sentence = sentence;
        let sentence = MatchSentence::new(sentence);

        let mut output: Vec<(usize, Suggestion)> = self
            .rules
            .maybe_par_iter()
            .enumerate()
            .filter(|(_, rule)| rule.enabled())
            .map(|(i, rule)| {
                let mut output = Vec::new();

                for suggestion in rule.apply_with_synth(
                    &sentence,
                    self.synth.as_deref(),
                    Some(&self.filter_data),
                ) {
                    output.push((i, suggestion));
                }

                output
            })
            .flatten()
            .collect();

        for (i, suggestion) in self.builtin_suggestions(plain_sentence, ctx)
            .into_iter()
            .enumerate()
            .map(|(j, sugg)| (self.rules.len() + j, sugg))
        {
            output.push((i, suggestion));
        }

        // rule-level <regexp> rules (regex-on-sentence-text); indices after
        // the built-ins so pattern rules and built-ins keep winning equal-
        // span ties, mirroring LT's text-level-rule ordering
        for (i, suggestion) in self
            .regex_rules
            .iter()
            .filter(|rule| rule.enabled)
            .flat_map(|rule| rule.apply(plain_sentence.text()))
            .enumerate()
            .map(|(k, sugg)| (self.rules.len() + 1_000_000 + k, sugg))
        {
            output.push((i, suggestion));
        }


        let n_pattern_rules = self.rules.len();
        output.sort_by(|(ia, a), (ib, b)| {
            a.span()
                .char()
                .start
                .cmp(&b.span().char().start)
                // tiers preserve the validated cross-family outcomes:
                // text-level built-ins first (a pattern or speller match at
                // the same span replaces them, as on the server), then
                // rule-level <regexp> rules, then pattern rules ascending
                // (grammar order) — among pattern rules the later rule
                // survives an equal-span tie like LT's stable sort does.
                .then_with(|| {
                    let rank = |i: usize| {
                        if i < n_pattern_rules {
                            2
                        } else if i >= n_pattern_rules + 1_000_000 {
                            1
                        } else {
                            0
                        }
                    };
                    rank(*ia)
                        .cmp(&rank(*ib))
                        .then_with(|| ia.cmp(ib))
                })
        });

        // Port of LT's CleanOverlappingFilter (the HTTP server always runs
        // with it): walk the position-sorted matches, drop overlapping ones -
        // ties resolved by longer match, then by the later match in list
        // order (LT appends the text-level built-ins after the sentence
        // rules, so with an exactly equal span the later rule - or the
        // built-in - wins, e.g. nl keeps IETS_KLEINS over GEURIGS_GURIGS).
        let suggestions: Vec<_> = output
            .into_iter()
            .map(|(_, suggestion)| {
                suggestion
                    .span()
                    .clone()
                    .lshift(sentence.span().start());
                suggestion
            })
            .collect();

        clean_overlapping(suggestions, self.builtin_lang.as_deref())
    }

    /// Compute the suggestions for a text by checking all rules.
    pub fn suggest(&self, text: &str, tokenizer: &Tokenizer) -> Vec<Suggestion> {
        if text.is_empty() {
            return Vec::new();
        }

        use crate::rule::post_filter::PostFilter;

        // rule group id -> rule IDs to check, for rules carrying
        // SuppressIfAnyRuleMatchesFilter (runs here because it needs the
        // whole rule set and the tokenizer)
        let suppress_specs: std::collections::HashMap<String, Vec<String>> = self
            .rules
            .iter()
            .filter_map(|rule| {
                if let Some(PostFilter::SuppressIfAny { rule_ids }) = &rule.post_filter {
                    Some((
                        rule.id.to_string().split('/').nth(1)?.to_string(),
                        rule_ids.clone(),
                    ))
                } else {
                    None
                }
            })
            .collect();

        let mut suggestions = Vec::new();

        // get suggestions sentence by sentence, carrying the text-level state
        // LT's text-level built-in rules see (previous sentence's last token,
        // numbered-list detection)
        let sentences: Vec<_> = tokenizer.pipe(text).collect();
        let mut prev_last_token: Option<String> = None;
        let mut prev_numbered_list = false;

        // LT `SentenceWhitespaceRule` (id WHITESPACE_RULE, every language):
        // a run of >=3 spaces after the last token of a sentence — at the
        // end of the text or before the next sentence — is flagged from
        // the second space on. Sentence text excludes trailing whitespace
        // (tokenizer.rs contract), so this pass runs at text level.
        let mut ws_text: Vec<Suggestion> = Vec::new();
        for (i, sentence) in sentences.iter().enumerate() {
            // token-based boundaries: the sentence slice may include
            // trailing whitespace itself (tokenizer contract is undefined)
            let last_tok_end = sentence
                .iter()
                .last()
                .map(|t| t.span().byte().end)
                .unwrap_or(sentence.span().byte().start);
            let next_start = sentences
                .get(i + 1)
                .and_then(|s| s.iter().next().map(|t| t.span().byte().start))
                .unwrap_or(text.len());
            let tail = &text[last_tok_end.min(text.len())..next_start.min(text.len())];
            let n = tail.bytes().take_while(|b| *b == b' ').count();
            if n >= 3 {
                let cs_start = sentence
                    .iter()
                    .last()
                    .map(|t| t.span().char().end)
                    .unwrap_or(0);
                ws_text.push(Suggestion::new(
                    "WHITESPACE_RULE".to_string(),
                    "Trailing whitespace.".to_string(),
                    Span::new((last_tok_end + 1)..(last_tok_end + n), (cs_start + 1)..(cs_start + n - 1)),
                    vec![String::new()],
                ));
            }
        }

        for (i, sentence) in sentences.iter().enumerate() {
            let ctx = crate::builtins::TextRuleContext {
                is_only_sentence: sentences.len() == 1,
                prev_last_token: prev_last_token.clone(),
                prev_numbered_list,
            };

            let mut sentence_suggestions = self.apply_with_context(sentence, &ctx);

            if !suppress_specs.is_empty() {
                sentence_suggestions.retain(|suggestion| {
                    let group = suggestion
                        .source()
                        .split('/')
                        .nth(1)
                        .unwrap_or(suggestion.source())
                        .to_string();
                    let rule_ids = match suppress_specs.get(&group) {
                        Some(ids) => ids,
                        None => return true,
                    };

                    let sent_start = sentence.span().byte().start;
                    let from = suggestion.span().byte().start - sent_start;
                    let to = suggestion.span().byte().end - sent_start;
                    let sentence_text = sentence.text();

                    // LT: suppress if ANY replacement makes one of the
                    // listed rules match the re-analyzed sentence (first
                    // sentence) overlapping the original span
                    for replacement in suggestion.replacements() {
                        let new_sentence = format!(
                            "{}{}{}",
                            &sentence_text[..from],
                            replacement,
                            &sentence_text[to..]
                        );
                        if let Some(new_first) = tokenizer.pipe(&new_sentence).next() {
                            for other in self.apply_with_context(&new_first, &ctx) {
                                if !rule_ids
                                    .iter()
                                    .any(|id| {
                                        other.source().split('/').nth(1) == Some(id.as_str())
                                    })
                                {
                                    continue;
                                }
                                let (ofrom, oto) = (
                                    other.span().byte().start,
                                    other.span().byte().end,
                                );
                                if ofrom <= to && oto >= from {
                                    return false;
                                }
                            }
                        }
                    }
                    true
                });
            }

            suggestions.extend(sentence_suggestions);

            prev_last_token = crate::builtins::last_significant_token(sentence);
            prev_numbered_list = crate::builtins::is_numbered_list_item(sentence.text());
            let _ = i;
        }

        suggestions.extend(ws_text);
        suggestions
    }

    /// Correct a text by first tokenizing, then finding all suggestions and choosing the first replacement of each suggestion.
    pub fn correct(&self, text: &str, tokenizer: &Tokenizer) -> String {
        let suggestions = self.suggest(text, tokenizer);
        apply_suggestions(text, &suggestions)
    }
}

/// Correct a text by applying suggestions to it.
/// In the case of multiple possible replacements, always chooses the first one.
pub fn apply_suggestions(text: &str, suggestions: &[Suggestion]) -> String {
    let mut offset: isize = 0;
    let mut chars: Vec<_> = text.chars().collect();

    for suggestion in suggestions {
        let replacement: Vec<_> = suggestion.replacements()[0].chars().collect();
        chars.splice(
            (suggestion.span().char().start as isize + offset) as usize
                ..(suggestion.span().char().end as isize + offset) as usize,
            replacement.iter().cloned(),
        );
        offset = offset + replacement.len() as isize - suggestion.span().char().len() as isize;
    }

    chars.into_iter().collect()
}

/// An iterator over references to rules.
pub struct RulesIter<'a> {
    selector: Option<&'a Selector>,
    inner: std::slice::Iter<'a, Rule>,
}

impl<'a> Iterator for RulesIter<'a> {
    type Item = &'a Rule;
    fn next(&mut self) -> Option<Self::Item> {
        let selector = self.selector.as_ref();

        self.inner
            .find(|rule| selector.map_or(true, |s| s.is_match(rule.id())))
    }
}

/// An iterator over mutable references to rules.
pub struct RulesIterMut<'a> {
    selector: Option<&'a Selector>,
    inner: std::slice::IterMut<'a, Rule>,
}

impl<'a> Iterator for RulesIterMut<'a> {
    type Item = &'a mut Rule;
    fn next(&mut self) -> Option<Self::Item> {
        let selector = self.selector.as_ref();

        self.inner
            .find(|rule| selector.map_or(true, |s| s.is_match(rule.id())))
    }
}

impl IntoIterator for Rules {
    type Item = Rule;
    type IntoIter = std::vec::IntoIter<Rule>;
    fn into_iter(self) -> Self::IntoIter {
        self.rules.into_iter()
    }
}

impl<R> FromIterator<R> for Rules
where
    R: Into<Rule>,
{
    fn from_iter<I: IntoIterator<Item = R>>(iter: I) -> Self {
        let rules: Vec<Rule> = iter.into_iter().map(|x| x.into()).collect();
        Self {
            rules,
            synth: None,
            filter_data: Default::default(),
            builtin_lang: None,
            regex_rules: Vec::new(),
        }
    }
}
