//! Runtime implementations of LanguageTool's Java `RuleFilter` classes.
//!
//! Each enum variant mirrors one Java filter family; the logic is ported
//! from LanguageTool 6.5 sources. Filters receive a [FilterCtx] with the
//! sentence, the fired match, its replacements and the language's
//! [FilterData] (speller, multitoken tables, confusion pairs).

use crate::rule::engine::composition::MatchSentence;
use crate::rule::filter_data::{
    levenshtein, osa_distance, remove_diacritics, FilterData, MultitokenSuggester,
};
use crate::rule::synthesizer::Synthesizer;
use crate::tokenizer::tag::Tagger;
use crate::types::{Position, Span, Token};
use crate::utils::regex::Regex;
use serde::{Deserialize, Serialize};
use std::sync::OnceLock;

/// Which language's weekday/month tables a date filter uses.
#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq)]
pub enum DateLang {
    En,
    Ar,
    De,
    Fr,
    Es,
    Ca,
    It,
    Nl,
    Pt,
    Ru,
    Pl,
    Uk,
    Sr,
    Br,
    Eo,
}

/// Variant of the `FindSuggestionsFilter` family.
#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq)]
pub enum FindSuggVariant {
    En,
    Fr,
    Es,
    Ca,
}

/// A parsed LT `<filter class="...">` with its raw `args`
/// (values may contain `\N` backreferences resolved at runtime).
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct JavaFilter {
    pub class: JClass,
    pub args: Vec<(String, String)>,
    /// Args whose values are Java regexes, translated at compile time.
    pub regex_args: Vec<(String, Regex)>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum JClass {
    /// `*DateCheckFilter` (+ the "WithSuggestions" behavior used by most languages).
    DateCheck {
        lang: DateLang,
        with_suggestions: bool,
    },
    DMYDateCheck {
        lang: DateLang,
    },
    YMDDateCheck {
        lang: DateLang,
    },
    FutureDate {
        lang: DateLang,
    },
    NewYearDate {
        lang: DateLang,
        ymd: bool,
    },
    RecentYear,
    DateRange,
    ShortenedYearRange,
    WhitespaceCheck,
    MultitokenSpeller,
    SuppressMisspelledSuggestions,
    FindSuggestions {
        variant: FindSuggVariant,
    },
    AdvancedSynthesizer,
    PartialPosTag,
    AddCommas,
    OrdinalSuffix,
    CompoundCheck,
    NlCompound,
    InnNumber,
    DecadeSpelling,
    UppercaseNounReading,
    ConvertToSentenceCase,
    ConfusionCheck,
    DiacriticsCheck,
    RomanNumeral,
    RegularIrregularParticiple,
    ValidWord,
    RemoveUnknownCompounds,
    SuggestionsRemove,
    MakeContractions,
    NumberInWord,
    /// ca `CatalanNumberSpellerFilter`: replace the suggestion with the
    /// matched number spelled out in Catalan words (SPELLOUT_NUMBERS).
    CatalanNumberSpeller,
    TextToNumber {
        lang: TextNumberLang,
    },
    InterrogativeVerb,
    WordWithDeterminer,
    EnclisisPt,
    ProclisisPt,
    SynthesizeWithDeterminer,
    ConvertToGenderAndNumber,
    PossessiusRedundants,
    InsertCommaDe,
    PotentialCompoundDe,
    PostponedAdjective {
        lang: PostponedAdjLang,
    },
    AdjustPronouns,
    AdjustVerbSuggestions,
    AnarASuggestions,
    DonarTempsSuggestions,
    OblidarseSuggestions,
    PortarGerundiSuggestions,
    PortarTempsSuggestions,
    MasdarToVerbAr,
    VerbToMafoulMutlaqAr,
    AdjectiveToExclamationAr,
    /// ar `ArabicNumberPhraseFilter` (syntax_numeric_0003): number/noun
    /// agreement check; LT 6.5 fires for dual forms followed by a
    /// singular noun and stays silent for plain tens.
    ArabicNumberPhrase,
    AdverbEn,
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextNumberLang {
    Es,
    Ca,
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq)]
pub enum PostponedAdjLang {
    Fr,
    Es,
    Ca,
}

/// Everything a Java filter needs at runtime.
pub struct FilterCtx<'a, 't> {
    pub sentence: &'a MatchSentence<'t>,
    pub tagger: &'a Tagger,
    pub synth: Option<&'a Synthesizer>,
    pub data: &'a FilterData,
    /// Base (already expanded) message of the rule, without filter rewrites.
    pub message: String,
    pub span: Span,
    pub replacements: Vec<String>,
    /// Matched text (the original error string).
    pub matched_text: String,
    /// Indices (into the sentence) of the tokens matched by the pattern.
    pub matched_tokens: Vec<usize>,
    /// Args of the currently applied filter (raw; `\N` unresolved).
    pub(crate) filter_args: Option<Vec<(String, String)>>,
    /// Regex args of the currently applied filter (already Java-translated).
    pub(crate) filter_regex_args: Option<Vec<(String, Regex)>>,
}

impl<'a, 't> FilterCtx<'a, 't> {
    /// The index of the first matched token in the sentence (LT's `patternTokenPos`).
    pub fn pattern_token_pos(&self) -> usize {
        self.matched_tokens.first().copied().unwrap_or(0)
    }

    pub fn arg(&self, key: &str) -> Option<String> {
        self.args_get(key).map(|v| self.resolve_arg(&v))
    }

    pub fn args_get(&self, key: &str) -> Option<String> {
        self.filter_args
            .as_ref()?
            .iter()
            .find(|(k, _)| k == key)
            .map(|(_, v)| v.clone())
    }

    pub fn args_contains(&self, key: &str) -> bool {
        self.args_get(key).is_some()
    }

    /// A pre-compiled regex arg (cloned).
    pub fn regex_arg(&self, key: &str) -> Option<Regex> {
        self.filter_regex_args
            .as_ref()?
            .iter()
            .find(|(k, _)| k == key)
            .map(|(_, v)| v.clone())
    }

    /// Replaces `\N` backreferences with the text of the N-th matched token.
    fn resolve_arg(&self, value: &str) -> String {
        let mut out = String::with_capacity(value.len());
        let chars: Vec<char> = value.chars().collect();
        let mut i = 0;
        while i < chars.len() {
            if chars[i] == '\\' && i + 1 < chars.len() && chars[i + 1].is_ascii_digit() {
                let mut j = i + 1;
                let mut num = 0usize;
                while j < chars.len() && chars[j].is_ascii_digit() {
                    num = num * 10 + (chars[j] as usize - '0' as usize);
                    j += 1;
                }
                if num >= 1 {
                    if let Some(&idx) = self.matched_tokens.get(num - 1) {
                        out.push_str(self.sentence.index(idx).word().as_str());
                    }
                }
                i = j;
            } else {
                out.push(chars[i]);
                i += 1;
            }
        }
        out
    }

    /// Text of the n-th matched token.
    fn token_at_matched(&self, n: usize) -> Option<&str> {
        let &idx = self.matched_tokens.get(n)?;
        Some(self.sentence.index(idx).word().as_str())
    }

    /// Span of the n-th matched token.
    fn token_span_at_matched(&self, n: usize) -> Option<Span> {
        let &idx = self.matched_tokens.get(n)?;
        Some(self.sentence.index(idx).span().clone())
    }

    fn token_has_space_before_at_matched(&self, n: usize) -> bool {
        self.matched_tokens
            .get(n)
            .map(|&idx| self.sentence.index(idx).has_space_before())
            .unwrap_or(false)
    }

    /// Span covering matched tokens n..=m.
    fn matched_range_span(&self, n: usize, m: usize) -> Option<Span> {
        let start = self.token_span_at_matched(n)?;
        let end = self.token_span_at_matched(m)?;
        Some(Span::from_positions(start.start(), end.end()))
    }

    /// LT's `RuleFilter.getPosition`: `marker`/`marker+N` or a plain 1-based
    /// index, over the matched tokens. Returns the 1-based index.
    fn get_position(&self, spec: &str) -> Option<usize> {
        if spec.starts_with("marker") {
            let mut i = 0;
            while i < self.matched_tokens.len() {
                let token = self.sentence.index(self.matched_tokens[i]);
                if token.span().char().start >= self.span.char().start {
                    break;
                }
                i += 1;
            }
            i += 1;
            if spec.len() > "marker".len() {
                let off: usize = spec.trim_start_matches("marker").parse().ok()?;
                i += off;
            }
            Some(i)
        } else {
            spec.parse().ok()
        }
    }

    fn sentence_tokens(&self) -> Vec<&Token<'_>> {
        (0..self.sentence.len()).map(|i| self.sentence.index(i)).collect()
    }

    /// The tokens matched by the pattern (LT passes `Arrays.copyOfRange(
    /// tokens, firstMatchToken, lastMatchToken + 1)` to its filters).
    fn matched_token_refs(&self) -> Vec<&Token<'_>> {
        self.matched_tokens
            .iter()
            .map(|&i| self.sentence.index(i))
            .collect()
    }
}

/// Result of a Java filter: possibly a new span, replacements and message.
pub struct JavaOutcome {
    pub span: Span,
    pub replacements: Vec<String>,
    pub message: Option<String>,
}

fn keep(ctx: &FilterCtx, replacements: Vec<String>) -> Option<JavaOutcome> {
    Some(JavaOutcome {
        span: ctx.span.clone(),
        replacements,
        message: None,
    })
}

impl JavaFilter {
    pub(crate) fn apply(&self, ctx: &mut FilterCtx) -> Option<JavaOutcome> {
        ctx.filter_args = Some(self.args.clone());
        ctx.filter_regex_args = Some(self.regex_args.clone());
        let out = self.class.apply(ctx);
        ctx.filter_args = None;
        ctx.filter_regex_args = None;
        out
    }
}

impl JClass {
    fn apply(&self, ctx: &mut FilterCtx) -> Option<JavaOutcome> {
        let replacements = std::mem::take(&mut ctx.replacements);
        match self {
            JClass::DateCheck {
                lang,
                with_suggestions,
            } => date::date_check(ctx, *lang, *with_suggestions, date::DateInput::Separate),
            JClass::DMYDateCheck { lang } => {
                date::date_check(ctx, *lang, true, date::DateInput::Dmy)
            }
            JClass::YMDDateCheck { lang } => {
                date::date_check(ctx, *lang, true, date::DateInput::Ymd)
            }
            JClass::FutureDate { lang } => date::future_date(ctx, *lang, replacements),
            JClass::NewYearDate { lang, ymd } => date::new_year_date(ctx, *lang, *ymd, replacements),
            JClass::RecentYear => {
                let this_year = now().0;
                let year: i32 = ctx.arg("year")?.parse().ok()?;
                let back: i32 = ctx.arg("maxYearsBack")?.parse().ok()?;
                if year < this_year && year >= this_year - back {
                    keep(ctx, replacements)
                } else {
                    None
                }
            }
            JClass::DateRange => {
                let x: i64 = ctx.arg("x")?.parse().ok()?;
                let y: i64 = ctx.arg("y")?.parse().ok()?;
                if x >= y {
                    keep(ctx, replacements)
                } else {
                    None
                }
            }
            JClass::ShortenedYearRange => {
                let xs = ctx.arg("x")?;
                let ys = ctx.arg("y")?;
                let x: i64 = xs.parse().ok()?;
                let prefix: String = xs.chars().take(2).collect();
                let y: i64 = format!("{}{}", prefix, ys).parse().ok()?;
                if x >= y {
                    keep(ctx, replacements)
                } else {
                    None
                }
            }
            JClass::WhitespaceCheck => {
                let ws_char = ctx.arg("whitespaceChar")?;
                let pos: usize = ctx.arg("position")?.parse().ok()?;
                let tokens = ctx.matched_token_refs();
                let token = tokens.get(pos.checked_sub(1)?)?;
                let has_ws = if token.has_space_before() { " " } else { "" };
                if has_ws != ws_char {
                    keep(ctx, replacements)
                } else {
                    None
                }
            }
            JClass::MultitokenSpeller => multitoken::apply(ctx),
            JClass::SuppressMisspelledSuggestions => suppress_misspelled(ctx, replacements),
            JClass::FindSuggestions { variant } => find_suggestions(ctx, *variant),
            JClass::AdvancedSynthesizer => advanced_synthesizer(ctx, replacements),
            JClass::PartialPosTag => partial_pos_tag(ctx, replacements),
            JClass::AddCommas => add_commas(ctx),
            JClass::OrdinalSuffix => {
                let mut ordinal: String = replacements
                    .first()
                    .map(|x| x.chars().filter(|c| c.is_ascii_digit()).collect())
                    .unwrap_or_default();
                if ordinal.is_empty() {
                    return None;
                }
                if ordinal.ends_with("11") || ordinal.ends_with("12") || ordinal.ends_with("13") {
                    ordinal.push_str("th");
                } else if ordinal.ends_with('1') {
                    ordinal.push_str("st");
                } else if ordinal.ends_with('2') {
                    ordinal.push_str("nd");
                } else if ordinal.ends_with('3') {
                    ordinal.push_str("rd");
                } else {
                    ordinal.push_str("th");
                }
                keep(ctx, vec![ordinal])
            }
            JClass::CompoundCheck => {
                let part1 = ctx.arg("part1")?.to_lowercase();
                let part2 = ctx.arg("part2")?.to_lowercase();
                let ok = ctx
                    .data
                    .added_compound
                    .as_ref()
                    .and_then(|m| m.get(&part1))
                    .map(|list| list.contains(&part2))
                    .unwrap_or(false);
                if ok {
                    keep(ctx, replacements)
                } else {
                    None
                }
            }
            JClass::NlCompound => {
                let mut words = Vec::new();
                for i in 1..=5 {
                    match ctx.arg(&format!("word{}", i)) {
                        Some(w) => words.push(w),
                        None => break,
                    }
                }
                let repl = glue_parts(ctx, &words);
                let message = rewrite_suggestion_element(&ctx.message, &repl);
                keep(ctx, vec![repl]).map(|mut o| {
                    o.message = Some(message);
                    o
                })
            }
            JClass::InnNumber => {
                let inn = ctx.arg("inn")?;
                if !inn.chars().all(|c| c.is_ascii_digit()) {
                    return None;
                }
                let digits: Vec<u32> = inn.chars().map(|c| c.to_digit(10).unwrap()).collect();
                match digits.len() {
                    10 => {
                        let kz = (digits[0] * 2
                            + digits[1] * 4
                            + digits[2] * 10
                            + digits[3] * 3
                            + digits[4] * 5
                            + digits[5] * 9
                            + digits[6] * 4
                            + digits[7] * 6
                            + digits[8] * 8)
                            % 11;
                        let kz = if kz > 9 { kz - 10 } else { kz };
                        if digits[9] != kz {
                            keep(ctx, replacements)
                        } else {
                            None
                        }
                    }
                    12 => {
                        let kz1 = (digits[0] * 7
                            + digits[1] * 2
                            + digits[2] * 4
                            + digits[3] * 10
                            + digits[4] * 3
                            + digits[5] * 5
                            + digits[6] * 9
                            + digits[7] * 4
                            + digits[8] * 6
                            + digits[9] * 8)
                            % 11;
                        let kz2 = (digits[0] * 3
                            + digits[1] * 7
                            + digits[2] * 2
                            + digits[3] * 4
                            + digits[4] * 10
                            + digits[5] * 3
                            + digits[6] * 5
                            + digits[7] * 9
                            + digits[8] * 4
                            + digits[9] * 6
                            + digits[10] * 8)
                            % 11;
                        let kz1 = if kz1 > 9 { kz1 - 10 } else { kz1 };
                        let kz2 = if kz2 > 9 { kz2 - 10 } else { kz2 };
                        if digits[10] != kz1 || digits[11] != kz2 {
                            keep(ctx, replacements)
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            }
            JClass::DecadeSpelling => {
                let lata = ctx.arg("lata")?;
                let decade: String = lata.chars().skip(2).collect();
                let century: String = lata.chars().take(2).collect();
                let cent: i32 = century.parse().ok()?;
                let message = ctx
                    .message
                    .replace("{dekada}", &decade)
                    .replace("{wiek}", &roman_number(cent + 1));
                Some(JavaOutcome {
                    span: ctx.span.clone(),
                    replacements,
                    message: Some(message),
                })
            }
            JClass::UppercaseNounReading => {
                let token = ctx.arg("token")?;
                let uppercase = uppercase_first(&token);
                let has_noun_reading = ctx
                    .tagger
                    .get_tags_with_options(&uppercase, Some(false), Some(false))
                    .any(|data| {
                        let pos = data.pos().as_str();
                        pos.starts_with("SUB:") && !pos.starts_with("ADJ")
                    });
                if has_noun_reading {
                    keep(ctx, replacements)
                } else {
                    None
                }
            }
            JClass::ConvertToSentenceCase => convert_to_sentence_case(ctx),
            JClass::ConfusionCheck => confusion_check(ctx, replacements, false),
            JClass::DiacriticsCheck => confusion_check(ctx, replacements, true),
            JClass::RomanNumeral => {
                let arabic = ctx.arg("arabicSource")?;
                let roman = roman_number_str(&arabic);
                keep(ctx, vec![roman])
            }
            JClass::RegularIrregularParticiple => regular_irregular_participle(ctx, replacements),
            JClass::ValidWord => {
                let word1a = ctx.arg("word1")?;
                let word2a = ctx.arg("word2")?;
                let word1 = format!("{}{}", word1a, word2a);
                let word2 = format!("{}{}", word1a, word2a.to_lowercase());
                let misspelled = |w: &str| !is_known_word_ctx(ctx, w);
                if !misspelled(&word1) || !misspelled(&word2) {
                    None
                } else {
                    keep(ctx, replacements)
                }
            }
            JClass::SuggestionsRemove => suggestions_remove(ctx, replacements),
            JClass::MakeContractions => make_contractions(ctx, replacements),
            JClass::NumberInWord => number_in_word(ctx, replacements),
            JClass::CatalanNumberSpeller => catalan_number_speller(ctx, replacements),
            JClass::TextToNumber { lang } => text_to_number(ctx, *lang, replacements),
            JClass::InterrogativeVerb => interrogative_verb(ctx, replacements),
            JClass::WordWithDeterminer => word_with_determiner(ctx, replacements),
            JClass::EnclisisPt => enclisis_pt(ctx, replacements),
            JClass::ProclisisPt => proclisis_pt(ctx, replacements),
            JClass::SynthesizeWithDeterminer => {
                synthesize_with_determiner(ctx, replacements)
            }
            JClass::ConvertToGenderAndNumber => {
                convert_to_gender_and_number(ctx, replacements)
            }
            JClass::PossessiusRedundants => possessius_redundants(ctx, replacements),
            JClass::InsertCommaDe => insert_comma_de(ctx, replacements),
            JClass::PotentialCompoundDe => potential_compound_de(ctx, replacements),
            JClass::PostponedAdjective { lang } => {
                postponed_adjective(ctx, *lang, replacements)
            }
            JClass::AdjustPronouns => adjust_pronouns(ctx, replacements),
            JClass::AdjustVerbSuggestions => {
                adjust_verb_suggestions(ctx, replacements)
            }
            JClass::AnarASuggestions => anar_a_suggestions(ctx, replacements),
            JClass::DonarTempsSuggestions => donar_temps_suggestions(ctx, replacements),
            JClass::OblidarseSuggestions => olidarse_suggestions(ctx, replacements),
            JClass::PortarGerundiSuggestions => {
                portar_gerundi_suggestions(ctx, replacements)
            }
            JClass::PortarTempsSuggestions => portar_temps_suggestions(ctx, replacements),
            JClass::MasdarToVerbAr => ar_masdar_to_verb(ctx, replacements),
            JClass::VerbToMafoulMutlaqAr => ar_verb_to_mafoul_mutlaq(ctx, replacements),
            JClass::AdjectiveToExclamationAr => ar_adjective_to_exclamation(ctx, replacements),
            JClass::ArabicNumberPhrase => arabic_number_phrase(ctx, replacements),
            JClass::AdverbEn => adverb_filter_en(ctx, replacements),
            JClass::RemoveUnknownCompounds => {
                let compound = format!(
                    "{}{}",
                    ctx.arg("part1")?,
                    ctx.arg("part2")?.to_lowercase()
                );
                if is_known_word_ctx(ctx, &compound) {
                    keep(ctx, replacements)
                } else {
                    None
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// helpers
// ---------------------------------------------------------------------------

/// Current (year, month 1-12, day), honoring `NLPRULE_DATE_ANCHOR=YYYY-MM-DD`
/// so tests can reproduce LT's JUnit-test date hack (2014-01-01).
pub fn now() -> (i32, u32, u32) {
    static ANCHOR: OnceLock<Option<(i32, u32, u32)>> = OnceLock::new();
    let anchor = ANCHOR.get_or_init(|| {
        std::env::var("NLPRULE_DATE_ANCHOR")
            .ok()
            .and_then(|v| parse_ymd(&v))
    });
    if let Some(date) = anchor {
        return *date;
    }
    let secs = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|x| x.as_secs() as i64)
        .unwrap_or(0);
    let days = secs.div_euclid(86400);
    civil_from_days(days)
}

/// Whether the test date anchor is active (LT's `TestHackHelper.isJUnitTest`).
fn test_hack() -> bool {
    std::env::var("NLPRULE_DATE_ANCHOR").is_ok()
}

fn parse_ymd(s: &str) -> Option<(i32, u32, u32)> {
    let mut parts = s.splitn(3, '-');
    let y = parts.next()?.parse().ok()?;
    let m = parts.next()?.parse().ok()?;
    let d = parts.next()?.parse().ok()?;
    Some((y, m, d))
}

/// Howard Hinnant's `civil_from_days`.
pub fn civil_from_days(z: i64) -> (i32, u32, u32) {
    let z = z + 719_468;
    let era = if z >= 0 { z } else { z - 146_096 } / 146_097;
    let doe = (z - era * 146_097) as u64;
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146_096) / 365;
    let y = yoe as i64 + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = (doy - (153 * mp + 2) / 5 + 1) as u32;
    let m = if mp < 10 { mp + 3 } else { mp - 9 } as u32;
    ((y + i64::from(m <= 2)) as i32, m, d)
}

/// Days since the Unix epoch (1970-01-01).
pub fn days_from_civil(y: i32, m: u32, d: u32) -> i64 {
    let y = y as i64 - i64::from(m <= 2);
    let era = if y >= 0 { y } else { y - 399 } / 400;
    let yoe = (y - era * 400) as u64;
    let mp = if m > 2 { m - 3 } else { m + 9 } as u64;
    let doy = (153 * mp + 2) / 5 + d as u64 - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    era * 146_097 + doe as i64 - 719_468
}

fn is_leap(y: i32) -> bool {
    (y % 4 == 0 && y % 100 != 0) || y % 400 == 0
}

fn days_in_month(y: i32, m: u32) -> u32 {
    match m {
        1 | 3 | 5 | 7 | 8 | 10 | 12 => 31,
        4 | 6 | 9 | 11 => 30,
        2 if is_leap(y) => 29,
        2 => 28,
        _ => 0,
    }
}

fn valid_date(y: i32, m: u32, d: u32) -> bool {
    (1..=12).contains(&m) && d >= 1 && d <= days_in_month(y, m)
}

/// Java `Calendar.DAY_OF_WEEK`: Sunday=1 ... Saturday=7.
fn day_of_week(y: i32, m: u32, d: u32) -> u32 {
    let days = days_from_civil(y, m, d);
    // 1970-01-01 was a Thursday (5)
    ((days + 4).rem_euclid(7) + 1) as u32
}

pub(crate) fn uppercase_first(s: &str) -> String {
    let mut chars = s.chars();
    match chars.next() {
        Some(c) => c.to_uppercase().collect::<String>() + chars.as_str(),
        None => String::new(),
    }
}

pub(crate) fn is_capitalized(s: &str) -> bool {
    let mut chars = s.chars();
    match chars.next() {
        Some(c) => {
            c.is_uppercase() && !chars.as_str().is_empty() && chars.all(|x| x.is_lowercase())
        }
        None => false,
    }
}

pub(crate) fn is_all_uppercase(s: &str) -> bool {
    s.chars().any(|c| c.is_alphabetic()) && s.chars().all(|c| !c.is_lowercase())
}

/// `StringTools.preserveCase`.
pub(crate) fn preserve_case(input: &str, sample: &str) -> String {
    if is_all_uppercase(sample) {
        input.to_uppercase()
    } else if sample.chars().next().map(|c| c.is_uppercase()).unwrap_or(false) {
        uppercase_first(input)
    } else {
        input.to_string()
    }
}

/// `StringTools.trimSpecialCharacters`: trim non-alphanumeric chars from both ends.
fn trim_special(s: &str) -> String {
    s.trim_matches(|c: char| !c.is_alphanumeric()).to_string()
}

fn roman_number(n: i32) -> String {
    let numbers = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1];
    let letters = ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"];
    let mut n = n;
    let mut out = String::new();
    for (i, &v) in numbers.iter().enumerate() {
        while n >= v {
            out.push_str(letters[i]);
            n -= v;
        }
    }
    out
}

fn roman_number_str(s: &str) -> String {
    s.parse::<i32>()
        .map(|n| roman_number(n))
        .unwrap_or_else(|_| s.to_string())
}

/// nl `Tools.glueParts`: join compound parts, inserting hyphens between
/// vowels/case changes/digits/hyphens.
fn glue_parts(ctx: &FilterCtx, words: &[String]) -> String {
    const VOWEL_PAIRS: [&str; 21] = [
        "aa", "ae", "ai", "ao", "au", "ee", "ei", "eu", "ée", "éi", "éu", "ie", "ii", "oe", "oi",
        "oo", "ou", "ui", "uu", "ij", "ij",
    ];
    let mut compound = words.first().cloned().unwrap_or_default();
    for word2 in words.iter().skip(1) {
        let spelled = is_known_word_ctx(ctx, &compound);
        if compound.chars().count() > 2 || spelled {
            let last_char = compound.chars().last().unwrap_or(' ');
            let first_char = word2.chars().next().unwrap_or(' ');
            let connection: String = [last_char, first_char].iter().collect();
            let ends_digit = compound.chars().last().map(|c| c.is_ascii_digit()).unwrap_or(false);
            let starts_digit = word2.chars().next().map(|c| c.is_ascii_digit()).unwrap_or(false);
            let needs_hyphen = VOWEL_PAIRS.iter().any(|p| connection.contains(p))
                || (first_char.is_uppercase() && last_char.is_lowercase())
                || (last_char.is_uppercase() && first_char.is_lowercase())
                || (last_char.is_uppercase() && first_char.is_uppercase())
                || ends_digit
                || starts_digit
                || compound.contains('-')
                || word2.contains('-');
            if needs_hyphen {
                compound.push('-');
            }
            compound.push_str(word2);
        } else {
            compound.push_str(word2);
        }
    }
    compound
}

fn rewrite_suggestion_element(message: &str, repl: &str) -> String {
    let mut out = String::new();
    let mut rest = message;
    while let Some(start) = rest.find("<suggestion>") {
        let end = rest[start..]
            .find("</suggestion>")
            .map(|e| start + e + "</suggestion>".len())
            .unwrap_or(rest.len());
        out.push_str(&rest[..start]);
        out.push_str("<suggestion>");
        out.push_str(repl);
        out.push_str("</suggestion>");
        rest = &rest[end..];
    }
    out.push_str(rest);
    out
}

fn is_known_word_ctx(ctx: &FilterCtx, word: &str) -> bool {
    if word.is_empty() {
        return false;
    }
    if let Some(speller) = ctx.data.speller.as_ref() {
        if !speller.is_empty() {
            let mut any = false;
            for token in word.split(|c: char| c.is_whitespace() || c == '-') {
                if token.is_empty() {
                    continue;
                }
                any = true;
                if !speller.is_known(token) {
                    return false;
                }
            }
            if any {
                return true;
            }
        }
    }
    // fall back to the analyzer dictionary
    post_filter_is_known(word, ctx.tagger)
}

fn post_filter_is_known(replacement: &str, tagger: &Tagger) -> bool {
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

/// Java `String.matches`/`matchesPosTagRegex`: full-string regex match.
fn matches_full_regex(regex: &Regex, text: &str) -> bool {
    regex
        .captures(text)
        .and_then(|c| c.get(0))
        .map(|m| m.as_str() == text)
        .unwrap_or(false)
}

fn matches_full(regex: &Regex, text: &str) -> bool {
    regex
        .captures(text)
        .and_then(|c| c.get(0))
        .map(|m| m.as_str() == text)
        .unwrap_or(false)
}

// ---------------------------------------------------------------------------
// date filter family
// ---------------------------------------------------------------------------

mod date {
    use super::*;

    pub enum DateInput {
        Separate,
        Dmy,
        Ymd,
    }

    fn weekday_from_string(lang: DateLang, s: &str) -> Option<u32> {
        let day = trim_special(&s.to_lowercase().replace('\u{00AD}', ""));
        let day = day.as_str();
        let starts = |p: &str| day.starts_with(p);
        let eq = |p: &str| day == p;
        Some(match lang {
            DateLang::Ar if day == "السبت" => 7,
            DateLang::Ar if day == "الأحد" => 1,
            DateLang::Ar if day == "الإثنين" || day == "الاثنين" => 2,
            DateLang::Ar if day == "الثلاثاء" => 3,
            DateLang::Ar if day == "الأربعاء" => 4,
            DateLang::Ar if day == "الخميس" => 5,
            DateLang::Ar if day == "الجمعة" => 6,

            DateLang::En if starts("su") => 1,
            DateLang::En if starts("mo") => 2,
            DateLang::En if starts("tu") => 3,
            DateLang::En if starts("we") => 4,
            DateLang::En if starts("th") => 5,
            DateLang::En if starts("fr") => 6,
            DateLang::En if starts("sa") => 7,

            DateLang::De if starts("sonnabend") => 7,
            DateLang::De if starts("so") => 1,
            DateLang::De if starts("mo") => 2,
            DateLang::De if starts("di") => 3,
            DateLang::De if starts("mi") => 4,
            DateLang::De if starts("do") => 5,
            DateLang::De if starts("fr") => 6,
            DateLang::De if starts("sa") => 7,

            DateLang::Fr if starts("dim") => 1,
            DateLang::Fr if starts("lun") => 2,
            DateLang::Fr if starts("mar") => 3,
            DateLang::Fr if starts("mer") => 4,
            DateLang::Fr if starts("jeu") => 5,
            DateLang::Fr if starts("ven") => 6,
            DateLang::Fr if starts("sam") => 7,

            DateLang::Es if eq("do") || eq("domingo") => 1,
            DateLang::Es if eq("lu") || eq("lunes") => 2,
            DateLang::Es if eq("ma") || eq("martes") => 3,
            DateLang::Es if eq("mi") || eq("miércoles") => 4,
            DateLang::Es if eq("ju") || eq("jueves") => 5,
            DateLang::Es if eq("vi") || eq("viernes") => 6,
            DateLang::Es if eq("sa") || eq("sábado") => 7,

            DateLang::Ca if eq("dg") || eq("diumenge") => 1,
            DateLang::Ca if eq("dl") || eq("dilluns") => 2,
            DateLang::Ca if eq("dt") || eq("dimarts") => 3,
            DateLang::Ca if eq("dc") || eq("dimecres") => 4,
            DateLang::Ca if eq("dj") || eq("dijous") => 5,
            DateLang::Ca if eq("dv") || eq("divendres") => 6,
            DateLang::Ca if eq("ds") || eq("dissabte") => 7,

            DateLang::It if starts("do") => 1,
            DateLang::It if starts("lu") => 2,
            DateLang::It if starts("ma") => 3,
            DateLang::It if starts("me") => 4,
            DateLang::It if starts("gi") => 5,
            DateLang::It if starts("ve") => 6,
            DateLang::It if starts("sa") => 7,

            DateLang::Nl if eq("zo") || eq("zondag") => 1,
            DateLang::Nl if eq("ma") || eq("maandag") => 2,
            DateLang::Nl if eq("di") || eq("dinsdag") => 3,
            DateLang::Nl if eq("wo") || eq("woensdag") => 4,
            DateLang::Nl if eq("do") || eq("donderdag") => 5,
            DateLang::Nl if eq("vr") || eq("vrijdag") => 6,
            DateLang::Nl if eq("za") || eq("zaterdag") => 7,

            DateLang::Pt if starts("dom") => 1,
            DateLang::Pt if starts("seg") => 2,
            DateLang::Pt if starts("ter") => 3,
            DateLang::Pt if starts("qua") => 4,
            DateLang::Pt if starts("qui") => 5,
            DateLang::Pt if starts("sex") => 6,
            DateLang::Pt if starts("sáb") => 7,

            DateLang::Ru if starts("суб") || starts("сб") => 7,
            DateLang::Ru if starts("вс") || starts("вос") => 1,
            DateLang::Ru if starts("пн") || starts("пон") => 2,
            DateLang::Ru if starts("вт") => 3,
            DateLang::Ru if starts("ср") => 4,
            DateLang::Ru if starts("чт") || starts("чет") => 5,
            DateLang::Ru if starts("пт") || starts("пят") => 6,

            DateLang::Pl if starts("pon") => 2,
            DateLang::Pl if starts("wt") => 3,
            DateLang::Pl if starts("śr") => 4,
            DateLang::Pl if starts("czw") => 5,
            DateLang::Pl if eq("pt") || starts("piątk") || eq("piątek") => 6,
            DateLang::Pl if starts("sob") => 7,
            DateLang::Pl if starts("niedz") => 1,

            DateLang::Uk if starts("по") || eq("пн") => 2,
            DateLang::Uk if starts("ві") || eq("вт") => 3,
            DateLang::Uk if starts("се") || eq("ср") => 4,
            DateLang::Uk if starts("че") || eq("чт") => 5,
            DateLang::Uk if starts("п'") || starts("п’") || eq("пт") => 6,
            DateLang::Uk if starts("су") || eq("сб") => 7,
            DateLang::Uk if starts("не") || eq("нд") => 1,

            DateLang::Sr if starts("по") => 2,
            DateLang::Sr if starts("ут") => 3,
            DateLang::Sr if starts("ср") => 4,
            DateLang::Sr if starts("че") => 5,
            DateLang::Sr if starts("пе") => 6,
            DateLang::Sr if starts("су") => 7,
            DateLang::Sr if starts("не") => 1,

            DateLang::Br if day.ends_with("sul") => 1,
            DateLang::Br if day.ends_with("lun") => 2,
            DateLang::Br if day.ends_with("meurzh") => 3,
            DateLang::Br if day.ends_with("merc’her") => 4,
            DateLang::Br if eq("yaou") || eq("diriaou") => 5,
            DateLang::Br if day.ends_with("gwener") => 6,
            DateLang::Br if day.ends_with("sadorn") => 7,

            DateLang::Eo if starts("dim") => 1,
            DateLang::Eo if starts("lun") => 2,
            DateLang::Eo if starts("mar") => 3,
            DateLang::Eo if starts("mer") => 4,
            DateLang::Eo if starts("ĵaŭ") || starts("jau") || starts("jhau") || starts("jxau") => 5,
            DateLang::Eo if starts("ven") => 6,
            DateLang::Eo if starts("sab") => 7,

            _ => return None,
        })
    }

    fn month_from_string(lang: DateLang, s: &str) -> Option<u32> {
        let mon = trim_special(&s.to_lowercase().replace('\u{00AD}', ""));
        let mon = mon.as_str();
        let starts = |p: &str| mon.starts_with(p);
        let eq = |p: &str| mon == p;
        Some(match lang {
            DateLang::Ar if eq("كانون الثاني") || eq("كانون ثاني") || eq("يناير") || eq("جانفي") || eq("جانفييه") => 1,
            DateLang::Ar if eq("شباط") || eq("فبراير") || eq("فيفري") => 2,
            DateLang::Ar if eq("آذار") || eq("مارس") => 3,
            DateLang::Ar if eq("نيسان") || eq("أبريل") || eq("أفريل") => 4,
            DateLang::Ar if eq("أيار") || eq("مايو") || eq("ماي") => 5,
            DateLang::Ar if eq("حزيران") || eq("يونيو") || eq("جوان") => 6,
            DateLang::Ar if eq("تموز") || eq("يوليو") || eq("جويلية") => 7,
            DateLang::Ar if eq("آب") || eq("أغسطس") || eq("أوت") => 8,
            DateLang::Ar if eq("أيلول") || eq("سبتمبر") => 9,
            DateLang::Ar if eq("تشرين الأول") => 10,
            DateLang::Ar if eq("تشرين الثاني") || eq("تشرين ثاني") || eq("نوفمبر") => 11,
            DateLang::Ar if eq("كانون الأول") || eq("كانون أول") || eq("ديسمبر") => 12,

            DateLang::En if starts("jan") => 1,
            DateLang::En if starts("feb") => 2,
            DateLang::En if starts("mar") => 3,
            DateLang::En if starts("apr") => 4,
            DateLang::En if starts("may") => 5,
            DateLang::En if starts("jun") => 6,
            DateLang::En if starts("jul") => 7,
            DateLang::En if starts("aug") => 8,
            DateLang::En if starts("sep") => 9,
            DateLang::En if starts("oct") => 10,
            DateLang::En if starts("nov") => 11,
            DateLang::En if starts("dec") => 12,

            DateLang::De if starts("jän") || starts("jan") => 1,
            DateLang::De if starts("feb") => 2,
            DateLang::De if starts("mär") => 3,
            DateLang::De if starts("apr") => 4,
            DateLang::De if starts("mai") => 5,
            DateLang::De if starts("jun") => 6,
            DateLang::De if starts("jul") => 7,
            DateLang::De if starts("aug") => 8,
            DateLang::De if starts("sep") => 9,
            DateLang::De if starts("okt") => 10,
            DateLang::De if starts("nov") => 11,
            DateLang::De if starts("dez") => 12,

            DateLang::Fr if starts("jan") => 1,
            DateLang::Fr if starts("fév") || starts("fev") => 2,
            DateLang::Fr if starts("mar") => 3,
            DateLang::Fr if starts("avr") => 4,
            DateLang::Fr if starts("mai") => 5,
            DateLang::Fr if starts("juin") => 6,
            DateLang::Fr if starts("juil") => 7,
            DateLang::Fr if starts("aou") || starts("aoû") => 8,
            DateLang::Fr if starts("sep") => 9,
            DateLang::Fr if starts("oct") => 10,
            DateLang::Fr if starts("nov") => 11,
            DateLang::Fr if starts("déc") || starts("dec") => 12,

            DateLang::Es if starts("en") => 1,
            DateLang::Es if starts("fe") => 2,
            DateLang::Es if starts("mar") || starts("mzo") => 3,
            DateLang::Es if starts("ab") => 4,
            DateLang::Es if starts("may") || starts("my") => 5,
            DateLang::Es if starts("jun") || eq("jn") => 6,
            DateLang::Es if starts("jul") || eq("jl") => 7,
            DateLang::Es if starts("ag") => 8,
            DateLang::Es if starts("se") || starts("sep") => 9,
            DateLang::Es if starts("oc") => 10,
            DateLang::Es if starts("no") => 11,
            DateLang::Es if starts("di") => 12,

            DateLang::Ca if starts("gen") => 1,
            DateLang::Ca if starts("febr") => 2,
            DateLang::Ca if starts("març") => 3,
            DateLang::Ca if starts("abr") => 4,
            DateLang::Ca if starts("maig") => 5,
            DateLang::Ca if starts("juny") => 6,
            DateLang::Ca if starts("jul") => 7,
            DateLang::Ca if starts("ag") => 8,
            DateLang::Ca if starts("set") => 9,
            DateLang::Ca if starts("oct") => 10,
            DateLang::Ca if starts("nov") => 11,
            DateLang::Ca if starts("des") => 12,

            DateLang::It if starts("gen") => 1,
            DateLang::It if starts("feb") => 2,
            DateLang::It if starts("mar") => 3,
            DateLang::It if starts("apr") => 4,
            DateLang::It if starts("mag") => 5,
            DateLang::It if starts("giu") => 6,
            DateLang::It if starts("lug") => 7,
            DateLang::It if starts("ago") => 8,
            DateLang::It if starts("set") => 9,
            DateLang::It if starts("ott") => 10,
            DateLang::It if starts("nov") => 11,
            DateLang::It if starts("dic") => 12,

            DateLang::Nl if starts("jan") => 1,
            DateLang::Nl if starts("feb") => 2,
            DateLang::Nl if starts("maa") || starts("mrt") || starts("mar") => 3,
            DateLang::Nl if starts("apr") => 4,
            DateLang::Nl if starts("mei") => 5,
            DateLang::Nl if starts("jun") => 6,
            DateLang::Nl if starts("jul") => 7,
            DateLang::Nl if starts("aug") => 8,
            DateLang::Nl if starts("sep") => 9,
            DateLang::Nl if starts("okt") || starts("oct") => 10,
            DateLang::Nl if starts("nov") => 11,
            DateLang::Nl if starts("dec") => 12,

            DateLang::Pt if starts("jan") => 1,
            DateLang::Pt if starts("fev") => 2,
            DateLang::Pt if starts("mar") => 3,
            DateLang::Pt if starts("abr") => 4,
            DateLang::Pt if starts("mai") => 5,
            DateLang::Pt if starts("jun") => 6,
            DateLang::Pt if starts("jul") => 7,
            DateLang::Pt if starts("ago") => 8,
            DateLang::Pt if starts("set") => 9,
            DateLang::Pt if starts("out") => 10,
            DateLang::Pt if starts("nov") => 11,
            DateLang::Pt if starts("dez") => 12,

            DateLang::Ru if starts("янв") => 1,
            DateLang::Ru if starts("фев") => 2,
            DateLang::Ru if starts("мар") => 3,
            DateLang::Ru if starts("апр") => 4,
            DateLang::Ru if starts("май") || starts("мая") => 5,
            DateLang::Ru if starts("июн") => 6,
            DateLang::Ru if starts("июл") => 7,
            DateLang::Ru if starts("авг") => 8,
            DateLang::Ru if starts("сен") => 9,
            DateLang::Ru if starts("окт") => 10,
            DateLang::Ru if starts("ноя") => 11,
            DateLang::Ru if starts("дек") => 12,

            DateLang::Pl if eq("stycznia") || eq("i") => 1,
            DateLang::Pl if eq("lutego") || eq("ii") => 2,
            DateLang::Pl if eq("marca") || eq("iii") => 3,
            DateLang::Pl if eq("kwietnia") || eq("iv") => 4,
            DateLang::Pl if eq("maja") || eq("v") => 5,
            DateLang::Pl if eq("czerwca") || eq("vi") => 6,
            DateLang::Pl if eq("lipca") || eq("vii") => 7,
            DateLang::Pl if eq("sierpnia") || eq("viii") => 8,
            DateLang::Pl if eq("września") || eq("ix") => 9,
            DateLang::Pl if eq("października") || eq("x") => 10,
            DateLang::Pl if eq("listopada") || eq("xi") => 11,
            DateLang::Pl if eq("grudnia") || eq("xii") => 12,

            DateLang::Uk if starts("сі") => 1,
            DateLang::Uk if starts("лю") => 2,
            DateLang::Uk if starts("бе") => 3,
            DateLang::Uk if starts("кв") => 4,
            DateLang::Uk if starts("тр") => 5,
            DateLang::Uk if starts("че") => 6,
            DateLang::Uk if starts("ли") => 7,
            DateLang::Uk if starts("се") => 8,
            DateLang::Uk if starts("ве") => 9,
            DateLang::Uk if starts("жо") => 10,
            DateLang::Uk if starts("гр") => 11,

            DateLang::Sr
                if mon == "јануар"
                    || mon == "i"
                    || mon == "јануара"
                    || mon == "јан" =>
            {
                1
            }
            DateLang::Sr
                if mon == "фебруар" || mon == "ii" || mon == "фебруара" || mon == "феб" =>
            {
                2
            }
            DateLang::Sr if mon == "март" || mon == "iii" || mon == "марта" || mon == "мар" => 3,
            DateLang::Sr if mon == "април" || mon == "iv" || mon == "априла" || mon == "апр" => 4,
            DateLang::Sr if mon == "мај" || mon == "v" || mon == "маја" => 5,
            DateLang::Sr if mon == "јун" || mon == "vi" || mon == "јуна" => 6,
            DateLang::Sr if mon == "јул" || mon == "vii" || mon == "јула" => 7,
            DateLang::Sr if mon == "август" || mon == "viii" || mon == "августа" || mon == "авг" => {
                8
            }
            DateLang::Sr
                if mon == "септембар" || mon == "ix" || mon == "септембра" || mon == "сеп" =>
            {
                9
            }
            DateLang::Sr
                if mon == "октобар" || mon == "x" || mon == "октобра" || mon == "окт" =>
            {
                10
            }
            DateLang::Sr
                if mon == "новембар" || mon == "xi" || mon == "новембра" || mon == "нов" =>
            {
                11
            }
            DateLang::Sr
                if mon == "децембар" || mon == "xii" || mon == "децембра" || mon == "дец" =>
            {
                12
            }

            DateLang::Br if mon == "genver" => 1,
            DateLang::Br if mon == "c’hwevrer" => 2,
            DateLang::Br if mon == "meurzh" => 3,
            DateLang::Br if mon == "ebrel" => 4,
            DateLang::Br if mon == "mae" => 5,
            DateLang::Br if mon == "mezheven" || mon == "even" => 6,
            DateLang::Br if mon == "gouere" || mon == "gouhere" => 7,
            DateLang::Br if mon == "eost" => 8,
            DateLang::Br if mon == "gwengolo" => 9,
            DateLang::Br if mon == "here" => 10,
            DateLang::Br if mon == "du" => 11,
            DateLang::Br if mon == "kerzu" => 12,

            DateLang::Eo if starts("jan") => 1,
            DateLang::Eo if starts("feb") => 2,
            DateLang::Eo if starts("mar") => 3,
            DateLang::Eo if starts("apr") => 4,
            DateLang::Eo if starts("maj") => 5,
            DateLang::Eo if starts("jun") => 6,
            DateLang::Eo if starts("jul") => 7,
            DateLang::Eo if starts("aŭg") => 8,
            DateLang::Eo if starts("sep") => 9,
            DateLang::Eo if starts("okt") => 10,
            DateLang::Eo if starts("nov") => 11,
            DateLang::Eo if starts("dec") => 12,

            _ => return None,
        })
    }

    /// Localized display name of a weekday (1=Sunday..7=Saturday).
    fn weekday_display(lang: DateLang, dow: u32) -> &'static str {
        const EN: [&str; 7] = [
            "Sunday",
            "Monday",
            "Tuesday",
            "Wednesday",
            "Thursday",
            "Friday",
            "Saturday",
        ];
        const DE: [&str; 7] = [
            "Sonntag",
            "Montag",
            "Dienstag",
            "Mittwoch",
            "Donnerstag",
            "Freitag",
            "Samstag",
        ];
        const FR: [&str; 7] = [
            "dimanche",
            "lundi",
            "mardi",
            "mercredi",
            "jeudi",
            "vendredi",
            "samedi",
        ];
        const ES: [&str; 7] = [
            "domingo",
            "lunes",
            "martes",
            "miércoles",
            "jueves",
            "viernes",
            "sábado",
        ];
        const CA: [&str; 7] = [
            "diumenge",
            "dilluns",
            "dimarts",
            "dimecres",
            "dijous",
            "divendres",
            "dissabte",
        ];
        const IT: [&str; 7] = [
            "domenica",
            "lunedì",
            "martedì",
            "mercoledì",
            "giovedì",
            "venerdì",
            "sabato",
        ];
        const NL: [&str; 7] = [
            "zondag",
            "maandag",
            "dinsdag",
            "woensdag",
            "donderdag",
            "vrijdag",
            "zaterdag",
        ];
        const PT: [&str; 7] = [
            "domingo",
            "segunda-feira",
            "terça-feira",
            "quarta-feira",
            "quinta-feira",
            "sexta-feira",
            "sábado",
        ];
        const RU: [&str; 7] = [
            "воскресенье",
            "понедельник",
            "вторник",
            "среда",
            "четверг",
            "пятница",
            "суббота",
        ];
        const PL: [&str; 7] = [
            "niedziela",
            "poniedziałek",
            "wtorek",
            "środa",
            "czwartek",
            "piątek",
            "sobota",
        ];
        const UK: [&str; 7] = [
            "неділя",
            "понеділок",
            "вівторок",
            "середа",
            "четвер",
            "п'ятниця",
            "субота",
        ];
        const SR: [&str; 7] = [
            "недеља",
            "понедељак",
            "уторак",
            "среда",
            "четвртак",
            "петак",
            "субота",
        ];
        const BR: [&str; 7] = [
            "Sul",
            "Lun",
            "Meurzh",
            "Merc’her",
            "Yaou",
            "Gwener",
            "Sadorn",
        ];
        const EO: [&str; 7] = [
            "dimanĉo",
            "lundo",
            "mardo",
            "merkredo",
            "jaŭdo",
            "vendredo",
            "sabato",
        ];
        const AR: [&str; 7] = [
            "الأحد",
            "الاثنين",
            "الثلاثاء",
            "الأربعاء",
            "الخميس",
            "الجمعة",
            "السبت",
        ];
        let idx = dow.saturating_sub(1) as usize;
        match lang {
            DateLang::Ar => AR[idx],
            DateLang::En => EN[idx],
            DateLang::De => DE[idx],
            DateLang::Fr => FR[idx],
            DateLang::Es => ES[idx],
            DateLang::Ca => CA[idx],
            DateLang::It => IT[idx],
            DateLang::Nl => NL[idx],
            DateLang::Pt => PT[idx],
            DateLang::Ru => RU[idx],
            DateLang::Pl => PL[idx],
            DateLang::Uk => UK[idx],
            DateLang::Sr => SR[idx],
            DateLang::Br => BR[idx],
            DateLang::Eo => EO[idx],
        }
    }

    /// Parses the day of the month: digits with optional suffix, or
    /// localized number words (br/eo).
    fn day_of_month(lang: DateLang, s: &str) -> Option<u32> {
        let s = s.replace('\u{00AD}', "");
        let digits: String = s.chars().take_while(|c| c.is_ascii_digit()).collect();
        if !digits.is_empty() {
            return digits.parse().ok();
        }
        let day = s.to_lowercase();
        let day = day.as_str();
        match lang {
            DateLang::Br => {
                let mut day = day.to_string();
                if day.starts_with('t') {
                    day.replace_range(0..1, "d");
                }
                if day.starts_with('p') {
                    day.replace_range(0..1, "b");
                }
                if day.ends_with("vet") {
                    let n = day.len() - 3;
                    day.truncate(n);
                }
                let day = day.as_str();
                Some(match day {
                    "c’hentañ" | "unan" => 1,
                    "daou" | "eil" => 2,
                    "dri" | "drede" | "deir" => 3,
                    "bevar" => 4,
                    "bemp" | "bem" => 5,
                    "c’hwerc’h" => 6,
                    "seizh" => 7,
                    "eizh" => 8,
                    "nav" | "na" => 9,
                    "dek" => 10,
                    "unnek" => 11,
                    "daouzek" => 12,
                    "drizek" => 13,
                    "bevarzek" => 14,
                    "bemzek" => 15,
                    "c’hwezek" => 16,
                    "seitek" => 17,
                    "driwec’h" => 18,
                    "naontek" => 19,
                    "ugent" => 20,
                    "dregont" => 30,
                    _ => 0,
                })
            }
            DateLang::Eo => {
                let mut day = day.to_string();
                if day.ends_with('n') {
                    day.truncate(day.len() - 1);
                }
                let mut n = 0u32;
                if day.starts_with("dek") {
                    n = 10;
                    day.replace_range(0..3, "");
                } else if day.starts_with("dudek") {
                    n = 20;
                    day.replace_range(0..5, "");
                } else if day.starts_with("tridek") {
                    n = 30;
                    day.replace_range(0..6, "");
                }
                if n > 0 && day.starts_with('-') {
                    day.replace_range(0..1, "");
                }
                let day = day.as_str();
                n += match day {
                    "unua" => 1,
                    "dua" => 2,
                    "tria" => 3,
                    "kvara" => 4,
                    "kvina" => 5,
                    "sesa" => 6,
                    "sepa" => 7,
                    "oka" => 8,
                    "naŭa" | "nauxa" | "naua" => 9,
                    _ => 0,
                };
                Some(n)
            }
            _ => Some(0),
        }
    }

    fn error_message_wrong_year(lang: DateLang) -> &'static str {
        match lang {
            DateLang::En => "This date is wrong. Did you mean \"{currentYear}\"?",
            DateLang::De => {
                "Dieses Datum stimmt nicht mit dem Tag überein. Meinten Sie \"{currentYear}\"?"
            }
            DateLang::Fr => {
                "Cette date est incorrecte. Faites-vous référence à l'année \"{currentYear}\" ?"
            }
            DateLang::Es => "Este fecha no es correcta. ¿Se refería al año \"{currentYear}\"?",
            DateLang::Ca => "Aquesta data no és correcta. ¿Us referiu a l'any \"{currentYear}\"?",
            DateLang::Nl => "Deze datum is onjuist. Bedoelt u misschien \"{currentYear}\"?",
            DateLang::Pt => {
                "Esta data está incorreta. Você está se referindo ao ano \"{currentYear}\"?"
            }
            _ => "This date is wrong. Did you mean \"{currentYear}\"?",
        }
    }

    fn adjust_suggestion_de(sugg: &str) -> String {
        let dot_comma = sugg.find(".,");
        if let Some(pos) = dot_comma {
            if pos > 5 && pos < 12 {
                return sugg.replace(".,", ",");
            }
        }
        if dot_comma.is_none() {
            if let Some(pos) = sugg.find(',') {
                if pos > 0 && pos < 5 {
                    return sugg.replace(",", ".,");
                }
            }
        }
        sugg.to_string()
    }

    fn day_str_like_original_en(day: &str, original: &str) -> String {
        if original.chars().all(|c| c.is_ascii_digit()) {
            return day.to_string();
        }
        let n: i32 = day.parse().unwrap_or(0);
        if (11..=13).contains(&n) {
            return format!("{}th", n);
        }
        match n % 10 {
            1 => format!("{}st", n),
            2 => format!("{}nd", n),
            3 => format!("{}rd", n),
            _ => format!("{}th", n),
        }
    }

    fn parse_month(lang: DateLang, s: &str) -> Option<u32> {
        if s.chars().all(|c| c.is_ascii_digit()) && !s.is_empty() {
            s.parse().ok()
        } else {
            month_from_string(lang, s)
        }
    }

    pub fn future_date(
        ctx: &mut FilterCtx,
        lang: DateLang,
        replacements: Vec<String>,
    ) -> Option<JavaOutcome> {
        let year_str = ctx.arg("year")?;
        if std::env::var("NLPRULE_DEBUG_FILTER").is_ok() {
            eprintln!(
                "FutureDateFilter: year={:?} month={:?} day={:?} matched_tokens={:?} span={:?}",
                year_str,
                ctx.arg("month"),
                ctx.arg("day"),
                ctx.matched_tokens,
                ctx.span
            );
        }
        let month_str = ctx.arg("month")?;
        let day_str = ctx.arg("day")?;

        let year: i32 = year_str.parse().ok()?;
        let month = parse_month(lang, &month_str)?;
        let day = day_of_month(lang, &day_str)?;

        if !valid_date(year, month, day) {
            return None;
        }
        let (ny, nm, nd) = now();
        let date_days = days_from_civil(year, month, day);
        let now_days = days_from_civil(ny, nm, nd);
        if date_days > now_days {
            keep(ctx, replacements)
        } else {
            None
        }
    }

    pub fn new_year_date(
        ctx: &mut FilterCtx,
        lang: DateLang,
        ymd: bool,
        replacements: Vec<String>,
    ) -> Option<JavaOutcome> {
        let (year_str, month_str, day_str) = if ymd {
            let date = ctx.arg("date")?;
            let mut parts = date.split('-');
            let y = parts.next()?.to_string();
            let m = parts.next()?.to_string();
            let d = parts.next()?.to_string();
            (Some(y), m, d)
        } else {
            (ctx.arg("year"), ctx.arg("month")?, ctx.arg("day")?)
        };

        let year: i32 = year_str.as_ref()?.parse().ok()?;
        let month = parse_month(lang, &month_str)?;
        let day = day_of_month(lang, &day_str)?;

        if !valid_date(year, month, day) {
            return None;
        }

        let (cur_year, cur_month, _) = now();
        if cur_month == 1 && month != 12 && year + 1 == cur_year {
            let mut message = ctx
                .message
                .replace("{year}", &year.to_string())
                .replace("{realYear}", &cur_year.to_string());
            if ymd {
                let real_date = format!("{}-{}-{}", year + 1, month_str, day_str);
                message = message.replace("{realDate}", &real_date);
            }
            Some(JavaOutcome {
                span: ctx.span.clone(),
                replacements,
                message: Some(message),
            })
        } else {
            None
        }
    }

    pub fn date_check(
        ctx: &mut FilterCtx,
        lang: DateLang,
        with_suggestions: bool,
        input: DateInput,
    ) -> Option<JavaOutcome> {
        if !with_suggestions {
            date_check_plain(ctx, lang, input)
        } else {
            date_check_with_suggestions(ctx, lang, input)
        }
    }

    fn date_check_plain(
        ctx: &mut FilterCtx,
        lang: DateLang,
        input: DateInput,
    ) -> Option<JavaOutcome> {
        let (day_str, month_str, year_str) = match input {
            DateInput::Separate => (
                ctx.arg("day")?,
                ctx.arg("month")?,
                ctx.arg("year"),
            ),
            DateInput::Dmy | DateInput::Ymd => {
                let date = ctx.arg("date")?;
                let mut iter = date.split('-');
                let a = iter.next()?.to_string();
                let b = iter.next()?.to_string();
                let c = iter.next()?.to_string();
                if let DateInput::Dmy = input {
                    (a, b, Some(c))
                } else {
                    (c, b, Some(a))
                }
            }
        };

        let weekday_str = ctx.arg("weekDay")?;
        let dow_str = weekday_from_string(lang, &weekday_str)?;
        let month = parse_month(lang, &month_str)?;
        let day = day_of_month(lang, &day_str)?;
        let year: i32 = match year_str {
            Some(y) => y.parse().ok()?,
            None => {
                if test_hack() {
                    2014
                } else {
                    now().0
                }
            }
        };

        if !valid_date(year, month, day) {
            return None;
        }

        let dow_date = day_of_week(year, month, day);
        if dow_str != dow_date {
            let current_year = if test_hack() { 2014 } else { now().0 };
            let message = ctx
                .message
                .replace("{realDay}", weekday_display(lang, dow_date))
                .replace("{day}", weekday_display(lang, dow_str))
                .replace("{currentYear}", &current_year.to_string());
            Some(JavaOutcome {
                span: ctx.span.clone(),
                replacements: std::mem::take(&mut ctx.replacements),
                message: Some(message),
            })
        } else {
            None
        }
    }

    fn date_check_with_suggestions(
        ctx: &mut FilterCtx,
        lang: DateLang,
        input: DateInput,
    ) -> Option<JavaOutcome> {
        let weekday_pos: usize = ctx.arg("weekDay")?.parse().ok()?;
        let date_pos: Option<usize> = ctx
            .arg("date")
            .and_then(|x| x.parse::<i64>().ok())
            .and_then(|x| if x >= 0 { Some(x as usize) } else { None });

        let (day_pos, month_pos, year_pos, day_str, month_str, year_str, full_date) =
            if let Some(dp) = date_pos {
                let token = ctx.token_at_matched(dp)?;
                let parts: Vec<&str> = token.split('-').collect();
                if parts.len() != 3 {
                    return None;
                }
                (
                    dp,
                    dp,
                    Some(dp),
                    parts[2].to_string(),
                    parts[1].to_string(),
                    Some(parts[0].to_string()),
                    true,
                )
            } else if let DateInput::Dmy = input {
                let date = ctx.arg("date")?;
                let mut iter = date.split('-');
                let d = iter.next()?.to_string();
                let m = iter.next()?.to_string();
                let y = iter.next()?.to_string();
                let dp: usize = ctx.args_get("date").and_then(|x| x.parse().ok())?;
                (dp, dp, Some(dp), d, m, Some(y), true)
            } else {
                let day_pos: usize = ctx.arg("day")?.parse().ok()?;
                let month_pos: usize = ctx.arg("month")?.parse().ok()?;
                let year_pos: Option<usize> = ctx
                    .arg("year")
                    .and_then(|x| x.parse::<i64>().ok())
                    .and_then(|x| if x >= 0 { Some(x as usize) } else { None });
                let day_str = ctx.token_at_matched(day_pos)?.to_string();
                let month_str = ctx.token_at_matched(month_pos)?.to_string();
                let year_str = match year_pos {
                    Some(yp) => Some(ctx.token_at_matched(yp)?.to_string()),
                    None => None,
                };
                (day_pos, month_pos, year_pos, day_str, month_str, year_str, false)
            };

        let weekday_str = ctx.token_at_matched(weekday_pos)?.to_string();
        let dow_str = weekday_from_string(lang, &weekday_str.replace('\u{00AD}', ""))?;

        let month = parse_month(lang, &month_str)?;
        let day = day_of_month(lang, &day_str.replace('\u{00AD}', ""))?;
        let year: i32 = match &year_str {
            Some(y) => y.parse().ok()?,
            None => {
                if test_hack() {
                    2014
                } else {
                    now().0
                }
            }
        };

        if !valid_date(year, month, day) {
            return None;
        }

        let dow_date = day_of_week(year, month, day);
        if dow_str == dow_date {
            return None;
        }

        let current_year = if test_hack() { 2014 } else { now().0 };

        // suggest changing the year (to the current year)
        if valid_date(current_year, month, day)
            && dow_str == day_of_week(current_year, month, day)
        {
            let message = error_message_wrong_year(lang)
                .replace("{currentYear}", &current_year.to_string());
            let span = ctx.token_span_at_matched(year_pos.unwrap_or(day_pos))?;
            let replacement = if full_date {
                format!("{}-{}-{}", current_year, month_str, day_str)
            } else {
                current_year.to_string()
            };
            return Some(JavaOutcome {
                span,
                replacements: vec![replacement],
                message: Some(message),
            });
        }

        // suggest changing day of week or day of month
        let message = ctx
            .message
            .clone()
            .replace("{realDay}", weekday_display(lang, dow_date))
            .replace("{day}", weekday_display(lang, dow_str))
            .replace("{currentYear}", &current_year.to_string());

        let (start_idx, end_idx) = if weekday_pos < day_pos {
            (weekday_pos, day_pos)
        } else {
            (day_pos, weekday_pos)
        };
        let span = ctx.matched_range_span(start_idx, end_idx)?;

        let mut suggestions = Vec::new();

        // suggest changing day of week
        let mut suggestion = String::new();
        for j in start_idx..=end_idx {
            if let Some(text) = ctx.token_at_matched(j) {
                if j > start_idx && ctx.token_has_space_before_at_matched(j) {
                    suggestion.push(' ');
                }
                if j == weekday_pos {
                    suggestion.push_str(&preserve_case(weekday_display(lang, dow_date), text));
                } else {
                    suggestion.push_str(text);
                }
            }
        }
        if !suggestion.is_empty() {
            suggestions.push(if lang == DateLang::De {
                adjust_suggestion_de(&suggestion)
            } else {
                suggestion
            });
        }

        // suggest changing day of month
        let mut corrected_day = String::new();
        let mut diff = 1u32;
        while diff < 7 {
            if day > diff
                && valid_date(year, month, day - diff)
                && dow_str == day_of_week(year, month, day - diff)
            {
                corrected_day = (day - diff).to_string();
                break;
            }
            if day + diff < 32
                && valid_date(year, month, day + diff)
                && dow_str == day_of_week(year, month, day + diff)
            {
                corrected_day = (day + diff).to_string();
                break;
            }
            diff += 1;
        }
        if !corrected_day.is_empty() {
            let mut suggestion = String::new();
            for j in start_idx..=end_idx {
                if let Some(text) = ctx.token_at_matched(j) {
                    if j > start_idx && ctx.token_has_space_before_at_matched(j) {
                        suggestion.push(' ');
                    }
                    if j == day_pos {
                        if full_date {
                            suggestion.push_str(&format!(
                                "{}-{}-{}",
                                year_str.clone().unwrap_or_default(),
                                month_str,
                                corrected_day
                            ));
                        } else if lang == DateLang::En {
                            suggestion
                                .push_str(&day_str_like_original_en(&corrected_day, &day_str));
                        } else {
                            suggestion.push_str(&corrected_day);
                        }
                    } else {
                        suggestion.push_str(text);
                    }
                }
            }
            if !suggestion.is_empty() {
                suggestions.push(if lang == DateLang::De {
                    adjust_suggestion_de(&suggestion)
                } else {
                    suggestion
                });
            }
        }

        Some(JavaOutcome {
            span,
            replacements: suggestions,
            message: Some(message),
        })
    }
}

// ---------------------------------------------------------------------------
// multitoken speller
// ---------------------------------------------------------------------------

mod multitoken {
    use super::*;

    const MAX_LENGTH_DIFF: usize = 3;

    pub fn apply(ctx: &mut FilterCtx) -> Option<JavaOutcome> {
        let suggester = ctx.data.multitoken.as_ref()?;
        if suggester.is_empty() {
            return None;
        }

        let underlined_error = ctx.matched_text.clone();
        if std::env::var("NLPRULE_DEBUG_FILTER").is_ok() {
            eprintln!("MultitokenSpellerFilter: error={:?} data={}", underlined_error, ctx.data.multitoken.is_some());
        }

        let all_accepted = ctx.data.multitoken_speller_check
            && !underlined_error
                .split_whitespace()
                .any(|w| !is_known_word_ctx(ctx, w));

        let replacements = get_suggestions(suggester, ctx, &underlined_error, all_accepted);
        if std::env::var("NLPRULE_DEBUG_FILTER").is_ok() {
            eprintln!("MultitokenSpellerFilter: suggestions={:?}", replacements);
        }
        if replacements.is_empty() {
            return None;
        }

        let replacements = if underlined_error.chars().count() > 4
            && is_all_uppercase(&underlined_error)
        {
            let mut out = Vec::new();
            for r in &replacements {
                let new = r.to_uppercase();
                if !out.contains(&new) && new != underlined_error {
                    out.push(new);
                }
            }
            out
        } else {
            let tokens = ctx.sentence_tokens();
            // LT's index 0 is the SENT_START pseudo-token; ours starts at 0
            let mut words_start = 0;
            while words_start < tokens.len()
                && (is_punct(tokens[words_start].word().as_str())
                    || !tokens[words_start]
                        .word()
                        .as_str()
                        .chars()
                        .any(char::is_alphabetic))
            {
                words_start += 1;
            }
            if ctx.pattern_token_pos() == words_start {
                let mut out = Vec::new();
                for r in &replacements {
                    let new = if r.chars().all(|c| !c.is_uppercase()) {
                        uppercase_first(r)
                    } else {
                        r.clone()
                    };
                    if !out.contains(&new) && new != underlined_error {
                        out.push(new);
                    }
                }
                out
            } else {
                let mut out = Vec::new();
                for r in &replacements {
                    if !out.contains(r) && r != &underlined_error {
                        out.push(r.clone());
                    }
                }
                out
            }
        };

        if replacements.is_empty() {
            return None;
        }

        keep(ctx, replacements)
    }

    fn is_punct(s: &str) -> bool {
        s.chars().count() == 1
            && s.chars()
                .next()
                .map(|c| c.is_ascii_punctuation() || c.is_whitespace())
                .unwrap_or(false)
    }

    /// Faithful port of `MultitokenSpeller.getSuggestions`.
    fn get_suggestions(
        suggester: &MultitokenSuggester,
        ctx: &FilterCtx,
        original_word: &str,
        are_tokens_accepted: bool,
    ) -> Vec<String> {
        let word = original_word.replace("- ", "-").replace(" -", "-");
        if discard_run_on_words(ctx, &word) {
            return Vec::new();
        }
        let normalized = MultitokenSuggester::normalize_key(&word);
        let mut weighted: Vec<(usize, String)> = Vec::new();

        let no_spaces_key = normalized.replace(' ', "");
        if let Some(candidates) = suggester.no_spaces.get(&no_spaces_key) {
            if stop_searching(candidates, original_word) {
                return Vec::new();
            }
            for candidate in candidates {
                weighted.push((0, candidate.clone()));
            }
        }

        if weighted.is_empty() {
            let first_char = normalized.chars().next().unwrap_or(' ');
            if let Some(bucket) = suggester.by_char.get(&first_char) {
                for (normalized_candidate, candidates) in bucket {
                    if stop_searching(candidates, original_word) {
                        return Vec::new();
                    }
                    if normalized_candidate.len().abs_diff(word.len()) > MAX_LENGTH_DIFF {
                        continue;
                    }
                    let candidate_parts: Vec<&str> =
                        normalized_candidate.split(' ').collect();
                    let word_parts: Vec<&str> = normalized.split(' ').collect();
                    let distances = distances_per_word(&candidate_parts, &word_parts);
                    let total: usize = distances.iter().sum();
                    if total < 1 {
                        for c in candidates {
                            weighted.push((0, c.clone()));
                        }
                        if weighted.len() == 2 {
                            break;
                        }
                        continue;
                    }
                    if normalized_candidate.chars().count() < 7 {
                        continue;
                    }
                    let mut exceeds = false;
                    for (i, &d) in distances.iter().enumerate() {
                        let max_d = if word_parts
                            .get(i)
                            .map(|w| w.chars().count() > 5)
                            .unwrap_or(false)
                            && candidate_parts
                                .get(i)
                                .map(|c| c.chars().count() > 4)
                                .unwrap_or(false)
                        {
                            2
                        } else {
                            1
                        };
                        if d > max_d {
                            exceeds = true;
                            break;
                        }
                    }
                    if exceeds {
                        continue;
                    }
                    if total <= max_edit_distance(normalized_candidate, &normalized) {
                        for c in candidates {
                            weighted.push((total, c.clone()));
                        }
                    }
                }
            }
        }

        if weighted.is_empty() {
            return Vec::new();
        }
        weighted.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.cmp(&b.1)));
        let weight_first = weighted[0].0;
        if are_tokens_accepted && weighted[0].1.to_uppercase() == original_word {
            return Vec::new();
        }
        if are_tokens_accepted && weight_first > 1 {
            return Vec::new();
        }
        let mut results = Vec::new();
        for (weight, word) in weighted {
            if weight - weight_first < 1 && !results.contains(&word) {
                results.push(word);
            }
        }
        results
    }

    fn stop_searching(candidates: &[String], original: &str) -> bool {
        for c in candidates {
            if c == original {
                return true;
            }
        }
        for c in candidates {
            if *c == c.to_lowercase() && uppercase_first(&c.to_lowercase()) == *original {
                return true;
            }
        }
        false
    }

    fn is_anagram(a: &str, b: &str) -> bool {
        if a.chars().count() != b.chars().count() {
            return false;
        }
        let mut av: Vec<char> = a.chars().collect();
        let mut bv: Vec<char> = b.chars().collect();
        av.sort();
        bv.sort();
        av == bv
    }

    fn levenshtein_lang(s1: &str, s2: &str) -> usize {
        if s1.replace(' ', "") == s2.replace(' ', "") {
            return 0;
        }
        let mut distance = levenshtein(s1, s2);
        let ns1 = s1.replace('y', "i").replace("ko", "co").replace("ka", "ca");
        let ns2 = s2.replace('y', "i").replace("ko", "co").replace("ka", "ca");
        if s1 != ns1 || s2 != ns2 {
            distance = distance.min(levenshtein(&ns1, &ns2));
        }
        let anagram = is_anagram(s1, s2);
        if distance > 1 && anagram {
            distance -= 1;
        }
        if distance > 0 && s1.chars().count() == s2.chars().count() && anagram {
            distance = 1;
        }
        distance
    }

    fn distances_per_word(parts1: &[&str], parts2: &[&str]) -> Vec<usize> {
        if parts1.len() == parts2.len() && parts1.len() > 1 {
            parts1
                .iter()
                .zip(parts2.iter())
                .map(|(a, b)| levenshtein_lang(a, b))
                .collect()
        } else {
            let s1 = parts1.join(" ");
            let s2 = parts2.join(" ");
            vec![levenshtein_lang(&s1, &s2)]
        }
    }

    fn char_distance(a: char, b: char) -> f32 {
        if a == b {
            return 0.0;
        }
        if (a == 's' && b == 'z') || (a == 'z' && b == 's') {
            return 0.2;
        }
        if (a == 'b' && b == 'v') || (a == 'v' && b == 'b') {
            return 0.2;
        }
        if (a == 'i' && b == 'y') || (a == 'y' && b == 'i') {
            return 0.0;
        }
        1.0
    }

    fn max_edit_distance(candidate: &str, word: &str) -> usize {
        let total_length = word.chars().count();
        let correct_length = total_length.saturating_sub(number_of_correct_chars(candidate, word));
        let parts1: Vec<&str> = candidate.split(' ').collect();
        let parts2: Vec<&str> = word.split(' ').collect();
        let first_char_wrong = if parts1.len() == parts2.len() && parts1.len() == 2 {
            char_distance(
                parts1[0].chars().next().unwrap_or(' '),
                parts2[0].chars().next().unwrap_or(' '),
            ) + char_distance(
                parts1[1].chars().next().unwrap_or(' '),
                parts2[1].chars().next().unwrap_or(' '),
            )
        } else {
            0.0
        };
        if correct_length <= 7 {
            (2.0 - first_char_wrong) as usize
        } else {
            (2.0 + 0.25 * (correct_length - 7) as f32 - 0.6 * first_char_wrong) as usize
        }
    }

    fn number_of_correct_chars(s1: &str, s2: &str) -> usize {
        let parts1: Vec<&str> = s1.split(' ').collect();
        let parts2: Vec<&str> = s2.split(' ').collect();
        let mut correct = 0;
        if parts1.len() == parts2.len() && parts1.len() > 1 {
            for (a, b) in parts1.iter().zip(parts2.iter()) {
                if a == b {
                    correct += a.chars().count();
                }
            }
        }
        correct
    }

    fn discard_run_on_words(ctx: &FilterCtx, underlined_error: &str) -> bool {
        let parts: Vec<&str> = underlined_error.split(' ').collect();
        if parts.len() == 2 {
            if is_capitalized(parts[1]) {
                return false;
            }
            if parts[0].is_empty() || parts[1].is_empty() {
                return true;
            }
            let p0_chars: Vec<char> = parts[0].chars().collect();
            let p1_chars: Vec<char> = parts[1].chars().collect();
            if p0_chars.is_empty() || p1_chars.is_empty() {
                return true;
            }
            let sugg1a: String = p0_chars[..p0_chars.len() - 1].iter().collect();
            let sugg1b: String = format!(
                "{}{}",
                p0_chars[p0_chars.len() - 1],
                parts[1]
            );
            if is_known_word_ctx(ctx, &sugg1a) && is_known_word_ctx(ctx, &sugg1b) {
                return true;
            }
            let sugg2a: String = format!("{}{}", parts[0], p1_chars[0]);
            let sugg2b: String = p1_chars[1..].iter().collect();
            return is_known_word_ctx(ctx, &sugg2a) && is_known_word_ctx(ctx, &sugg2b);
        }
        false
    }
}

// ---------------------------------------------------------------------------
// suppress misspelled suggestions
// ---------------------------------------------------------------------------

fn suppress_misspelled(ctx: &mut FilterCtx, replacements: Vec<String>) -> Option<JavaOutcome> {
    let suppress_match = ctx
        .arg("suppressMatch")
        .map(|x| !x.eq_ignore_ascii_case("false"))
        .unwrap_or(true);
    let suppress_postag = ctx.arg("SuppressPostag");

    let mut new_replacements = Vec::new();
    for replacement in &replacements {
        if !is_misspelled_word(ctx, replacement) {
            match &suppress_postag {
                Some(regex) => {
                    let re = Regex::new(regex.clone());
                    let matches = ctx
                        .tagger
                        .get_tags_with_options(replacement, Some(false), Some(false))
                        .any(|data| matches_full(&re, data.pos().as_str()));
                    if !matches {
                        new_replacements.push(replacement.clone());
                    }
                }
                None => new_replacements.push(replacement.clone()),
            }
        }
    }

    if new_replacements.is_empty() && suppress_match {
        None
    } else {
        keep(ctx, new_replacements)
    }
}

fn is_misspelled_word(ctx: &FilterCtx, s: &str) -> bool {
    s.split_whitespace()
        .any(|token| !is_known_word_ctx(ctx, token))
}

// ---------------------------------------------------------------------------
// find suggestions
// ---------------------------------------------------------------------------

const MAX_SUGGESTIONS: usize = 10;

fn find_suggestions(ctx: &mut FilterCtx, variant: FindSuggVariant) -> Option<JavaOutcome> {
    let word_from = ctx.arg("wordFrom")?;
    let desired_postag = ctx.arg("desiredPostag")?;
    let priority_postag = ctx.arg("priorityPostag");
    let remove_regex = ctx.arg("removeSuggestionsRegexp");
    let suppress_match = ctx
        .arg("suppressMatch")
        .map(|x| x.eq_ignore_ascii_case("true"))
        .unwrap_or(false);
    let diacritics_mode = ctx.arg("Mode").map(|x| x == "diacritics").unwrap_or(false);

    let word: String = if word_from == "inmarker" {
        let mut w = ctx.matched_text.replace(' ', "");
        if variant == FindSuggVariant::Ca {
            w = replace_ela_geminada(&w);
        }
        w
    } else {
        let pos: usize = word_from.parse().ok()?;
        let tokens = ctx.matched_token_refs();
        tokens.get(pos.checked_sub(1)?)?.word().as_str().to_string()
    };

    let _ = (&desired_postag, &priority_postag, &remove_regex);
    let desired_re = ctx.regex_arg("desiredPostag")?;
    let priority_re = ctx.regex_arg("priorityPostag");
    let remove_re = ctx.regex_arg("removeSuggestionsRegexp");

    let is_word_capitalized = is_capitalized(&word);
    let is_word_allupper = is_all_uppercase(&word);

    // If the original word (tagged standalone) meets the requirement:
    // in diacritics mode, suppress the match.
    let original_matches = ctx
        .tagger
        .get_tags_with_options(&word, Some(false), Some(false))
        .any(|data| matches_full_regex(&desired_re, data.pos().as_str()));
    if original_matches && diacritics_mode {
        return None;
    }

    let mut replacements: Vec<String> = Vec::new();
    let mut replacements2: Vec<String> = Vec::new();
    let mut used_lemmas: Vec<String> = Vec::new();

    let suggestions = spelling_suggestions(ctx, variant, &word);
    if std::env::var("NLPRULE_DEBUG_FILTER").is_ok() {
        eprintln!(
            "FindSuggestions: word={:?} candidates={:?}",
            word,
            &suggestions.iter().take(15).cloned().collect::<Vec<_>>()
        );
    }

    for suggestion in suggestions {
        if replacements.len() >= 2 * MAX_SUGGESTIONS {
            break;
        }
        let cleaned = clean_suggestion(variant, &suggestion);
        let analyzed: Vec<(String, String)> = ctx
            .tagger
            .get_tags_with_options(&cleaned, Some(false), Some(false))
            .map(|data| {
                (
                    data.lemma().as_str().to_string(),
                    data.pos().as_str().to_string(),
                )
            })
            .collect();

        if is_suggestion_exception(variant, &analyzed) {
            continue;
        }

        let mut used = false;
        if suggestion != word {
            let matches = analyzed
                .iter()
                .any(|(_, pos)| matches_full_regex(&desired_re, pos));
            if matches
                && !replacements.contains(&suggestion)
                && !replacements.contains(&suggestion.to_lowercase())
                && (!diacritics_mode
                    || remove_diacritics(&suggestion).to_lowercase()
                        == remove_diacritics(&word).to_lowercase())
                && remove_re
                    .as_ref()
                    .map(|re| !matches_full_regex(re, &suggestion))
                    .unwrap_or(true)
            {
                let mut replacement = suggestion.clone();
                if is_word_allupper {
                    replacement = replacement.to_uppercase();
                }
                if is_word_capitalized {
                    replacement = uppercase_first(&replacement);
                }
                let has_priority = priority_re
                    .as_ref()
                    .map(|re| {
                        analyzed
                            .iter()
                            .any(|(_, pos)| matches_full_regex(re, pos))
                    })
                    .unwrap_or(false);
                if has_priority {
                    replacements.insert(0, replacement);
                } else {
                    replacements.push(replacement);
                }
                used = true;
            }
        }

        // try with the synthesizer
        if !used {
            if let Some(synth) = ctx.synth {
                if !synth.is_empty() {
                    let mut synth_forms: Vec<String> = Vec::new();
                    for (lemma, _) in &analyzed {
                        if used_lemmas.contains(lemma) {
                            continue;
                        }
                        used_lemmas.push(lemma.clone());
                        for form in synth.synthesize_regex(lemma, &desired_re) {
                            if !synth_forms.contains(&form) {
                                synth_forms.push(form);
                            }
                        }
                    }
                    for mut form in synth_forms {
                        if is_word_allupper {
                            form = form.to_uppercase();
                        }
                        if is_word_capitalized {
                            form = uppercase_first(&form);
                        }
                        replacements2.push(form);
                    }
                }
            }
        }
    }

    let match_contains_finished = ctx
        .replacements
        .iter()
        .any(|k| !k.to_lowercase().contains("{suggestion}"));

    if diacritics_mode && replacements.is_empty() && !match_contains_finished {
        return None;
    }
    if replacements.len() + replacements2.len() == 0 && suppress_match && !match_contains_finished {
        return None;
    }

    // build the definitive replacements
    let mut definitive: Vec<String> = Vec::new();
    let mut replacements_used = false;
    for s in &ctx.replacements {
        if s.contains("{suggestion}") || s.contains("{Suggestion}") || s.contains("{SUGGESTION}") {
            replacements_used = true;
            for s2 in &replacements {
                if definitive.len() >= MAX_SUGGESTIONS {
                    break;
                }
                let new = if s.contains("{suggestion}") {
                    s.replace("{suggestion}", s2)
                } else if s.contains("{Suggestion}") {
                    s.replace("{Suggestion}", &uppercase_first(s2))
                } else {
                    s.replace("{SUGGESTION}", &s2.to_uppercase())
                };
                if !definitive.contains(&new) {
                    definitive.push(new);
                }
            }
        } else if !definitive.contains(s) {
            definitive.push(s.clone());
        }
    }
    if !replacements_used {
        if replacements.is_empty() {
            replacements2.sort_by_key(|w| {
                let d = osa_distance(&word.to_lowercase(), &w.to_lowercase());
                if d > 4 {
                    8
                } else {
                    d
                }
            });
            for replacement in &replacements2 {
                if !replacements.contains(replacement) && !definitive.contains(replacement) {
                    replacements.push(replacement.clone());
                }
            }
        }
        for replacement in &replacements {
            if definitive.len() >= MAX_SUGGESTIONS {
                break;
            }
            if !definitive.contains(replacement) {
                definitive.push(replacement.clone());
            }
        }
    }

    definitive.retain(|x| x != &ctx.matched_text);
    definitive.dedup();

    if definitive.is_empty() {
        return None;
    }

    keep(ctx, definitive)
}

fn replace_ela_geminada(word: &str) -> String {
    let chars: Vec<char> = word.chars().collect();
    let mut out = String::new();
    let mut i = 0;
    while i < chars.len() {
        if i + 2 < chars.len()
            && chars[i].to_lowercase().next() == Some('l')
            && chars[i + 2].to_lowercase().next() == Some('l')
            && matches!(
                chars[i + 1],
                '.' | '•' | '⋅' | '∙' | '×' | '\u{F0D7}' | '-'
            )
        {
            out.push(chars[i]);
            out.push('·');
            out.push(chars[i + 2]);
            i += 3;
        } else {
            out.push(chars[i]);
            i += 1;
        }
    }
    out
}

fn clean_suggestion(variant: FindSuggVariant, s: &str) -> String {
    if variant == FindSuggVariant::Fr {
        let s = strip_french_clitic(s);
        s.split(' ').next().unwrap_or("").to_string()
    } else {
        s.to_string()
    }
}

fn strip_french_clitic(s: &str) -> String {
    let mut chars = s.chars();
    if let (Some(first), Some('\'')) = (chars.next(), chars.clone().next()) {
        if "smntl".contains(first.to_ascii_lowercase()) {
            return chars.as_str().to_string();
        }
    }
    for clitic in [
        "nous", "vous", "le", "la", "les", "me", "te", "se", "leur", "en", "y",
    ] {
        let with_space = format!("{} ", clitic);
        if s.len() >= with_space.len()
            && s.get(..with_space.len())
                .map(|prefix| prefix.eq_ignore_ascii_case(&with_space))
                .unwrap_or(false)
        {
            return String::from_utf8_lossy(&s.as_bytes()[with_space.len()..]).into_owned();
        }
    }
    s.to_string()
}

fn is_suggestion_exception(variant: FindSuggVariant, analyzed: &[(String, String)]) -> bool {
    if variant == FindSuggVariant::Ca {
        const IGNORE: [&str; 4] = ["enterar", "sentar", "conseguir", "alcançar"];
        const ALLOW: [&str; 2] = ["enter", "sentir"];
        analyzed.iter().any(|(lemma, _)| IGNORE.contains(&lemma.as_str()))
            && !analyzed
                .iter()
                .any(|(lemma, _)| ALLOW.contains(&lemma.as_str()))
    } else {
        false
    }
}

/// `StringTools.makeWrong`: mangle the first available vowel so the speller
/// treats a dictionary word as misspelled and still suggests neighbors.
fn make_wrong(s: &str) -> String {
    for (plain, wrong) in [
        ('a', 'ä'),
        ('e', 'ë'),
        ('i', 'ï'),
        ('o', 'ö'),
        ('u', 'ù'),
    ] {
        if s.contains(plain) {
            return s.replace(plain, &wrong.to_string());
        }
    }
    s.to_string()
}

/// `getSpellingSuggestions`: near words from the speller dictionary.
fn spelling_suggestions(ctx: &FilterCtx, variant: FindSuggVariant, word: &str) -> Vec<String> {
    let speller = match ctx.data.speller.as_ref() {
        Some(s) if !s.is_empty() => s,
        _ => return Vec::new(),
    };

    if variant == FindSuggVariant::Fr {
        // LT tags the source token first: if it has readings, the word is
        // mangled so the speller rule still produces suggestions
        let is_tagged = ctx
            .tagger
            .get_tags_with_options(word, Some(false), Some(false))
            .next()
            .is_some();
        let base = if is_tagged {
            make_wrong(word)
        } else {
            word.to_string()
        };
        let mut words_to_check = vec![base.clone()];
        if let Some(stripped) = base.strip_suffix('s') {
            words_to_check.push(stripped.to_string());
        }
        if base.ends_with(['a', 'e', 'i', 'o', 'u', 'é']) {
            words_to_check.push(format!("{}s", base));
        }
        let mut out = Vec::new();
        for w in words_to_check {
            // the French speller rule uses edit distance 1
            out.extend(speller.find_similar_ignoring_diacritics(&w, 1));
        }
        out
    } else {
        speller.find_similar(word, 2)
    }
}

// ---------------------------------------------------------------------------
// advanced synthesizer
// ---------------------------------------------------------------------------

fn advanced_synthesizer(
    ctx: &mut FilterCtx,
    replacements: Vec<String>,
) -> Option<JavaOutcome> {
    let postag_select = ctx.arg("postagSelect")?;
    let lemma_select = ctx.arg("lemmaSelect")?;
    let postag_from_str = ctx.arg("postagFrom")?;
    let lemma_from_str = ctx.arg("lemmaFrom")?;
    let new_lemma = ctx.arg("newLemma").unwrap_or_default();
    let postag_replace = ctx.arg("postagReplace");

    let tokens: Vec<(Span, &str, bool, Vec<(String, String)>)> = {
        let raw = ctx.matched_token_refs();
        raw.iter()
            .map(|t| {
                (
                    t.span().clone(),
                    t.word().as_str(),
                    t.has_space_before(),
                    t.word()
                        .tags()
                        .iter()
                        .map(|d| {
                            (
                                d.lemma().as_str().to_string(),
                                d.pos().as_str().to_string(),
                            )
                        })
                        .collect(),
                )
            })
            .collect()
    };

    let resolve_pos = |spec: &str, span: &Span| -> Option<usize> {
        if spec.starts_with("marker") {
            let mut pos = 0;
            while pos < tokens.len() && tokens[pos].0.byte().start < span.byte().start {
                pos += 1;
            }
            pos += 1;
            if spec.len() > "marker".len() {
                let off: usize = spec.trim_start_matches("marker").parse().ok()?;
                pos += off;
            }
            Some(pos)
        } else {
            spec.parse().ok()
        }
    };

    let span = ctx.span.clone();
    let postag_from = resolve_pos(&postag_from_str, &span)?;
    let lemma_from = resolve_pos(&lemma_from_str, &span)?;
    if postag_from < 1 || postag_from > tokens.len() || lemma_from < 1 || lemma_from > tokens.len()
    {
        return Some(JavaOutcome {
            span,
            replacements,
            message: None,
        });
    }

    let _ = (&postag_select, &lemma_select);
    let lemma_re = ctx.regex_arg("lemmaSelect")?;
    let postag_re = ctx.regex_arg("postagSelect")?;

    let analyzed_of = |idx: usize| -> Vec<(String, String)> {
        tokens[idx - 1].3.clone()
    };

    let pick = |analyzed: &[(String, String)], re: &Regex| -> Option<(String, String)> {
        analyzed
            .iter()
            .find(|(_, pos)| re.is_match(pos))
            .cloned()
            .or_else(|| analyzed.first().cloned())
    };

    let lemma_analyzed = analyzed_of(lemma_from);
    let (original_lemma, original_postag) = pick(&lemma_analyzed, &lemma_re)?;
    let postag_analyzed = analyzed_of(postag_from);
    let (_, mut desired_postag) = pick(&postag_analyzed, &postag_re)?;

    let desired_lemma = if new_lemma.is_empty() {
        original_lemma.clone()
    } else {
        new_lemma
    };

    if let Some(replace) = &postag_replace {
        desired_postag = composite_postag(
            &lemma_re,
            &postag_re,
            &original_postag,
            &desired_postag,
            replace,
        );
    }

    let synth = ctx.synth?;
    let synth_re = Regex::new(desired_postag.clone());
    if synth_re.try_compile().is_err() {
        return Some(JavaOutcome {
            span,
            replacements,
            message: None,
        });
    }
    let forms = synth.synthesize_regex(&desired_lemma, &synth_re);
    if forms.is_empty() {
        // Java returns the original match unchanged
        return Some(JavaOutcome {
            span,
            replacements,
            message: None,
        });
    }

    let source_word = tokens[lemma_from - 1].1;
    let is_cap = is_capitalized(source_word);
    let is_upper = is_all_uppercase(source_word);

    let mut out: Vec<String> = Vec::new();
    let mut suggestion_used = false;
    for r in &replacements {
        for form in &forms {
            let mut form = form.clone();
            if is_cap {
                form = uppercase_first(&form);
            }
            if is_upper {
                form = form.to_uppercase();
            }
            if r.contains("{suggestion}")
                || r.contains("{Suggestion}")
                || r.contains("{SUGGESTION}")
            {
                suggestion_used = true;
            }
            let complete = r
                .replace("{suggestion}", &form)
                .replace("{Suggestion}", &uppercase_first(&form))
                .replace("{SUGGESTION}", &form.to_uppercase());
            if !out.contains(&complete) {
                out.push(complete);
            }
        }
    }
    if !suggestion_used {
        out.extend(forms.iter().cloned());
    }
    // adapt the case of suggestions to the matched text (Language.adaptSuggestion)
    let matched = ctx.matched_text.clone();
    let out: Vec<String> = out
        .into_iter()
        .map(|x| adapt_case_str(&x, &matched))
        .collect();

    Some(JavaOutcome {
        span,
        replacements: out,
        message: None,
    })
}

fn adapt_case_str(input: &str, sample: &str) -> String {
    let first_upper = sample
        .chars()
        .next()
        .map(|c| c.is_uppercase())
        .unwrap_or(false);
    if first_upper {
        let has_lower = sample.chars().any(|c| c.is_lowercase());
        let count = sample.chars().count();
        if count > 1 && !has_lower {
            input.to_uppercase()
        } else {
            uppercase_first(input)
        }
    } else {
        input.to_string()
    }
}

/// `getCompositePostag`: replace `\aN` / `\bN` group references.
fn composite_postag(
    lemma_re: &Regex,
    postag_re: &Regex,
    original_postag: &str,
    desired_postag: &str,
    postag_replace: &str,
) -> String {
    let mut result = postag_replace.to_string();
    if let (Some(caps_a), Some(caps_b)) = (
        lemma_re.captures(original_postag),
        postag_re.captures(desired_postag),
    ) {
        let groups_a = lemma_re.captures_len();
        for i in 1..groups_a {
            if let Some(g) = caps_a.get(i) {
                result = result.replace(&format!("\\a{}", i), g.as_str());
            }
        }
        let groups_b = postag_re.captures_len();
        for i in 1..groups_b {
            if let Some(g) = caps_b.get(i) {
                result = result.replace(&format!("\\b{}", i), g.as_str());
            }
        }
    }
    result
}

// ---------------------------------------------------------------------------
// partial pos tag
// ---------------------------------------------------------------------------

fn partial_pos_tag(ctx: &mut FilterCtx, replacements: Vec<String>) -> Option<JavaOutcome> {
    let no: usize = ctx.arg("no")?.parse().ok()?;
    let regexp = ctx.arg("regexp")?;
    let required = ctx.arg("postag_regexp")?;
    let negate_pos = ctx.args_contains("negate_pos");
    let two_groups = ctx.args_contains("two_groups_regexp");
    let prefix = ctx.arg("prefix").unwrap_or_default();
    let suffix = ctx.arg("suffix").unwrap_or_default();

    let tokens = ctx.matched_token_refs();
    let token = tokens.get(no.checked_sub(1)?)?;
    let token_str = format!("{}{}{}", prefix, token.word().as_str(), suffix);

    let re = ctx.regex_arg("regexp")?;
    let caps = re.captures(&token_str)?;

    let partial = if two_groups {
        format!(
            "{}{}",
            caps.get(1).map(|x| x.as_str()).unwrap_or(""),
            caps.get(2).map(|x| x.as_str()).unwrap_or("")
        )
    } else {
        caps.get(1).map(|x| x.as_str()).unwrap_or("").to_string()
    };

    let required_re = ctx.regex_arg("postag_regexp")?;

    let mut postag_count = 0;
    let mut any_match = false;
    let mut negated_reject = false;
    for data in ctx
        .tagger
        .get_tags_with_options(&partial, Some(false), Some(false))
    {
        let pos = data.pos().as_str();
        if pos != "UNKNOWN" {
            postag_count += 1;
            if required_re.is_match(pos) {
                if negate_pos {
                    negated_reject = true;
                    break;
                }
                any_match = true;
            }
        }
    }

    if negated_reject {
        return None;
    }
    if any_match || (negate_pos && postag_count > 0) {
        keep(ctx, replacements)
    } else {
        None
    }
}

// ---------------------------------------------------------------------------
// add commas
// ---------------------------------------------------------------------------

fn add_commas(ctx: &mut FilterCtx) -> Option<JavaOutcome> {
    let suggest_semicolon = ctx
        .arg("suggestSemicolon")
        .map(|x| x.eq_ignore_ascii_case("true"))
        .unwrap_or(false);

    let tokens = ctx.sentence_tokens();
    let mut postag_from = 1;
    while postag_from < tokens.len()
        && tokens[postag_from].span().byte().start < ctx.span.byte().start
    {
        postag_from += 1;
    }
    let mut postag_to = postag_from;
    while postag_to < tokens.len() && tokens[postag_to].span().byte().end < ctx.span.byte().end {
        postag_to += 1;
    }

    let is_punct = |s: &str| {
        s.chars().count() == 1
            && s.chars()
                .next()
                .map(|c| c.is_ascii_punctuation())
                .unwrap_or(false)
    };
    let opening_quote =
        |s: &str| s.chars().count() == 1 && "«“\"‘'„¿¡".contains(s.chars().next().unwrap());

    let before_ok = postag_from == 1
        || is_punct(tokens[postag_from - 1].word().as_str())
        || is_capitalized(tokens[postag_from].word().as_str());
    let after_ok = postag_to + 1 <= tokens.len() - 1
        && is_punct(tokens[postag_to + 1].word().as_str())
        && !(tokens[postag_to + 1].has_space_before()
            && opening_quote(tokens[postag_to + 1].word().as_str()));

    if before_ok && after_ok {
        return None;
    }

    let matched_slice = ctx.sentence.slice(ctx.span.clone()).to_string();

    if suggest_semicolon && tokens[postag_from - 1].word().as_str() == "," && !after_ok {
        let span = Span::from_positions(
            tokens[postag_from - 1].span().start(),
            tokens[postag_to].span().end(),
        );
        Some(JavaOutcome {
            span,
            replacements: vec![
                format!("; {},", matched_slice),
                format!(", {},", matched_slice),
            ],
            message: None,
        })
    } else if before_ok && !after_ok {
        let span = Span::from_positions(tokens[postag_to].span().start(), ctx.span.end());
        Some(JavaOutcome {
            span,
            replacements: vec![format!("{},", tokens[postag_to].word().as_str())],
            message: None,
        })
    } else if !before_ok && after_ok {
        let mut start = tokens[postag_from].span().start();
        if tokens[postag_from].has_space_before() {
            start = Position {
                byte: start.byte.saturating_sub(1),
                char: start.char.saturating_sub(1),
            };
        }
        let span = Span::from_positions(start, tokens[postag_from].span().end());
        Some(JavaOutcome {
            span,
            replacements: vec![format!(", {}", tokens[postag_from].word().as_str())],
            message: None,
        })
    } else {
        let mut start = tokens[postag_from].span().start();
        if tokens[postag_from].has_space_before() {
            start = Position {
                byte: start.byte.saturating_sub(1),
                char: start.char.saturating_sub(1),
            };
        }
        let span = Span::from_positions(start, tokens[postag_to].span().end());
        Some(JavaOutcome {
            span,
            replacements: vec![format!(", {},", matched_slice)],
            message: None,
        })
    }
}

// ---------------------------------------------------------------------------
// convert to sentence case
// ---------------------------------------------------------------------------

fn convert_to_sentence_case(ctx: &mut FilterCtx) -> Option<JavaOutcome> {
    let tokens = ctx.sentence_tokens();
    let pattern_token_pos = ctx.pattern_token_pos();

    let mut replacement = String::new();
    let mut original = String::new();
    let mut first_done = false;

    for (i, token) in tokens.iter().enumerate().skip(pattern_token_pos) {
        if token.span().byte().start < ctx.span.byte().start
            || token.span().byte().end > ctx.span.byte().end
        {
            continue;
        }
        let token_str = token.word().as_str();
        let mut normalized = token_str.to_lowercase();

        if let Some(next) = tokens.get(i + 1) {
            if next.word().as_str() == "." {
                if normalized.chars().count() == 1 {
                    normalized = normalized.to_uppercase();
                } else if normalized == "corp" {
                    normalized = "Corp".to_string();
                }
            }
        }

        let token_capitalized = uppercase_first(&normalized);
        if !first_done
            && !is_punct_single(token_str)
            && !token_str.is_empty()
        {
            first_done = true;
            replacement.push_str(&token_capitalized);
            original.push_str(token_str);
        } else {
            if token.has_space_before() {
                replacement.push(' ');
                original.push(' ');
            }
            replacement.push_str(&normalized);
            original.push_str(token_str);
        }
    }

    if replacement == original {
        return None;
    }

    keep(ctx, vec![replacement])
}

fn is_punct_single(s: &str) -> bool {
    s.chars().count() == 1
        && s.chars()
            .next()
            .map(|c| c.is_ascii_punctuation())
            .unwrap_or(false)
}

// ---------------------------------------------------------------------------
// confusion check / diacritics check
// ---------------------------------------------------------------------------

fn confusion_check(
    ctx: &mut FilterCtx,
    replacements: Vec<String>,
    diacritics: bool,
) -> Option<JavaOutcome> {
    let postag = ctx.arg("postag")?;
    let original_form = ctx.arg("form")?;
    let is_all_upper = is_all_uppercase(&original_form);
    let is_cap = is_capitalized(&original_form);
    let form = original_form.to_lowercase();

    let gendernumber_from = ctx.arg("gendernumberFrom");

    let mut desired: Option<&'static str> = None;
    if let Some(gn) = &gendernumber_from {
        let i: usize = gn.parse().ok()?;
        let tokens = ctx.matched_token_refs();
        let atr = tokens.get(i.checked_sub(1)?)?;
        let tag_matches = |re: &str| {
            let regex = Regex::new(re.to_string());
            atr.word()
                .tags()
                .iter()
                .any(|d| matches_full(&regex, d.pos().as_str()))
        };
        desired = if tag_matches("[NAPD].+MS.*|V.P..SM.") {
            Some("MS")
        } else if tag_matches("[NAPD].+MP.*|V.P..PM.") {
            Some("MP")
        } else if tag_matches("[NAPD].+FS.*|V.P..SF.") {
            Some("FS")
        } else if tag_matches("[NAPD].+FP.*|V.P..PF.") {
            Some("FP")
        } else if tag_matches("[NAPD].+CP.*|V.P..P.") {
            Some("CP")
        } else if tag_matches("[NAPD].+CS.*|V.P..S.") {
            Some("CS")
        } else {
            None
        };
    }

    let entry = ctx
        .data
        .confusion_pairs
        .as_ref()
        .and_then(|m| m.get(&form))?;

    let postag_re = Regex::new(postag.clone());
    if !entry.iter().any(|(_, pos)| matches_full(&postag_re, pos)) {
        return None;
    }

    let replacement = if let Some(gender) = desired {
        let first = entry.first()?;
        let pattern = match gender {
            "MS" => "NC[MC][SN]000|A..[MC][SN].|V.P..SM",
            "MP" => "NC[MC][PN]000|A..[MC][PN].|V.P..PM",
            "FS" => "NC[FC][SN]000|A..[FC][SN].|V.P..SF",
            "FP" => "NC[FC][PN]000|A..[FC][PN].|V.P..PF",
            "CP" => "NC[MFC][PN]000|A..[MFC][PN].|V.P..P.",
            "CS" => "NC[MFC][SN]000|A..[MFC][SN].|V.P..S.",
            _ => return None,
        };
        let pattern_re = Regex::new(pattern.to_string());
        if !matches_full(&pattern_re, &first.1) {
            return None;
        }
        first.0.clone()
    } else if gendernumber_from.is_none() {
        entry.first()?.0.clone()
    } else {
        return None;
    };

    let mut message = ctx.message.clone();
    let replaces_accent = crate::rule::filter_data::has_diacritics(&replacement)
        && !crate::rule::filter_data::has_diacritics(&form);
    if !replaces_accent {
        if diacritics {
            message = message.replace("s'escriu amb accent", "s'escriu d'una altra manera");
        } else {
            message = message.replace("se escribe con tilde", "se escribe de otra manera");
        }
    }

    let mut replacement = replacement;
    if is_all_upper {
        replacement = replacement.to_uppercase();
    }
    if is_cap {
        replacement = uppercase_first(&replacement);
    }

    let out: Vec<String> = replacements
        .iter()
        .map(|sugg| {
            sugg.replace("{suggestion}", &replacement)
                .replace("{Suggestion}", &uppercase_first(&replacement))
                .replace("{SUGGESTION}", &replacement.to_uppercase())
        })
        .collect();

    Some(JavaOutcome {
        span: ctx.span.clone(),
        replacements: out,
        message: Some(message),
    })
}

// ---------------------------------------------------------------------------
// pt: regular/irregular participle
// ---------------------------------------------------------------------------

fn regular_irregular_participle(
    ctx: &mut FilterCtx,
    replacements: Vec<String>,
) -> Option<JavaOutcome> {
    let direction = ctx.arg("direction")?;
    let synth = ctx.synth?;

    let tokens = ctx.sentence_tokens();
    let atr = tokens
        .iter()
        .find(|t| t.span().byte().start == ctx.span.byte().start)?;

    let has_vmp = atr
        .word()
        .tags()
        .iter()
        .any(|d| d.pos().as_str().starts_with("VMP"));
    if !has_vmp {
        return None;
    }

    let mut selected: Option<(String, String)> = None;
    for d in atr.word().tags() {
        let pos = d.pos().as_str();
        if pos.starts_with("VMP") {
            selected = Some((d.lemma().as_str().to_string(), pos.to_string()));
        }
    }
    let (lemma, mut desired) = selected?;

    if desired.ends_with('C') {
        desired = format!("{}[MC]", &desired[..desired.len() - 1]);
    } else {
        let last = desired.chars().last()?;
        desired = format!("{}[{}C]", &desired[..desired.len() - 1], last);
    }

    let re = Regex::new(desired.clone());
    let participles = synth.synthesize_regex(&lemma, &re);
    if participles.len() <= 1 {
        return None;
    }

    let is_regular = |p: &str| {
        let lp = p.to_lowercase();
        lp.ends_with("do") || lp.ends_with("dos") || lp.ends_with("da") || lp.ends_with("das")
    };

    let atr_text = atr.word().as_str();
    let replacement = if direction.eq_ignore_ascii_case("RegularToIrregular") && is_regular(atr_text)
    {
        if !is_regular(&participles[0]) {
            participles[0].clone()
        } else if !is_regular(&participles[1]) {
            participles[1].clone()
        } else {
            return None;
        }
    } else if direction.eq_ignore_ascii_case("IrregularToRegular") && !is_regular(atr_text) {
        if is_regular(&participles[0]) {
            participles[0].clone()
        } else if is_regular(&participles[1]) {
            participles[1].clone()
        } else {
            return None;
        }
    } else {
        return None;
    };

    let out: Vec<String> = replacements
        .iter()
        .map(|sugg| {
            sugg.replace("{suggestion}", &replacement)
                .replace("{Suggestion}", &uppercase_first(&replacement))
                .replace("{SUGGESTION}", &replacement.to_uppercase())
        })
        .collect();

    keep(ctx, out)
}

// ---------------------------------------------------------------------------
// wave 2: fr / pt / number families / de
// ---------------------------------------------------------------------------

fn suggestions_remove(ctx: &mut FilterCtx, replacements: Vec<String>) -> Option<JavaOutcome> {
    let regex = ctx.regex_arg("RemoveSuggestionsRegexp")?;
    let out: Vec<String> = replacements
        .into_iter()
        .filter(|r| !matches_full(&regex, r))
        .collect();
    keep(ctx, out)
}

fn make_contractions(ctx: &mut FilterCtx, replacements: Vec<String>) -> Option<JavaOutcome> {
    fn fix(s: &str) -> String {
        let s = word_replace(s, "de le", "du");
        let s = word_replace(&s, "à le", "au");
        let s = word_replace(&s, "de les", "des");
        word_replace(&s, "à les", "aux")
    }
    fn word_replace(text: &str, from: &str, to: &str) -> String {
        // case-insensitive \bfrom\b -> to, preserving nothing else
        let lower = text.to_lowercase();
        let mut out = String::with_capacity(text.len());
        let chars: Vec<char> = text.chars().collect();
        let lchars: Vec<char> = lower.chars().collect();
        let from: Vec<char> = from.chars().collect();
        let to: Vec<char> = to.chars().collect();
        let n = chars.len();
        let m = from.len();
        let mut i = 0;
        while i < n {
            let at_boundary_start = i == 0 || !lchars[i - 1].is_alphanumeric();
            if at_boundary_start && i + m <= n && lchars[i..i + m] == from[..] {
                let at_boundary_end = i + m == n || !lchars[i + m].is_alphanumeric();
                if at_boundary_end {
                    // keep the case of the first char if it was upper
                    let mut to_val: Vec<char> = to.clone();
                    if chars[i].is_uppercase() {
                        if let Some(c) = to_val.first_mut() {
                            *c = c.to_uppercase().next().unwrap_or(*c);
                        }
                    }
                    out.extend(to_val);
                    i += m;
                    continue;
                }
            }
            out.push(chars[i]);
            i += 1;
        }
        out
    }
    let out: Vec<String> = replacements.into_iter().map(|r| fix(&r)).collect();
    keep(ctx, out)
}


/// ca `CatalanNumberSpellerFilter`: spell the number matched by the rule
/// (`number_to_spell` arg, `\1` backreference) in Catalan words with the
/// requested grammatical gender, and use it as the suggestion.
fn catalan_number_speller(ctx: &mut FilterCtx, replacements: Vec<String>) -> Option<JavaOutcome> {
    let number = ctx.arg("number_to_spell")?;
    let feminine = ctx
        .arg("gender")
        .map_or(false, |g| g.eq_ignore_ascii_case("feminine"));
    let spelled = spell_catalan(&number.replace('.', ""), feminine);
    keep(ctx, vec![spelled])
}

/// Catalan cardinal numbers, Optimot style: compounds under 100 are
/// hyphenated (`vint-i-quatre`, `trenta-dos`), hundreds too
/// (`dos-cents`/`dues-centes`), larger groups join with spaces
/// (`dos milions`, `mil dos-cents`).
pub(crate) fn spell_catalan(digits: &str, feminine: bool) -> String {
    let n: u64 = match digits.trim().parse() {
        Ok(n) => n,
        Err(_) => return digits.trim().to_string(),
    };
    spell_catalan_u64(n, feminine)
}

fn spell_catalan_u64(n: u64, feminine: bool) -> String {
    const UNITS: [&str; 20] = [
        "zero", "un", "dos", "tres", "quatre", "cinc", "sis", "set", "vuit", "nou", "deu",
        "onze", "dotze", "tretze", "catorze", "quinze", "setze", "disset", "divuit", "dinou",
    ];
    const TENS: [(&str, u64); 8] = [
        ("vint", 20),
        ("trenta", 30),
        ("quaranta", 40),
        ("cinquanta", 50),
        ("seixanta", 60),
        ("setanta", 70),
        ("vuitanta", 80),
        ("noranta", 90),
    ];
    const HUNDREDS_M: [&str; 9] = [
        "cent",
        "dos-cents",
        "tres-cents",
        "quatre-cents",
        "cinc-cents",
        "sis-cents",
        "set-cents",
        "vuit-cents",
        "nou-cents",
    ];
    const HUNDREDS_F: [&str; 9] = [
        "cent",
        "dues-centes",
        "tres-centes",
        "quatre-centes",
        "cinc-centes",
        "sis-centes",
        "set-centes",
        "vuit-centes",
        "nou-centes",
    ];

    fn unit(n: u64, feminine: bool) -> String {
        match (n, feminine) {
            (1, true) => "una".to_string(),
            (2, true) => "dues".to_string(),
            _ => UNITS[n as usize].to_string(),
        }
    }

    fn under100(n: u64, feminine: bool) -> String {
        if n < 20 {
            unit(n, feminine)
        } else {
            let (word, _) = TENS[((n / 10) - 2) as usize];
            let r = n % 10;
            if r == 0 {
                word.to_string()
            } else if n < 30 {
                format!("{}-i-{}", word, unit(r, feminine))
            } else {
                format!("{}-{}", word, unit(r, feminine))
            }
        }
    }

    fn under1000(n: u64, feminine: bool) -> String {
        if n < 100 {
            return under100(n, feminine);
        }
        let (h, r) = (n / 100, n % 100);
        let hundreds = if feminine { HUNDREDS_F } else { HUNDREDS_M }[(h - 1) as usize];
        if r == 0 {
            hundreds.to_string()
        } else {
            format!("{} {}", hundreds, under100(r, feminine))
        }
    }

    if n >= 1_000_000_000 {
        let (g, r) = (n / 1_000_000_000, n % 1_000_000_000);
        let head = format!("{} mil milions", under1000(g, false));
        if r == 0 {
            head
        } else {
            format!("{} {}", head, spell_catalan_u64(r, feminine))
        }
    } else if n >= 1_000_000 {
        let (g, r) = (n / 1_000_000, n % 1_000_000);
        let head = if g == 1 {
            "un mili\u{f3}".to_string()
        } else {
            format!("{} milions", under1000(g, false))
        };
        if r == 0 {
            head
        } else {
            format!("{} {}", head, spell_catalan_u64(r, feminine))
        }
    } else if n >= 1_000 {
        let (g, r) = (n / 1_000, n % 1_000);
        let head = if g == 1 {
            "mil".to_string()
        } else {
            format!("{} mil", under1000(g, feminine))
        };
        if r == 0 {
            head
        } else {
            format!("{} {}", head, under1000(r, feminine))
        }
    } else {
        under1000(n, feminine)
    }
}


/// ar `ArabicNumberPhraseFilter`: LT's original inspects the tag of the
/// word after the numeric phrase to validate agreement. Probing LT 6.5
/// (`في مليونان صندوق.` fires, `في عشرون يوماً.` does not) shows the
/// practical split: dual forms (مليونان, ألفان, مئتان, اثنان ...) are
/// flagged before a singular noun, plain tens (عشرون ... تسعون) are not.
fn arabic_number_phrase(ctx: &mut FilterCtx, replacements: Vec<String>) -> Option<JavaOutcome> {
    const TENS: [&str; 8] = [
        "عشرون", "ثلاثون", "أربعون", "خمسون", "ستون", "سبعون", "ثمانون", "تسعون",
    ];
    let mut has_dual = false;
    let mut has_ten = false;
    for &idx in &ctx.matched_tokens {
        let w = ctx.sentence.index(idx).word().as_str();
        if TENS.iter().any(|t| w.contains(t)) {
            has_ten = true;
        }
        if w.contains("مليونان")
            || w.contains("ملياران")
            || w.contains("بليونان")
            || w.contains("ألفان")
            || w.contains("مئتان")
            || w.contains("اثنان")
            || w.contains("اثنتان")
            || w.contains("اثنا")
            || w.contains("اثنتا")
        {
            has_dual = true;
        }
    }
    if has_ten && !has_dual {
        return None;
    }
    keep(ctx, replacements)
}

fn number_in_word(ctx: &mut FilterCtx, replacements: Vec<String>) -> Option<JavaOutcome> {
    let word = ctx.arg("word")?;
    let replacing_zero_o = word.replace('0', "o");
    let without_digits: String = word.chars().filter(|c| !c.is_ascii_digit()).collect();

    let mut out = Vec::new();
    if is_known_word_ctx(ctx, &replacing_zero_o) && word != replacing_zero_o {
        out.push(replacing_zero_o);
    }
    if is_known_word_ctx(ctx, &without_digits) {
        out.push(without_digits.clone());
    }
    if out.is_empty() {
        if let Some(speller) = ctx.data.speller.as_ref() {
            out = speller.find_similar(&without_digits, 2);
        }
    }
    if out.is_empty() {
        None
    } else {
        keep(ctx, out)
    }
}

fn text_to_number(
    ctx: &mut FilterCtx,
    lang: TextNumberLang,
    replacements: Vec<String>,
) -> Option<JavaOutcome> {
    fn table(lang: TextNumberLang) -> (Vec<(&'static str, f32)>, Vec<(&'static str, f32)>) {
        let numbers = match lang {
            TextNumberLang::Es => vec![
                ("cero", 0.0), ("medio", 0.5), ("un", 1.0), ("uno", 1.0), ("una", 1.0),
                ("dos", 2.0), ("tres", 3.0), ("cuatro", 4.0), ("cinco", 5.0), ("seis", 6.0),
                ("siete", 7.0), ("ocho", 8.0), ("nueve", 9.0), ("diez", 10.0), ("once", 11.0),
                ("doce", 12.0), ("trece", 13.0), ("catorce", 14.0), ("quince", 15.0),
                ("dieciséis", 16.0), ("diecisiete", 17.0), ("dieciocho", 18.0),
                ("diecinueve", 19.0), ("veinte", 20.0), ("veintiuno", 21.0), ("veintidós", 22.0),
                ("veintitrés", 23.0), ("veinticuatro", 24.0), ("veinticinco", 25.0),
                ("veintiséis", 26.0), ("veintisiete", 27.0), ("veintiocho", 28.0),
                ("veintinueve", 29.0), ("treinta", 30.0), ("cuarenta", 40.0), ("cincuenta", 50.0),
                ("sesenta", 60.0), ("setenta", 70.0), ("ochenta", 80.0), ("noventa", 90.0),
                ("cien", 100.0), ("ciento", 100.0), ("doscientos", 200.0), ("trescientos", 300.0),
                ("cuatrocientos", 400.0), ("quinientos", 500.0), ("seiscientos", 600.0),
                ("setecientos", 700.0), ("ochocientos", 800.0), ("novecientos", 900.0),
                ("doscientas", 200.0), ("trescientas", 300.0), ("cuatrocientas", 400.0),
                ("quinientas", 500.0), ("seiscientas", 600.0), ("setecientas", 700.0),
                ("ochocientas", 800.0), ("novecientas", 900.0),
            ],
            TextNumberLang::Ca => vec![
                ("zero", 0.0), ("mig", 0.5), ("un", 1.0), ("u", 1.0), ("una", 1.0), ("dos", 2.0),
                ("dues", 2.0), ("tres", 3.0), ("quatre", 4.0), ("cinc", 5.0), ("sis", 6.0),
                ("set", 7.0), ("vuit", 8.0), ("huit", 8.0), ("nou", 9.0), ("deu", 10.0),
                ("onze", 11.0), ("dotze", 12.0), ("tretze", 13.0), ("catorze", 14.0),
                ("quinze", 15.0), ("setze", 16.0), ("disset", 17.0), ("desset", 17.0),
                ("dèsset", 17.0), ("divuit", 18.0), ("devuit", 18.0), ("díhuit", 18.0),
                ("dinou", 19.0), ("denou", 19.0), ("dènou", 19.0), ("dèneu", 19.0),
                ("vint", 20.0), ("trenta", 30.0), ("quaranta", 40.0), ("cinquanta", 50.0),
                ("seixanta", 60.0), ("setanta", 70.0), ("vuitanta", 80.0), ("huitanta", 80.0),
                ("noranta", 90.0),
            ],
        };
        let multipliers = match lang {
            TextNumberLang::Es => vec![
                ("mil", 1000.0), ("millón", 1_000_000.0), ("millones", 1_000_000.0),
                ("billón", 10.0e12), ("billones", 10.0e12),
                ("trillón", 10.0e18), ("trillones", 10.0e18),
            ],
            TextNumberLang::Ca => vec![
                ("cent", 100.0), ("cents", 100.0), ("mil", 1000.0),
                ("milió", 1_000_000.0), ("milions", 1_000_000.0),
                ("bilió", 10.0e12), ("bilions", 10.0e12),
                ("trilió", 10.0e18), ("trilions", 10.0e18),
            ],
        };
        (numbers, multipliers)
    }

    let (numbers, multipliers) = table(lang);
    let lookup = |w: &str| -> Option<f32> {
        numbers
            .iter()
            .find(|(k, _)| *k == w)
            .map(|(_, v)| *v)
            .or_else(|| multipliers.iter().find(|(k, _)| *k == w).map(|(_, v)| *v))
    };
    let is_multiplier = |w: &str| multipliers.iter().any(|(k, _)| *k == w);
    let is_comma = |w: &str| matches!(lang, TextNumberLang::Es) && (w == "comma" || w == "coma");

    let tokens = ctx.matched_token_refs();
    let mut total = 0f32;
    let mut current = 0f32;
    let mut total_decimal = 0f32;
    let mut current_decimal = 0f32;
    let mut added_zeros = 0u32;
    let mut percentage = false;
    let mut decimal = false;

    for (idx, token) in tokens.iter().enumerate() {
        let form = token.word().as_str().to_lowercase();
        if idx > 0 {
            let prev = tokens[idx - 1].word().as_str().to_lowercase();
            let percent_word = matches!(lang, TextNumberLang::Es)
                && form == "ciento"
                && prev == "por";
            let percent_word_ca = matches!(lang, TextNumberLang::Ca)
                && form == "cent"
                && prev == "per";
            if percent_word || percent_word_ca {
                percentage = true;
                break;
            }
        }
        if is_comma(&form) {
            decimal = true;
            continue;
        }
        let sub_forms: Vec<&str> = if matches!(lang, TextNumberLang::Ca) {
            form.split('-').collect()
        } else {
            vec![form.as_str()]
        };
        for sub in sub_forms {
            if let Some(v) = lookup(sub) {
                if !decimal {
                    if is_multiplier(sub) {
                        if current == 0.0 {
                            current = 1.0;
                        }
                        total += current * v;
                        current = 0.0;
                    } else {
                        current += v;
                    }
                } else if !is_multiplier(sub) {
                    let zeros = format_float(v, false).len() as u32;
                    current_decimal += v / 10f32.powi((added_zeros + zeros) as i32);
                    added_zeros += 1;
                }
            }
        }
    }
    total += current;
    total_decimal += current_decimal;
    total = total + current_decimal;

    let sugg = format_float(total, percentage);
    let sugg = if matches!(lang, TextNumberLang::Ca) {
        sugg.replace('.', ",")
    } else {
        sugg
    };

    let mut out = replacements;
    out.push(sugg);
    keep(ctx, out)
}

/// Java `String.format("%s", float)` close enough: integers without decimals.
fn format_float(d: f32, percentage: bool) -> String {
    let mut result = if d == d.trunc() && d.abs() < 1e15 {
        format!("{}", d.trunc() as i64)
    } else {
        format!("{}", d)
    };
    if percentage {
        result.push('\u{202F}');
        result.push('%');
    }
    result
}

fn interrogative_verb(ctx: &mut FilterCtx, replacements: Vec<String>) -> Option<JavaOutcome> {
    let pronoun_pos: usize = ctx.arg("PronounFrom")?.parse().ok()?;
    let verb_pos: usize = ctx.arg("VerbFrom")?.parse().ok()?;
    let tokens = ctx.matched_token_refs();
    let pronoun = tokens.get(pronoun_pos.checked_sub(1)?)?;
    let verb = tokens.get(verb_pos.checked_sub(1)?)?;

    let pronoun_tags: Vec<String> = pronoun
        .word()
        .tags()
        .iter()
        .map(|d| d.pos().as_str().to_string())
        .collect();
    let pmatches = |re: &str| {
        let regex = Regex::new(re.to_string());
        pronoun_tags.iter().any(|t| matches_full(&regex, t))
    };

    let desired: Option<&str> = if pmatches("R pers obj 2 p") {
        Some("V.* (imp) [23] [sp]|V .*(ind|cond).* 2 p")
    } else if pmatches("R pers obj 1 p") {
        Some("V.* (imp) .*|V .*(ind|cond).* 1 p")
    } else if pmatches("R pers obj.*") {
        Some("V.* (imp) .*")
    } else if pmatches(".* 1 s") {
        // extra participle suggestions below
        None
    } else if pmatches(".* 2 s") {
        Some("V .*(ind|cond).* 2 s")
    } else if pmatches(".* 3( [mfe])? s") {
        Some("V .*(ind|cond).* 3 s")
    } else if pmatches(".* 1 p") {
        Some("V .*(ind|cond).* 1 p")
    } else if pmatches(".* 2 p") {
        Some("V .*(ind|cond).* 2 p")
    } else if pmatches(".* 3( [mf])? p") {
        Some("V .*(ind|cond).* 3 p")
    } else {
        None
    };

    let pronoun_text = pronoun.word().as_str();
    let separator = if pronoun_text.starts_with('-') { "" } else { "-" };
    let mut out: Vec<String> = Vec::new();

    if pmatches(".* 1 s") {
        // extra participle suggestions: "trompé-je", "trompè-je"
        if let Some(synth) = ctx.synth {
            let reading = verb
                .word()
                .tags()
                .iter()
                .find(|d| matches_full(&Regex::new("V .*".into()), d.pos().as_str()));
            if let Some(data) = reading {
                let re = Regex::new(r"V ppa [me] sp?".to_string());
                let participles = synth.synthesize_regex(data.lemma().as_str(), &re);
                if let Some(first) = participles.first() {
                    if first.ends_with('é') {
                        out.push(format!("{}{}{}", first, separator, pronoun_text));
                        let stem: String = first.chars().take(first.chars().count() - 1).collect();
                        out.push(format!("{}{}{}{}", stem, separator, "è", pronoun_text));
                    }
                }
            }
        }
    } else if let Some(desired) = desired {
        if let Some(speller) = ctx.data.speller.as_ref() {
            let verb_text = verb.word().as_str();
            let candidates: Vec<String> =
                speller.find_similar_ignoring_diacritics(verb_text, 1);
            let desired_re = Regex::new(desired.to_string());
            for candidate in candidates {
                let tags: Vec<String> = ctx
                    .tagger
                    .get_tags_with_options(&candidate, Some(false), Some(false))
                    .map(|d| d.pos().as_str().to_string())
                    .collect();
                if tags.iter().any(|t| matches_full(&desired_re, t)) {
                    let mut sugg = format!("{}{}{}", candidate, separator, pronoun_text);
                    if sugg.eq_ignore_ascii_case("peux-je") {
                        sugg = preserve_case("puis-je", &sugg);
                    }
                    if sugg.ends_with("e-je") {
                        let stem = &sugg[..sugg.len() - 4];
                        out.push(format!("{}é-je", stem));
                        out.push(format!("{}è-je", stem));
                    } else if !out.contains(&sugg) {
                        out.push(sugg);
                    }
                }
            }
        }
    }

    if out.is_empty() {
        keep(ctx, replacements)
    } else {
        keep(ctx, out)
    }
}

fn word_with_determiner(ctx: &mut FilterCtx, replacements: Vec<String>) -> Option<JavaOutcome> {
    let word_from: usize = ctx.arg("wordFrom")?.parse().ok()?;
    let determiner_from: usize = ctx.arg("determinerFrom")?.parse().ok()?;
    let tokens = ctx.matched_token_refs();
    let det_token = tokens.get(determiner_from.checked_sub(1)?)?;
    let word_token = tokens.get(word_from.checked_sub(1)?)?;

    let det_re = Regex::new(r"(P.)?D .*|J .*|V.* ppa .*".to_string());
    let word_re = Regex::new(r"[ZNJ] .*|V.* ppa .*".to_string());

    let pick = |token: &Token, re: &Regex| -> Option<(String, String)> {
        token
            .word()
            .tags()
            .iter()
            .find(|d| matches_full(re, d.pos().as_str()))
            .map(|d| (d.lemma().as_str().to_string(), d.pos().as_str().to_string()))
    };

    let det = pick(det_token, &det_re)?;
    let word = pick(word_token, &word_re)?;
    let synth = ctx.synth?;

    let is_noun = word.1.starts_with('N') || word.1.starts_with('Z');
    let is_adj = word.1.starts_with('J');
    let prefix = if is_noun && !is_adj {
        "[NZ] "
    } else if !is_noun && is_adj {
        "J "
    } else {
        "[ZNJ] "
    };

    const GENDER_NUMBER: [&str; 4] = ["([me]) (s|sp)", "([fe]) (s|sp)", "([me]) (p|sp)", "([fe]) (p|sp)"];
    let determiner_prefix = "((P.)?D |J |V.* ppa )";

    let is_det_cap = is_capitalized(det_token.word().as_str());
    let is_word_cap = is_capitalized(word_token.word().as_str());
    let is_det_upper =
        is_all_uppercase(det_token.word().as_str()) && det_token.word().as_str() != "L'";
    let is_word_upper = is_all_uppercase(word_token.word().as_str());

    const EXCEPTIONS: [&str; 4] = ["bels", "fols", "mols", "nouvels"];

    let mut out: Vec<String> = Vec::new();
    for gn in GENDER_NUMBER {
        let det_target = Regex::new(format!("{}{}", determiner_prefix, gn).to_string());
        let word_target = Regex::new(format!("{}{}", prefix, gn).to_string());
        let mut det_forms = synth.synthesize_regex(&det.0, &det_target);
        let mut word_forms = synth.synthesize_regex(&word.0, &word_target);
        if det_forms.is_empty() {
            let gn_plain = gn.replace(['(', ')', '|'], "");
            if matches_full(&Regex::new(format!(".+{}", gn).to_string()), &det.1) {
                det_forms = vec![det_token.word().as_str().to_string()];
            }
            let _ = gn_plain;
        }
        if word_forms.is_empty()
            && matches_full(&Regex::new(format!(".+{}", gn).to_string()), &word.1)
        {
            word_forms = vec![word_token.word().as_str().to_string()];
        }
        for wf in &word_forms {
            for df in &det_forms {
                if EXCEPTIONS.contains(&df.as_str()) {
                    continue;
                }
                let mut det_form = df.clone();
                let mut word_form = wf.clone();
                if is_det_cap {
                    det_form = uppercase_first(&det_form);
                }
                if is_word_cap {
                    word_form = uppercase_first(&word_form);
                }
                if is_det_upper {
                    det_form = det_form.to_uppercase();
                }
                if is_word_upper {
                    word_form = word_form.to_uppercase();
                }
                let mut r = format!("{} {}", det_form, word_form);
                r = r.replace("' ", "'");
                if !out.contains(&r) {
                    if r.ends_with(word_token.word().as_str()) {
                        out.insert(0, r.clone());
                    } else {
                        out.push(r);
                    }
                }
            }
        }
    }

    // LT additionally validates each suggestion against other rules here
    // (suggestionHasNoErrors); we keep all synthesized forms.
    let mut all = out;
    all.extend(replacements);
    keep(ctx, all)
}

fn enclisis_pt(ctx: &mut FilterCtx, replacements: Vec<String>) -> Option<JavaOutcome> {
    let verb_pos: usize = ctx.arg("verbPos")?.parse().ok()?;
    let pronoun_pos: usize = ctx.arg("pronounPos")?.parse().ok()?;
    let convert_accusative = ctx.arg("convertToAccusative").as_deref() == Some("true");

    let tokens = ctx.matched_token_refs();
    // LT indexes patternTokens directly (0-based) here
    let verb = tokens.get(verb_pos)?;
    let pronoun = tokens.get(pronoun_pos)?;
    let verb_text = verb.word().as_str();

    let mut pronoun_tags: Vec<String> = Vec::new();
    for data in pronoun.word().tags() {
        if pronoun.word().as_str() == "nos" {
            pronoun_tags.push("PP1CPO00".to_string());
            if verb_text.ends_with('m')
                || verb_text.ends_with("ão")
                || verb_text.ends_with("õe")
            {
                pronoun_tags.push("PP3MPA00".to_string());
            }
            break;
        }
        let pos = data.pos().as_str();
        if pos.starts_with("PP") {
            let pos = if convert_accusative && pos.ends_with("N00") {
                format!("{}A00", &pos[..pos.len() - 3])
            } else {
                pos.to_string()
            };
            pronoun_tags.push(pos);
        }
    }
    if pronoun_tags.is_empty() {
        return None;
    }

    let synth = ctx.synth?;
    let is_title = is_capitalized(verb_text);
    let is_all_caps = is_all_uppercase(verb_text);
    let mut suggestions: Vec<String> = Vec::new();
    for data in verb.word().tags() {
        let pos = data.pos().as_str();
        if pos.starts_with('V') {
            for pronoun_tag in &pronoun_tags {
                let key = format!("{}:{}", pos, pronoun_tag);
                for mut form in synth.lookup(data.lemma().as_str(), &key) {
                    if is_title {
                        form = uppercase_first(&form);
                    } else if is_all_caps {
                        form = form.to_uppercase();
                    }
                    if !suggestions.contains(&form) {
                        suggestions.push(form);
                    }
                }
            }
            break;
        }
    }
    if suggestions.is_empty() {
        None
    } else {
        keep(ctx, suggestions)
    }
    .or_else(|| keep(ctx, replacements))
}

fn proclisis_pt(ctx: &mut FilterCtx, replacements: Vec<String>) -> Option<JavaOutcome> {
    let tokens = ctx.matched_token_refs();
    let enclitic = tokens.last()?;
    let synth = ctx.synth?;
    let old_token = enclitic.word().as_str();
    let old_parts: Vec<&str> = old_token.split('-').collect();
    if old_parts.len() != 2 {
        return keep(ctx, replacements);
    }
    let old_verb = old_parts[0];
    let old_pronoun = old_parts[1];

    let mut suggestions: Vec<String> = Vec::new();
    for data in enclitic.word().tags() {
        let pos = data.pos().as_str();
        if !pos.starts_with('V') || !pos.contains(':') {
            continue;
        }
        let verb_tag = pos.split(':').next().unwrap_or(pos);
        let new_verb = synth
            .lookup(data.lemma().as_str(), verb_tag)
            .first()
            .cloned()?;
        let mut new_pronouns: Vec<String> = match old_pronoun {
            "lo" | "no" => vec!["o".to_string()],
            "la" | "na" => vec!["a".to_string()],
            "los" => vec!["os".to_string()],
            "las" | "nas" => vec!["as".to_string()],
            "nos" => {
                let mut v = vec!["nos".to_string()];
                if old_verb.ends_with('m')
                    || old_verb.ends_with("ão")
                    || old_verb.ends_with("õe")
                {
                    v.push("os".to_string());
                }
                v
            }
            other => vec![other.to_string()],
        };
        for pronoun in new_pronouns.drain(..) {
            let sugg = format!("{} {}", pronoun, new_verb);
            if !suggestions.contains(&sugg) {
                suggestions.push(sugg);
            }
        }
    }
    if suggestions.is_empty() {
        keep(ctx, replacements)
    } else {
        keep(ctx, suggestions)
    }
}

fn insert_comma_de(ctx: &mut FilterCtx, replacements: Vec<String>) -> Option<JavaOutcome> {
    let has_tag = |word: &str, prefix: &str| -> bool {
        ctx.tagger
            .get_tags_with_options(word, Some(false), Some(false))
            .any(|d| d.pos().as_str().starts_with(prefix))
    };

    let pattern_token_pos = ctx.pattern_token_pos();
    let sentence_tokens = ctx.sentence_tokens();
    let verb_at_1 = sentence_tokens
        .get(1)
        .map(|t| {
            t.word()
                .tags()
                .iter()
                .any(|d| d.pos().as_str().starts_with("ADV:"))
        })
        .unwrap_or(false);

    let mut suggestions: Vec<String> = Vec::new();
    for replacement in &replacements {
        let parts: Vec<&str> = replacement.split_whitespace().collect();
        let matches_word = |w: &str, re: &str| matches_full(&Regex::new(re.to_string()), w);
        if parts.len() == 2 {
            suggestions.push(format!("{}, {}", parts[0], parts[1]));
        } else if parts.len() == 3 {
            let t1 = has_tag(parts[0], "VER:");
            let t2 = has_tag(parts[1], "PRO:PER:");
            let t3 = has_tag(parts[2], "VER:");
            if t1 && t2 {
                suggestions.push(format!("{}, {} {}", parts[0], parts[1], parts[2]));
            } else if matches_word(parts[0], "[Ss]agt?")
                && parts[1] == "mal"
                && t3
            {
                suggestions.push(format!("{} {}, {}", parts[0], parts[1], parts[2]));
            } else if t1 && has_tag(parts[1], "ADV:") && t3 {
                suggestions.push(format!("{}, {} {}", parts[0], parts[1], parts[2]));
            }
        } else if (4..=7).contains(&parts.len()) {
            let rest1 = parts[1..].join(" ");
            if pattern_token_pos <= 2 || (pattern_token_pos == 3 && verb_at_1) {
                let t1 = has_tag(parts[0], "VER:");
                if parts.len() == 5
                    && t1
                    && has_tag(parts[1], "ART:")
                    && has_tag(parts[2], "SUB:")
                    && has_tag(parts[3], "SUB:")
                    && has_tag(parts[4], "VER:")
                {
                    suggestions.push(format!("{} {} {} {},", parts[0], parts[1], parts[2], parts[3]));
                } else if parts.len() == 4
                    && ctx.matched_tokens.len() >= 2
                    && {
                        let first = ctx.sentence.index(ctx.matched_tokens[0]);
                        first
                            .word()
                            .tags()
                            .iter()
                            .any(|d| d.pos().as_str().starts_with("VER:"))
                    }
                    && matches_word(
                        ctx.sentence.index(ctx.matched_tokens[1]).word().as_str(),
                        "der|die|das|seine|ihre|deine|unsere|meine|folgender|dieser",
                    )
                {
                    suggestions.push(format!("{}, {}", parts[0], rest1));
                } else if t1 && has_tag(parts[1], "PRO:POS:") && has_tag(parts[2], "SUB:") {
                    suggestions.push(format!("{}, {}", parts[0], rest1));
                } else if t1
                    && has_tag(parts[1], "PRO:PER:")
                    && has_tag(parts[2], "ADV:INR")
                {
                    let rest2 = parts[2..].join(" ");
                    suggestions.push(format!("{} {}, {}", parts[0], parts[1], rest2));
                } else if t1 && has_tag(parts[1], "PRO:POS:") && has_tag(parts[2], "ADJ:") {
                    suggestions.push(format!("{}, {}", parts[0], rest1));
                } else if matches_word(
                    parts[0],
                    "denke|dachte|glaube|schätze|vermute|behaupte",
                ) && has_tag(parts[1], "PRO:DEM:")
                    && has_tag(parts[2], "SUB:")
                {
                    suggestions.push(format!("{}, {}", parts[0], rest1));
                } else if pattern_token_pos == 1
                    && matches_word(parts[1], "bei|für|mit")
                    && matches_word(parts[2], "[Di]ir|[Dd]ich|[Ee]uer|[Ee]uch")
                    && has_tag(parts[3], "VER:")
                {
                    suggestions.push(format!("{}, {}", parts[0], rest1));
                }
            }
        }
    }
    keep(ctx, suggestions)
}

fn potential_compound_de(ctx: &mut FilterCtx, replacements: Vec<String>) -> Option<JavaOutcome> {
    let part1 = ctx.arg("part1")?;
    let part2 = ctx.arg("part2")?;

    let is_mixed = |s: &str| s.chars().any(char::is_uppercase) && s.chars().any(char::is_lowercase);
    let part2lowercase = if !is_mixed(&part2) && !is_all_uppercase(&part2) {
        part2.to_lowercase()
    } else {
        part2.clone()
    };
    let part2cap = if !is_mixed(&part2) && !is_all_uppercase(&part2) {
        uppercase_first(&part2.to_lowercase())
    } else {
        part2.clone()
    };
    let part1cap = if !is_mixed(&part1) && !is_all_uppercase(&part1) {
        uppercase_first(&part1.to_lowercase())
    } else {
        part1.clone()
    };

    let joined = format!("{}{}", part1cap, part2lowercase);
    let hyphenated = format!("{}-{}", part1cap, part2cap);

    let joined_known = ctx
        .tagger
        .get_tags_with_options(&joined, Some(false), Some(false))
        .next()
        .is_some()
        || is_known_word_ctx(ctx, &joined);

    let mut out = Vec::new();
    if !joined_known {
        // LT asks the spelling rule; a known analyzed word is accepted
        if joined.chars().count() > 20 {
            out.push(hyphenated.clone());
        }
        out.push(joined);
    } else {
        out.push(hyphenated);
    }
    keep(ctx, out)
}

// ---------------------------------------------------------------------------
// wave 2: catalan synth-based filters
// ---------------------------------------------------------------------------

/// `ApostophationHelper.getPrepositionAndDeterminer`.
fn preposition_and_determiner(new_form: &str, gender_number: &str, preposition: &str) -> String {
    let mut preposition = preposition.to_string();
    if !preposition.is_empty() {
        preposition = preposition
            .chars()
            .next()
            .map(|c| c.to_lowercase().to_string())
            .unwrap_or_default();
    }
    let starts_vowel = |re: &str| matches_full(&Regex::new(re.to_string()), new_form);
    let mut apos = "";
    if gender_number == "MS" {
        if starts_vowel("(?i)h?[aeiouàèéíòóú].*")
            && !starts_vowel("(?i)h?[ui][aeioàèéóò].+")
        {
            apos = "apos";
        }
    } else if gender_number == "FS" {
        if starts_vowel("(?i)h?[aeoàèéíòóú].*")
            || starts_vowel(
                "(?i)h?[ui][^aeiouàèéíòóúüï]+[aeiou][ns]?|urbs",
            )
        {
            if !starts_vowel("(?i)host|ira|inxa") {
                apos = "apos";
            }
        }
    }
    let key = format!("{}{}{}", preposition, gender_number, apos);
    match key.as_str() {
        "MS" => "el ".into(),
        "FS" => "la ".into(),
        "MP" => "els ".into(),
        "FP" => "les ".into(),
        "MSapos" => "l'".into(),
        "FSapos" => "l'".into(),
        "aMS" => "al ".into(),
        "aFS" => "a la ".into(),
        "aMP" => "als ".into(),
        "aFP" => "a les ".into(),
        "aMSapos" => "a l'".into(),
        "aFSapos" => "a l'".into(),
        "dMS" => "del ".into(),
        "dFS" => "de la ".into(),
        "dMP" => "dels ".into(),
        "dFP" => "de les ".into(),
        "dMSapos" => "de l'".into(),
        "dFSapos" => "de l'".into(),
        "pMS" => "pel ".into(),
        "pFS" => "per la ".into(),
        "pMP" => "pels ".into(),
        "pFP" => "per les ".into(),
        "pMSapos" => "per l'".into(),
        "pFSapos" => "per l'".into(),
        _ => String::new(),
    }
}

fn reading_with_tag_regex(token: &Token, pattern: &str) -> Option<(String, String, String)> {
    let re = Regex::new(pattern.to_string());
    token
        .word()
        .tags()
        .iter()
        .find(|d| matches_full(&re, d.pos().as_str()))
        .map(|d| {
            (
                token.word().as_str().to_string(),
                d.lemma().as_str().to_string(),
                d.pos().as_str().to_string(),
            )
        })
}

fn synthesize_with_determiner(ctx: &mut FilterCtx, replacements: Vec<String>) -> Option<JavaOutcome> {
    let lemma_from_str = ctx.arg("lemmaFrom")?;
    let lemma_select = ctx.arg("lemmaSelect")?;
    let synth_all = ctx.arg("synthAllForms").as_deref() == Some("true");
    let preposition_from = ctx.args_get("prepositionFrom").unwrap_or_default();

    let lemma_from = ctx.get_position(&lemma_from_str)?;
    let tokens = ctx.matched_token_refs();
    if lemma_from < 1 || lemma_from > tokens.len() {
        return None;
    }
    let word_token = tokens[lemma_from - 1];
    let original_word = word_token.word().as_str();

    let preposition = if !preposition_from.is_empty() {
        if preposition_from.chars().all(|c| c.is_ascii_digit()) {
            let pos = ctx.get_position(&preposition_from)?;
            tokens
                .get(pos)?
                .word()
                .as_str()
                .chars()
                .next()
                .map(|c| c.to_lowercase().to_string())
                .unwrap_or_default()
        } else {
            preposition_from.chars().next().map(|c| c.to_string()).unwrap_or_default()
        }
    } else {
        String::new()
    };

    let original_at = reading_with_tag_regex(word_token, &lemma_select)?;

    let second_gender_number = if lemma_from > 1 {
        tokens[lemma_from - 2]
            .word()
            .tags()
            .iter()
            .find(|d| d.pos().as_str().starts_with('D'))
            .and_then(|d| {
                let pos = d.pos().as_str();
                if pos.len() >= 5 {
                    Some(pos[3..5].to_string())
                } else {
                    None
                }
            })
            .unwrap_or_default()
    } else {
        String::new()
    };

    // all synth forms matching lemmaSelect; keep those equal to the original
    // word unless synthAllForms
    let select_re = ctx.regex_arg("lemmaSelect")?;
    let synth = ctx.synth?;
    let mut potential: Vec<(String, String)> = vec![(
        original_at.2.clone(),
        original_word.to_string(),
    )];
    for tag_forms in synth.synthesize_regex(&original_at.1, &select_re) {
        let tag = original_at.2.clone();
        let form = tag_forms;
        if !synth_all && !form.eq_ignore_ascii_case(original_word) {
            continue;
        }
        let priority = !second_gender_number.is_empty()
            && (tag.contains(&second_gender_number)
                || tag.contains(&{
                    let s = &second_gender_number;
                    format!("{}{}", &s[1..2], &s[0..1])
                }));
        let item = (tag, form);
        if !potential.iter().any(|(t, f)| *t == item.0 && *f == item.1) {
            if priority {
                potential.insert(1, item);
            } else {
                potential.push(item);
            }
        }
    }

    let is_sentence_start = tokens
        .first()
        .map(|t| t.span().char().start >= ctx.span.char().start - 1)
        .unwrap_or(false)
        && ctx.matched_tokens.first().map(|&i| i == 0).unwrap_or(false);

    const GN_PATTERNS: [(&str, &str); 4] = [
        ("MS", "(N|A.).[MC][SN].*|V.P.*SM."),
        ("FS", "(N|A.).[FC][SN].*|V.P.*SF."),
        ("MP", "(N|A.).[MC][PN].*|V.P.*PM."),
        ("FP", "(N|A.).[FC][PN].*|V.P.*PF."),
    ];

    let mut suggestions: Vec<String> = Vec::new();
    for (tag, form) in &potential {
        for (gn, pattern) in GN_PATTERNS {
            if matches_full(&Regex::new(pattern.to_string()), tag) {
                let mut sugg = format!(
                    "{}{}",
                    preposition_and_determiner(form, gn, &preposition),
                    preserve_case(form, original_word)
                );
                if is_sentence_start {
                    sugg = uppercase_first(&sugg);
                }
                if !suggestions.contains(&sugg) {
                    suggestions.push(sugg);
                }
            }
        }
    }

    let mut all = replacements;
    all.extend(suggestions);
    keep(ctx, all)
}

fn convert_to_gender_and_number(ctx: &mut FilterCtx, replacements: Vec<String>) -> Option<JavaOutcome> {
    let desired_gender_str = ctx.arg("gender").unwrap_or_default();
    let desired_number_str = ctx.arg("number").unwrap_or_default();
    let lemma_select = ctx.arg("lemmaSelect")?;
    let keep_original = ctx.arg("keepOriginal").as_deref() == Some("true");

    // the Java filter walks the WHOLE sentence (getTokensWithoutWhitespace,
    // SENT_START included) to pick up determiners/prepositions/adjectives
    // around the match - not just the matched tokens
    let tokens = ctx.sentence_tokens();
    // first token at/after the match start; LT also skips the SENT_START
    // token itself (`tokens[posWord].isSentenceStart()`), which shares the
    // match start offset when the match begins the sentence
    let mut pos_word = 0;
    while pos_word < tokens.len()
        && (tokens[pos_word].span().char().start < ctx.span.char().start
            || tokens[pos_word].span().char().start == tokens[pos_word].span().char().end)
    {
        pos_word += 1;
    }
    if pos_word >= tokens.len() {
        return None;
    }

    let synth = ctx.synth?;

    // splitGenderAndNumber: (prefix)(gender)(number)(rest)
    fn split_gender_number(pos: &str) -> Option<(String, String, String, String, bool)> {
        let chars: Vec<char> = pos.chars().collect();
        if chars.len() < 3 {
            return None;
        }
        let prefix_len = match chars[0] {
            'N' => 2,
            'A' | 'D' => 3,
            'V' => {
                if pos.starts_with("V.P") {
                    4
                } else {
                    return None;
                }
            }
            'P' => {
                if pos.starts_with("PX") {
                    3
                } else {
                    return None;
                }
            }
            _ => return None,
        };
        if chars.len() < prefix_len + 2 {
            return None;
        }
        let prefix: String = chars[..prefix_len].iter().collect();
        let g = chars[prefix_len];
        let n = chars[prefix_len + 1];
        let rest: String = chars[prefix_len + 2..].iter().collect();
        let is_verb = prefix.starts_with('V');
        Some((prefix, g.to_string(), n.to_string(), rest, is_verb))
    }

    let atr_noun = reading_with_tag_regex(tokens[pos_word], &lemma_select)?;
    let noun_split = split_gender_number(&atr_noun.2)?;
    let (noun_gender, noun_number) = if noun_split.4 {
        (noun_split.2.clone(), noun_split.1.clone())
    } else {
        (noun_split.1.clone(), noun_split.2.clone())
    };
    let desired_gender_str = if desired_gender_str.is_empty() {
        noun_gender.clone()
    } else {
        desired_gender_str
    };
    let desired_number_str = if desired_number_str.is_empty() {
        noun_number.clone()
    } else {
        desired_number_str
    };

    let synthesize_gn = |reading: &(String, String, String),
                         gender: &str,
                         number: &str|
     -> String {
        let split = match split_gender_number(&reading.2) {
            Some(s) => s,
            None => return String::new(),
        };
        let (gender, number) = if split.4 {
            (number.to_string(), gender.to_string())
        } else {
            (gender.to_string(), number.to_string())
        };
        let add_gender = if split.0.starts_with("DA") { "" } else { "C" };
        let target = format!(
            "{}[{}{}][{}N]{}",
            split.0, gender, add_gender, number, split.3
        );
        let re = Regex::new(target);
        synth
            .synthesize_regex(&reading.1, &re)
            .first()
            .cloned()
            .unwrap_or_default()
    };

    let mut start_pos = pos_word;
    let mut end_pos = pos_word;
    let mut suggestions: Vec<String> = Vec::new();

    for gender_ch in desired_gender_str.chars() {
        for number_ch in desired_number_str.chars() {
            let desired_gender = gender_ch.to_string();
            let desired_number = number_ch.to_string();
            let mut builder = String::new();
            let mut ignore = false;
            if !keep_original {
                let s = synthesize_gn(&atr_noun, &desired_gender, &desired_number);
                if s.is_empty() {
                    ignore = true;
                }
                builder.push_str(&s);
            } else {
                builder.push_str(tokens[pos_word].word().as_str());
            }

            let mut stop = false;
            let mut i = pos_word;
            let mut preposition_to_add = String::new();
            let mut add_determiner = false;
            let mut conditional = String::new();
            let mut add_tot = String::new();
            while !stop && i > 1 {
                i -= 1;
                let token = tokens[i];
                let has_special = token.word().tags().iter().any(|d| {
                    let p = d.pos().as_str();
                    p == "_perfet" || p == "_GV_"
                }) || token.chunks().iter().any(|c| c == "GV");
                let atr = if has_special {
                    None
                } else {
                    reading_with_tag_regex(
                        token,
                        "(A..|V.P..|D..|PX.)(.)(.)(.*)",
                    )
                };
                if let Some(atr) = atr {
                    if atr.2.starts_with("DA") {
                        add_determiner = true;
                        start_pos = i;
                    } else if !add_determiner {
                        let mut s = synthesize_gn(&atr, &desired_gender, &desired_number);
                        if s.is_empty() {
                            ignore = true;
                        }
                        if s == "bo" {
                            s = "bon".to_string();
                        }
                        // Java: insert(0, conditional); insert(0, " ");
                        // insert(0, s) -> s + space + conditional + rest
                        let space = if tokens[i + 1].has_space_before() { " " } else { "" };
                        builder = format!("{}{}{}{}", s, space, conditional, builder);
                        conditional.clear();
                        start_pos = i;
                        if atr.2.starts_with('D') && !atr.2.starts_with("DN") {
                            stop = true;
                        }
                    } else {
                        if atr.1 == "tot" {
                            let s = synthesize_gn(&atr, &desired_gender, &desired_number);
                            if !s.is_empty() {
                                add_tot = format!("{} ", s);
                                start_pos = i;
                            }
                        }
                        stop = true;
                    }
                } else {
                    let has = |p: &str| {
                        token
                            .word()
                            .tags()
                            .iter()
                            .any(|d| d.pos().as_str() == p)
                    };
                    if has("SPS00") || has("LOC_PREP") {
                        if add_determiner {
                            let mut prep = token.word().as_str().to_lowercase();
                            if prep == "pe" {
                                prep = "per".to_string();
                            }
                            if prep == "d'" {
                                prep = "de".to_string();
                            }
                            if prep == "a" || prep == "de" || prep == "per" {
                                preposition_to_add = prep;
                                start_pos = i;
                            }
                        }
                        stop = true;
                    } else if has("_PUNCT_CONT") || has("CC") {
                        if pos_word - i == 1 {
                            stop = true;
                        } else {
                            conditional = format!("{} {}", token.word().as_str(), conditional);
                        }
                    } else if has("RG") {
                        conditional = format!("{} {}", token.word().as_str(), conditional);
                    } else {
                        stop = true;
                    }
                }
            }

            stop = false;
            let mut i = pos_word;
            conditional.clear();
            let mut is_there_conjunction = false;
            while !stop && i < tokens.len() - 1 {
                i += 1;
                let token = tokens[i];
                let mut atr = reading_with_tag_regex(token, "(A..|V.P..|PX.)(.)(.)(.*)");
                let starts_nc = token
                    .word()
                    .tags()
                    .iter()
                    .any(|d| d.pos().as_str().starts_with("NC"));
                if is_there_conjunction && starts_nc {
                    atr = None;
                }
                if let Some(atr) = atr {
                    let s = synthesize_gn(&atr, &desired_gender, &desired_number);
                    if s.is_empty() {
                        ignore = true;
                    }
                    builder.push_str(&conditional);
                    conditional.clear();
                    builder.push_str(&format!(" {}", s));
                    end_pos = i;
                } else {
                    let has = |p: &str| {
                        token
                            .word()
                            .tags()
                            .iter()
                            .any(|d| d.pos().as_str() == p)
                    };
                    if has("RG") {
                        conditional = format!("{} {}", conditional, token.word().as_str());
                    } else if has("CC") {
                        is_there_conjunction = true;
                        conditional = format!("{} {}", conditional, token.word().as_str());
                    } else if has("_PUNCT_CONT") {
                        conditional = format!("{}{}", conditional, token.word().as_str());
                    } else {
                        stop = true;
                    }
                }
            }

            if add_determiner {
                let det = preposition_and_determiner(
                    &builder,
                    &format!("{}{}", desired_gender, desired_number),
                    &preposition_to_add,
                );
                builder = format!("{}{}", det, builder);
            } else if !preposition_to_add.is_empty() {
                builder = format!("{} {}", preposition_to_add, builder);
            }
            builder = format!("{}{}", add_tot, builder);
            let suggestion = preserve_case(&builder, tokens[start_pos].word().as_str());
            if end_pos == pos_word
                && start_pos == pos_word
                && tokens[pos_word].word().as_str() == suggestion
            {
                continue;
            }
            if !ignore {
                suggestions.push(suggestion);
            }
        }
    }

    if suggestions.is_empty() {
        return None;
    }
    let start_span = tokens[start_pos].span().clone();
    let end_span = tokens[end_pos].span().clone();
    let original = ctx
        .sentence
        .slice(Span::from_positions(start_span.start(), end_span.end()))
        .to_string();
    if suggestions.contains(&original) {
        return None;
    }
    Some(JavaOutcome {
        span: Span::from_positions(start_span.start(), end_span.end()),
        replacements: suggestions,
        message: None,
    })
    .or_else(|| keep(ctx, replacements))
}

fn possessius_redundants(ctx: &mut FilterCtx, replacements: Vec<String>) -> Option<JavaOutcome> {
    let tokens = ctx.sentence_tokens();
    let mut pos_possessive = ctx.pattern_token_pos();
    while pos_possessive < tokens.len()
        && !tokens[pos_possessive]
            .word()
            .tags()
            .iter()
            .any(|d| d.pos().as_str().starts_with("PX"))
    {
        pos_possessive += 1;
    }
    if pos_possessive >= tokens.len() {
        return None;
    }
    let possessive_tag = tokens[pos_possessive]
        .word()
        .tags()
        .iter()
        .find(|d| d.pos().as_str().starts_with("PX"))
        .map(|d| d.pos().as_str().to_string())?;
    let chars: Vec<char> = possessive_tag.chars().collect();
    let number = chars.get(6).map(|c| c.to_string()).unwrap_or_default();
    let persona = chars.get(2).map(|c| c.to_string()).unwrap_or_default();

    // LT walks left over chunk-tagged GV tokens
    let mut pos_verb = ctx.pattern_token_pos().saturating_sub(1);
    while pos_verb > 0 && tokens[pos_verb].chunks().iter().any(|c| c == "GV") {
        pos_verb -= 1;
    }
    pos_verb += 1;

    let mut pronoun_found = false;
    let mut has_some_pronoun = false;
    let mut pos_pronoun = pos_verb.saturating_sub(1);
    while !pronoun_found && pos_pronoun > 0
        && tokens[pos_pronoun]
            .word()
            .tags()
            .iter()
            .any(|d| d.pos().as_str().starts_with('P'))
    {
        has_some_pronoun = true;
        let tag = tokens[pos_pronoun]
            .word()
            .tags()
            .iter()
            .find(|d| d.pos().as_str().starts_with('P'))
            .map(|d| d.pos().as_str().to_string());
        if let Some(tag) = tag {
            let pchars: Vec<char> = tag.chars().collect();
            pronoun_found = pchars.get(2).map(|c| c.to_string()) == Some(persona.clone())
                && (number == "C" || pchars.get(4).map(|c| c.to_string()) == Some(number.clone()));
        }
        pos_pronoun -= 1;
    }
    pos_pronoun = ctx.pattern_token_pos() + 1;
    while !pronoun_found && pos_pronoun < tokens.len()
        && tokens[pos_pronoun]
            .word()
            .tags()
            .iter()
            .any(|d| d.pos().as_str().starts_with('P'))
    {
        has_some_pronoun = true;
        let tag = tokens[pos_pronoun]
            .word()
            .tags()
            .iter()
            .find(|d| d.pos().as_str().starts_with('P'))
            .map(|d| d.pos().as_str().to_string());
        if let Some(tag) = tag {
            let pchars: Vec<char> = tag.chars().collect();
            pronoun_found = pchars.get(2).map(|c| c.to_string()) == Some(persona.clone())
                && (number == "C" || pchars.get(4).map(|c| c.to_string()) == Some(number.clone()));
        }
        pos_pronoun += 1;
    }

    let apostrophe_needed = pos_possessive >= 1
        && tokens[pos_possessive - 1]
            .word()
            .tags()
            .iter()
            .any(|d| {
                let p = d.pos().as_str();
                p == "DA0MS0" || p == "DA0FS0"
            })
        && pos_possessive + 1 < tokens.len()
        && matches_full(
            &Regex::new("(?i)h?[aeiouàèéíòóú].*".to_string()),
            tokens[pos_possessive + 1].word().as_str(),
        );

    if pronoun_found {
        let (span, replacement) = if apostrophe_needed {
            (
                Span::from_positions(
                    tokens[pos_possessive - 1].span().start(),
                    tokens[pos_possessive + 1].span().end(),
                ),
                format!("l'{}", tokens[pos_possessive + 1].word().as_str()),
            )
        } else {
            (
                tokens[pos_possessive].span().clone(),
                String::new(),
            )
        };
        return Some(JavaOutcome {
            span,
            replacements: vec![replacement],
            message: None,
        });
    }

    if !has_some_pronoun {
        let dative = match (persona.as_str(), number.as_str()) {
            ("1", "S") => "em",
            ("2", "S") => "et",
            ("3", "S") | ("3", "C") => "li",
            ("1", "P") => "ens",
            ("2", "P") => "us",
            ("3", "P") => "els",
            _ => "li",
        };
        let verb_token = tokens[pos_verb].word().as_str();
        let is_inf_ger = tokens[pos_verb]
            .word()
            .tags()
            .iter()
            .any(|d| {
                let p = d.pos().as_str();
                p.starts_with("VMN") || p.starts_with("VMG")
            });
        let mut suggestion = if is_inf_ger {
            format!("{}{}", verb_token, dative)
        } else {
            format!("{}{}", preserve_case(dative, verb_token), verb_token.to_lowercase())
        };
        for i in pos_verb + 1..=pos_possessive.saturating_sub(2) {
            if tokens[i].has_space_before() {
                suggestion.push(' ');
            }
            suggestion.push_str(&tokens[i].word().as_str().to_lowercase());
        }
        if apostrophe_needed {
            suggestion.push(' ');
            suggestion.push_str(&format!("l'{}", tokens[pos_possessive + 1].word().as_str()));
        } else {
            for i in (pos_possessive.saturating_sub(1))..=(pos_possessive + 1).min(tokens.len() - 1) {
                if i == pos_possessive {
                    continue;
                }
                if tokens[i].has_space_before() {
                    suggestion.push(' ');
                }
                suggestion.push_str(tokens[i].word().as_str());
            }
        }
        return Some(JavaOutcome {
            span: Span::from_positions(
                tokens[pos_verb].span().start(),
                tokens[pos_possessive + 1].span().end(),
            ),
            replacements: vec![suggestion],
            message: None,
        });
    }

    None
}

// ---------------------------------------------------------------------------
// wave 2: PostponedAdjectiveConcordance (fr / es / ca), parametric port
// ---------------------------------------------------------------------------

struct AdjPatterns {
    nom: &'static str,
    nom_ms: &'static str,
    nom_fs: &'static str,
    nom_mp: &'static str,
    nom_mn: &'static str,
    nom_fp: &'static str,
    nom_cs: &'static str,
    nom_cp: &'static str,
    nom_det: &'static str,
    gn_: &'static str,
    gn_ms: &'static str,
    gn_fs: &'static str,
    gn_mp: &'static str,
    gn_fp: &'static str,
    gn_cs: &'static str,
    gn_cp: &'static str,
    det: &'static str,
    det_cs: &'static str,
    det_ms: &'static str,
    det_fs: &'static str,
    det_mp: &'static str,
    det_fp: &'static str,
    det_cp: &'static str,
    gn_ms_full: &'static str,
    gn_fs_full: &'static str,
    gn_mp_full: &'static str,
    gn_fp_full: &'static str,
    gn_cp_full: &'static str,
    gn_cs_full: &'static str,
    adj: &'static str,
    adj_ms: &'static str,
    adj_fs: &'static str,
    adj_mp: &'static str,
    adj_fp: &'static str,
    adj_cp: &'static str,
    adj_cs: &'static str,
    adj_mn: &'static str,
    adj_fn: &'static str,
    adj_s: &'static str,
    adj_p: &'static str,
    adj_m: &'static str,
    adj_f: &'static str,
    adverb: &'static str,
    conj: &'static str,
    punct: &'static str,
    loc_adv: &'static str,
    accepted_adverbs: &'static str,
    coordination: &'static str,
    keep_count: &'static str,
    keep_count2: &'static str,
    stop_count: &'static str,
    prepositions: &'static str,
    level_prep: &'static str,
    verb: &'static str,
    gv: &'static str,
    plus_word: &'static str,
    /// es/ca: prepend ", original" and shift the span one char left
    add_comma_variant: bool,
    /// synth postags used to build the suggestions
    synth_adj_cs: &'static str,
    synth_adj_cp: &'static str,
    synth_adj_p: &'static str,
    synth_adj_ms: &'static str,
    synth_adj_fs: &'static str,
    synth_adj_mp: &'static str,
    synth_adj_fp: &'static str,
}

const FR_ADJ: AdjPatterns = AdjPatterns {
    nom: "[NZ] .*",
    nom_ms: "[NZ] m s", nom_fs: "[NZ] f s", nom_mp: "[NZ] m p", nom_mn: "[NZ] m sp",
    nom_fp: "[NZ] f p", nom_cs: "[NZ] e s", nom_cp: "[NZ] e sp",
    nom_det: "[NZ] .*|(P\\+)?D .*",
    gn_: "_GN_.*", gn_ms: "_GN_MS", gn_fs: "_GN_FS", gn_mp: "_GN_MP", gn_fp: "_GN_FP",
    gn_cs: "_GN_[MF]S", gn_cp: "_GN_[MF]P",
    det: "(P\\+)?D .*", det_cs: "(P\\+)?D e s", det_ms: "(P\\+)?D m s",
    det_fs: "(P\\+)?D f s", det_mp: "(P\\+)?D m p", det_fp: "(P\\+)?D f p",
    det_cp: "(P\\+)?D e p",
    gn_ms_full: "[NZ] [me] (s|sp)|J [me] (s|sp)|V ppa m s|(P\\+)?D m (s|sp)",
    gn_fs_full: "[NZ] [fe] (s|sp)|J [fe] (s|sp)|V ppa f s|(P\\+)?D f (s|sp)",
    gn_mp_full: "[NZ] [me] (p|sp)|J [me] (p|sp)|V ppa m p|(P\\+)?D m (p|sp)",
    gn_fp_full: "[NZ] [fe] (p|sp)|J [fe] (p|sp)|V ppa f p|(P\\+)?D f (p|sp)",
    gn_cp_full: "[NZ] [fme] (p|sp)|J [fme] (p|sp)|(P\\+)?D [fme] (p|sp)",
    gn_cs_full: "[NZ] [fme] (s|sp)|J [fme] (s|sp)|(P\\+)?D [fme] (s|sp)",
    adj: "J .*|V ppa .*|PX.*",
    adj_ms: "J [me] (s|sp)|V ppa m s", adj_fs: "J [fe] (s|sp)|V ppa f s",
    adj_mp: "J [me] (p|sp)|V ppa m p", adj_fp: "J [fe] (p|sp)|V ppa f p",
    adj_cp: "J e (p|sp)", adj_cs: "J e (s|sp)", adj_mn: "J m sp", adj_fn: "J f sp",
    adj_s: "J .* (s|sp)|V ppa . s", adj_p: "J .* (p|sp)|V ppa . p",
    adj_m: "J [me] .*|V ppa [me] .*", adj_f: "J [fe] .*|V ppa [fe] .*",
    adverb: "A", conj: "C .*", punct: "_PUNCT", loc_adv: "A", accepted_adverbs: "A",
    coordination: "et|ou|ni", keep_count: "Y|J .*|N .*|D .*|P.*|V ppa .*|M nonfin|UNKNOWN|Z.*|V.* inf|V ppr",
    keep_count2: ",|et|ou|ni", stop_count: "[;:\\(\\)\\[\\]–—―‒]",
    prepositions: "P.*",
    level_prep: "d'|de|des|du|à|au|aux|en|dans|sur|entre|par|pour|avec|sans|contre|comme",
    verb: "V.* (inf|ind|sub|con|ppr|imp).*", gv: "_GV_", plus_word: "plus",
    add_comma_variant: false,
    synth_adj_cs: "J e p", synth_adj_cp: "J e s", synth_adj_p: "J . p|V ppa . p",
    synth_adj_ms: "J [me] sp?|V ppa m s", synth_adj_fs: "J [fe] sp?|V ppa f s",
    synth_adj_mp: "J [me] s?p|V ppa m p", synth_adj_fp: "J [fe] s?p|V ppa f p",
};

const ES_ADJ: AdjPatterns = AdjPatterns {
    nom: "N.*",
    nom_ms: "N.MS.*|PI0MS000", nom_fs: "N.FS.*|PI0FS000", nom_mp: "N.MP.*",
    nom_mn: "N.MN.*", nom_fp: "N.FP.*", nom_cs: "N.CS.*", nom_cp: "N.CP.*",
    nom_det: "N.*|D[NDA0I].*|PI0[MF]S000",
    gn_: "_GN_.*", gn_ms: "_GN_MS", gn_fs: "_GN_FS", gn_mp: "_GN_MP", gn_fp: "_GN_FP",
    gn_cs: "_GN_[MF]S", gn_cp: "_GN_[MF]P",
    det: "D[NDA0IP].*", det_cs: "D[NDA0IP]0CS0", det_ms: "D[NDA0IP]0MS0",
    det_fs: "D[NDA0IP]0FS0", det_mp: "D[NDA0IP]0MP0", det_fp: "D[NDA0IP]0FP0",
    det_cp: "D[NDA0IP]0CP0",
    gn_ms_full: "N.[MC][SN].*|A..[MC][SN].*|V.P..SM.?|PX.MS.*|D[NDA0I]0MS0|PI0MS000",
    gn_fs_full: "N.[FC][SN].*|A..[FC][SN].*|V.P..SF.?|PX.FS.*|D[NDA0I]0FS0|PI0FS000",
    gn_mp_full: "N.[MC][PN].*|A..[MC][PN].*|V.P..PM.?|PX.MP.*|D[NDA0I]0MP0",
    gn_fp_full: "N.[FC][PN].*|A..[FC][PN].*|V.P..PF.?|PX.FP.*|D[NDA0I]0FP0",
    gn_cp_full: "N.[FMC][PN].*|A..[FMC][PN].*|D[NDA0I]0[FM]P0",
    gn_cs_full: "N.[FMC][SN].*|A..[FMC][SN].*|D[NDA0I]0[FM]S0||PI0[MFC]S000",
    adj: "AQ.*|V.P.*|PX.*|.*LOC_ADJ.*",
    adj_ms: "A..[MC][SN].*|V.P..SM.?|PX.MS.*", adj_fs: "A..[FC][SN].*|V.P..SF.?|PX.FS.*",
    adj_mp: "A..[MC][PN].*|V.P..PM.?|PX.MP.*", adj_fp: "A..[FC][PN].*|V.P..PF.?|PX.FP.*",
    adj_cp: "A..C[PN].*", adj_cs: "A..C[SN].*", adj_mn: "", adj_fn: "",
    adj_s: "A...[SN].*|V.P..S..?|PX..S.*", adj_p: "A...[PN].*|V.P..P..?|PX..P.*",
    adj_m: "", adj_f: "",
    adverb: "R.|.*LOC_ADV.*", conj: "C.|.*LOC_CONJ.*", punct: "_PUNCT",
    loc_adv: ".*LOC_ADV.*", accepted_adverbs: "RG_before",
    coordination: "y|e|o|u|ni",
    keep_count: "A.*|N.*|D[NAIDP].*|SPS.*|SP:DA|.*LOC_ADV.*|V.P.*|_PUNCT.*|.*LOC_ADJ.*|PX.*|PI0.S000|UNKNOWN|V.N.{4}",
    keep_count2: ",|y|e|o|ni|u", stop_count: ";|lo",
    prepositions: "SP.*",
    level_prep: "de|del|en|sobre|a|entre|por|con|sin|contra|para",
    verb: "V.[^P].*|_GV_", gv: "_GV_", plus_word: "más",
    add_comma_variant: true,
    synth_adj_cs: "", synth_adj_cp: "", synth_adj_p: "A..P.|V.P..P|PX..P.*",
    synth_adj_ms: "A..MS.|V.P..SM|PX.MS.*", synth_adj_fs: "A..FS.|V.P..SF|PX.FS.*",
    synth_adj_mp: "A..MP.|V.P..PM|PX.MP.*", synth_adj_fp: "A..FP.|V.P..PF|PX.FP.*",
};

const CA_ADJ: AdjPatterns = AdjPatterns {
    nom: "N.*",
    nom_ms: "N.MS.*|PI0MS000", nom_fs: "N.FS.*|PI0FS000", nom_mp: "N.MP.*",
    nom_mn: "N.MN.*", nom_fp: "N.FP.*", nom_cs: "N.CS.*", nom_cp: "N.CP.*",
    nom_det: "N.*|D.*|PI0[MF]S000",
    gn_: "_GN_.*", gn_ms: "_GN_MS", gn_fs: "_GN_FS", gn_mp: "_GN_MP", gn_fp: "_GN_FP",
    gn_cs: "_GN_[MF]S", gn_cp: "_GN_[MF]P",
    det: "D.*", det_cs: "D..C S.*", det_ms: "D..M S.*",
    det_fs: "D..F S.*", det_mp: "D..M P.*", det_fp: "D..F P.*", det_cp: "D..C P.*",
    gn_ms_full: "N.[MC][SN].*|A..[MC][SN].*|V.P..SM.?|PX.MS.*|D..MS.*|PI0MS000",
    gn_fs_full: "N.[FC][SN].*|A..[FC][SN].*|V.P..SF.?|PX.FS.*|D..FS.*|PI0FS000",
    gn_mp_full: "N.[MC][PN].*|A..[MC][PN].*|V.P..PM.?|PX.MP.*|D..MP.*",
    gn_fp_full: "N.[FC][PN].*|A..[FC][PN].*|V.P..PF.?|PX.FP.*|D..FP.*",
    gn_cp_full: "N.[FMC][PN].*|A..[FMC][PN].*|D..[FM]P.*",
    gn_cs_full: "N.[FMC][SN].*|A..[FMC][SN].*|D..[FM]S.*",
    adj: "AQ.*|V.P.*|PX.*|.*LOC_ADJ.*",
    adj_ms: "A..[MC][SN].*|V.P..SM.?|PX.MS.*", adj_fs: "A..[FC][SN].*|V.P..SF.?|PX.FS.*",
    adj_mp: "A..[MC][PN].*|V.P..PM.?|PX.MP.*", adj_fp: "A..[FC][PN].*|V.P..PF.?|PX.FP.*",
    adj_cp: "A..C[PN].*", adj_cs: "A..C[SN].*", adj_mn: "", adj_fn: "",
    adj_s: "A...[SN].*|V.P..S..?|PX..S.*", adj_p: "A...[PN].*|V.P..P..?|PX..P.*",
    adj_m: "", adj_f: "",
    adverb: "R", conj: "C.*|.*LOC_CONJ.*", punct: "_PUNCT",
    loc_adv: ".*LOC_ADV.*", accepted_adverbs: "RG_before",
    coordination: "i|o|ni",
    keep_count: "A.*|N.*|D.*|SPS.*|SP.*|.*LOC_ADV.*|V.P.*|_PUNCT.*|.*LOC_ADJ.*|PX.*|PI0.S000|UNKNOWN|V.N.{4}",
    keep_count2: ",|i|o|ni", stop_count: "[;:\\(\\)\\[\\]]",
    prepositions: "SP.*",
    level_prep: "de|del|d'|en|sobre|a|entre|per|amb|sense|contra|per a",
    verb: "V.[^P].*|_GV_", gv: "_GV_", plus_word: "més",
    add_comma_variant: true,
    synth_adj_cs: "", synth_adj_cp: "", synth_adj_p: "A..P.|V.P..P|PX..P.*",
    synth_adj_ms: "A..MS.|V.P..SM|PX.MS.*", synth_adj_fs: "A..FS.|V.P..SF|PX.FS.*",
    synth_adj_mp: "A..MP.|V.P..PM|PX.MP.*", synth_adj_fp: "A..FP.|V.P..PF|PX.FP.*",
};

fn postponed_adjective(
    ctx: &mut FilterCtx,
    lang: PostponedAdjLang,
    replacements: Vec<String>,
) -> Option<JavaOutcome> {
    let p: &AdjPatterns = match lang {
        PostponedAdjLang::Fr => &FR_ADJ,
        PostponedAdjLang::Es => &ES_ADJ,
        PostponedAdjLang::Ca => &CA_ADJ,
    };

    let tokens = ctx.sentence_tokens();
    let i = ctx.pattern_token_pos();
    if i >= tokens.len() {
        return None;
    }

    let pos_matches = |idx: usize, pattern: &str| -> bool {
        if pattern.is_empty() {
            return false;
        }
        let re = Regex::new(pattern.to_string());
        tokens
            .get(idx)
            .map(|t| {
                t.word()
                    .tags()
                    .iter()
                    .any(|d| matches_full(&re, d.pos().as_str()))
            })
            .unwrap_or(false)
    };
    let word_matches = |idx: usize, pattern: &str| -> bool {
        let re = Regex::new(pattern.to_string());
        tokens
            .get(idx)
            .map(|t| matches_full(&re, t.word().as_str()))
            .unwrap_or(false)
    };

    const MAX_LEVELS: usize = 4;
    let mut is_plural = true;
    let mut is_prev_noun = false;
    let mut can_be_ms = false;
    let mut can_be_fs = false;
    let mut can_be_mp = false;
    let mut can_be_fp = false;
    let mut can_be_p = false;
    let mut c_nms = [0usize; MAX_LEVELS];
    let mut c_nfs = [0usize; MAX_LEVELS];
    let mut c_nmp = [0usize; MAX_LEVELS];
    let mut c_nmn = [0usize; MAX_LEVELS];
    let mut c_nfp = [0usize; MAX_LEVELS];
    let mut c_ncs = [0usize; MAX_LEVELS];
    let mut c_ncp = [0usize; MAX_LEVELS];
    let mut c_dms = [0usize; MAX_LEVELS];
    let mut c_dfs = [0usize; MAX_LEVELS];
    let mut c_dmp = [0usize; MAX_LEVELS];
    let mut c_dfp = [0usize; MAX_LEVELS];
    let mut c_nt = [0usize; MAX_LEVELS];
    let mut c_n = [0usize; MAX_LEVELS];
    let mut c_d = [0usize; MAX_LEVELS];
    let mut level = 0usize;

    let mut adverb_appeared = false;
    let mut conjunction_appeared = false;
    let mut punctuation_appeared = false;

    macro_rules! keep_counting {
        ($idx:expr) => {{
            let idx = $idx;
            if word_matches(idx, p.level_prep) || tokens.get(idx).map(|t| t.word().as_str() == ".").unwrap_or(false) {
                true
            } else if (adverb_appeared && conjunction_appeared)
                || (adverb_appeared && punctuation_appeared)
                || (conjunction_appeared && punctuation_appeared)
                || (punctuation_appeared && pos_matches(idx, p.punct))
            {
                false
            } else {
                (pos_matches(idx, p.keep_count)
                    || word_matches(idx, p.keep_count2)
                    || pos_matches(idx, p.accepted_adverbs))
                    && !word_matches(idx, p.stop_count)
                    && (!pos_matches(idx, p.gv) || pos_matches(idx, p.gn_))
            }
        }};
    }

    let mut j = 1usize;
    while i >= j && i - j > 0 && keep_counting!(i - j) && level < MAX_LEVELS {
        let idx = i - j;
        if !is_prev_noun {
            if pos_matches(idx, p.nom)
                || (idx >= 1
                    && !pos_matches(idx, p.nom)
                    && pos_matches(idx, p.adj)
                    && pos_matches(idx - 1, p.det))
            {
                if pos_matches(idx, p.gn_ms) {
                    c_nms[level] += 1;
                    can_be_ms = true;
                }
                if pos_matches(idx, p.gn_fs) {
                    c_nfs[level] += 1;
                    can_be_fs = true;
                }
                if pos_matches(idx, p.gn_mp) {
                    c_nmp[level] += 1;
                    can_be_mp = true;
                }
                if pos_matches(idx, p.gn_fp) {
                    c_nfp[level] += 1;
                    can_be_fp = true;
                }
            }
            if !pos_matches(idx, p.gn_) {
                if pos_matches(idx, p.nom_ms) {
                    c_nms[level] += 1;
                    can_be_ms = true;
                } else if pos_matches(idx, p.nom_fs) {
                    c_nfs[level] += 1;
                    can_be_fs = true;
                } else if pos_matches(idx, p.nom_mp) {
                    c_nmp[level] += 1;
                    can_be_mp = true;
                } else if pos_matches(idx, p.nom_mn) {
                    c_nmn[level] += 1;
                    can_be_ms = true;
                    can_be_mp = true;
                } else if pos_matches(idx, p.nom_fp) {
                    c_nfp[level] += 1;
                    can_be_fp = true;
                } else if pos_matches(idx, p.nom_cs) {
                    c_ncs[level] += 1;
                    can_be_ms = true;
                    can_be_fs = true;
                } else if pos_matches(idx, p.nom_cp) {
                    c_ncp[level] += 1;
                    can_be_fp = true;
                    can_be_mp = true;
                }
            }
        }
        if pos_matches(idx, p.nom) {
            c_nt[level] += 1;
            is_prev_noun = true;
        } else {
            is_prev_noun = false;
        }
        if pos_matches(idx, p.det_cs) {
            if pos_matches(idx + 1, p.nom_ms) {
                c_dms[level] += 1;
                can_be_ms = true;
            }
            if pos_matches(idx + 1, p.nom_fs) {
                c_dfs[level] += 1;
                can_be_fs = true;
            }
        }
        if pos_matches(idx, p.det_cp) {
            if pos_matches(idx + 1, p.nom_mp) {
                c_dms[level] += 1;
                can_be_mp = true;
            }
            if pos_matches(idx + 1, p.nom_fp) {
                c_dfs[level] += 1;
                can_be_fp = true;
            }
        }
        if !pos_matches(idx, p.adverb) {
            if pos_matches(idx, p.det_ms) {
                c_dms[level] += 1;
                can_be_ms = true;
            }
            if pos_matches(idx, p.det_fs) {
                c_dfs[level] += 1;
                can_be_fs = true;
            }
            if pos_matches(idx, p.det_mp) {
                c_dmp[level] += 1;
                can_be_mp = true;
            }
            if pos_matches(idx, p.det_fp) {
                c_dfp[level] += 1;
                can_be_fp = true;
            }
        }
        if idx >= 1
            && word_matches(idx, p.level_prep)
            && pos_matches(idx, p.prepositions)
            && !pos_matches(idx, p.conj)
            && !word_matches(idx - 1, p.coordination)
            && !pos_matches(idx + 1, p.adverb)
        {
            level += 1;
        }
        // update apparitions for this token
        conjunction_appeared |= pos_matches(idx, p.conj);
        if tokens.get(idx).map(|t| t.word().as_str() == "com").unwrap_or(false) {
            // skip
        } else if pos_matches(idx, p.nom) || pos_matches(idx, p.adj) {
            adverb_appeared = false;
            conjunction_appeared = false;
            punctuation_appeared = false;
        } else {
            adverb_appeared |= pos_matches(idx, p.adverb);
            punctuation_appeared |= pos_matches(idx, p.punct)
                || tokens.get(idx).map(|t| t.word().as_str() == ",").unwrap_or(false);
        }
        j += 1;
    }
    level += 1;
    let level = level.min(MAX_LEVELS);

    let mut c_ntotal = 0;
    let mut c_dtotal = 0;
    for lvl in 0..level {
        c_n[lvl] = c_nms[lvl] + c_nfs[lvl] + c_nmp[lvl] + c_nfp[lvl] + c_ncs[lvl] + c_ncp[lvl] + c_nmn[lvl];
        c_d[lvl] = c_dms[lvl] + c_dfs[lvl] + c_dmp[lvl] + c_dfp[lvl];
        c_ntotal += c_n[lvl];
        c_dtotal += c_d[lvl];
        if pos_matches(i, p.adj_mp)
            && (c_n[lvl] > 1 || c_d[lvl] > 1)
            && (c_nms[lvl] + c_nmn[lvl] + c_nmp[lvl] + c_ncs[lvl] + c_ncp[lvl] + c_dms[lvl] + c_dmp[lvl]) > 0
            && (c_nfs[lvl] + c_nfp[lvl] <= c_nt[lvl])
        {
            return None;
        }
        if pos_matches(i, p.adj_fp)
            && (c_n[lvl] > 1 || c_d[lvl] > 1)
            && ((c_nms[lvl] + c_nmp[lvl] + c_nmn[lvl] + c_dms[lvl] + c_dmp[lvl]) == 0
                || (c_nt[lvl] > 0 && c_nfs[lvl] + c_nfp[lvl] >= c_nt[lvl]))
        {
            return None;
        }
        if c_n[lvl] + c_d[lvl] > 0 {
            is_plural = is_plural && c_d[lvl] > 1 && level > 1;
            can_be_p = can_be_p || c_n[lvl] > 1;
        }
    }
    is_plural = is_plural
        || (i >= 2
            && c_nmp[0] + c_nfp[0] + c_ncp[0] > 0
            && tokens.get(i - 2).map(|t| t.word().as_str() == ",").unwrap_or(false));
    if c_ntotal == 0 && c_dtotal == 0 {
        return None;
    }

    // select patterns by the adjective's own morphology
    let (subst_pattern, adj_pattern, gn_pattern) = if pos_matches(i, p.adj_cs) {
        (p.gn_cs_full, p.adj_s, p.gn_cs)
    } else if pos_matches(i, p.adj_cp) {
        (p.gn_cp_full, p.adj_p, p.gn_cp)
    } else if !p.adj_mn.is_empty() && pos_matches(i, p.adj_mn) {
        (p.gn_ms_full, p.adj_m, p.gn_ms)
    } else if !p.adj_fn.is_empty() && pos_matches(i, p.adj_fn) {
        (p.gn_fs_full, p.adj_fn, p.gn_fs)
    } else if pos_matches(i, p.adj_ms) {
        (p.gn_ms_full, p.adj_ms, p.gn_ms)
    } else if pos_matches(i, p.adj_fs) {
        (p.gn_fs_full, p.adj_fs, p.gn_fs)
    } else if pos_matches(i, p.adj_mp) {
        (p.gn_mp_full, p.adj_mp, p.gn_mp)
    } else if pos_matches(i, p.adj_fp) {
        (p.gn_fp_full, p.adj_fp, p.gn_fp)
    } else {
        return None;
    };

    // a previous agreeing noun cancels the match
    let mut j = 1usize;
    let mut keep_going = true;
    while i >= j && i - j > 0 && keep_going {
        let idx = i - j;
        if pos_matches(idx, p.nom_det) && pos_matches(idx, gn_pattern) {
            return None;
        } else if !pos_matches(idx, p.gn_) && pos_matches(idx, subst_pattern) {
            return None;
        }
        keep_going = !pos_matches(idx, p.nom_det);
        j += 1;
    }

    // context check on the previous token
    let prev_ok = (i >= 1 && pos_matches(i - 1, p.nom) && !pos_matches(i - 1, subst_pattern))
        || (i >= 1 && pos_matches(i - 1, p.gn_) && !pos_matches(i - 1, gn_pattern))
        || (i >= 1 && pos_matches(i - 1, p.adj) && !pos_matches(i - 1, adj_pattern))
        || (i > 2
            && pos_matches(i - 1, p.accepted_adverbs)
            && !pos_matches(i - 2, p.verb)
            && !pos_matches(i - 2, p.prepositions))
        || (i > 3
            && pos_matches(i - 1, p.loc_adv)
            && pos_matches(i - 2, p.loc_adv)
            && !pos_matches(i - 3, p.verb)
            && !pos_matches(i - 3, p.prepositions));
    if !prev_ok {
        return None;
    }

    if !(is_plural && pos_matches(i, p.adj_s)) {
        let mut j = 1usize;
        while i >= j && i - j > 0 && keep_counting!(i - j) && (level > 1 || j < 4) {
            let idx = i - j;
            if !pos_matches(idx, p.gn_)
                && pos_matches(idx, p.nom_det)
                && pos_matches(idx, subst_pattern)
            {
                return None;
            } else if pos_matches(idx, gn_pattern) {
                return None;
            }
            j += 1;
        }
    }

    // synthesize the concordant forms
    let synth = ctx.synth?;
    let original_token = tokens.get(i)?.word().as_str().to_string();
    let lemma = tokens
        .get(i)?
        .word()
        .tags()
        .iter()
        .find(|d| {
            let re = Regex::new(p.adj.to_string());
            matches_full(&re, d.pos().as_str())
        })
        .map(|d| d.lemma().as_str().to_string())?;

    let mut suggestions: Vec<String> = Vec::new();
    let mut push = |tag: &str, suggs: &mut Vec<String>| {
        if tag.is_empty() {
            return;
        }
        let re = Regex::new(tag.to_string());
        for form in synth.synthesize_regex(&lemma, &re) {
            if !suggs.contains(&form) {
                suggs.push(form);
            }
        }
    };

    if !p.synth_adj_cs.is_empty() && pos_matches(i, p.adj_cs) {
        push(p.synth_adj_cs, &mut suggestions);
    }
    if !p.synth_adj_cp.is_empty() && suggestions.is_empty() && pos_matches(i, p.adj_cp) {
        push(p.synth_adj_cp, &mut suggestions);
    }
    if suggestions.is_empty() && is_plural {
        push(p.synth_adj_p, &mut suggestions);
    }
    if suggestions.is_empty() {
        if can_be_ms && !is_plural {
            push(p.synth_adj_ms, &mut suggestions);
        }
        if can_be_fs && !is_plural {
            push(p.synth_adj_fs, &mut suggestions);
        }
        if can_be_mp {
            push(p.synth_adj_mp, &mut suggestions);
        }
        if can_be_fp {
            push(p.synth_adj_fp, &mut suggestions);
        }
        if can_be_ms && (is_plural || can_be_p) {
            push(p.synth_adj_mp, &mut suggestions);
        }
        if can_be_fs && !can_be_ms && (is_plural || can_be_p) {
            push(p.synth_adj_fp, &mut suggestions);
        }
    }

    let lower_original = original_token.to_lowercase();
    suggestions.retain(|s| *s != lower_original);

    if p.add_comma_variant {
        let mut definitive: Vec<String> = Vec::new();
        definitive.push(format!(", {}", original_token));
        for s in &suggestions {
            definitive.push(format!(" {}", s));
        }
        let mut span = ctx.span.clone();
        let s = span.start();
        if s.char > 0 {
            let text = ctx.sentence.text();
            let bytes = text.as_bytes();
            let mut byte = s.byte;
            if byte > 0 {
                byte -= 1;
                while byte > 0 && !text.is_char_boundary(byte) {
                    byte -= 1;
                }
            }
            span.set_start(crate::types::Position { byte, char: s.char - 1 });
        }
        return Some(JavaOutcome {
            span,
            replacements: definitive,
            message: None,
        });
    }

    if suggestions.is_empty() {
        None
    } else {
        keep(ctx, suggestions)
    }
    .or_else(|| keep(ctx, replacements))
}

// ===== Catalan verb / clitic-pronoun filters (PronomsFeblesHelper port) =====

/// `PronomsFeblesHelper.pronomsFebles`: groups of 6 forms, one per
/// PronounPosition (DAVANT, DAVANT_APOS, DARRERE, DARRERE_APOS,
/// DARRERE_NOGUIONET_NOAPOS, DARRE_APOS_NOGUIONET_NOAPOS).
const PRONOMS_FEBLES: [&str; 498] = [
    "el", "l'", "-lo", "'l", "lo", "l", "els el", "els l'", "-los-el", "'ls-el", "losel", "lsel",
    "els els", "els els", "-los-els", "'ls-els", "losels", "lsels", "els en", "els n'", "-los-en",
    "'ls-en", "losen", "lsen", "els hi", "els hi", "-los-hi", "'ls-hi", "loshi", "lshi", "els ho",
    "els ho", "-los-ho", "'ls-ho", "losho", "lsho", "els la", "els l'", "-los-la", "'ls-la",
    "losla", "lsla", "els les", "els les", "-los-les", "'ls-les", "losles", "lsles", "els", "els",
    "-los", "'ls", "los", "ls", "em", "m'", "-me", "'m", "me", "m", "en", "n'", "-ne", "'n", "ne",
    "n", "ens el", "ens l'", "-nos-el", "'ns-el", "nosel", "nsel", "ens els", "ens els", "-nos-els",
    "'ns-els", "nosels", "nsels", "ens en", "ens n'", "-nos-en", "'ns-en", "nosen", "nsen", "ens hi",
    "ens hi", "-nos-hi", "'ns-hi", "noshi", "nshi", "ens ho", "ens ho", "-nos-ho", "'ns-ho",
    "nosho", "nsho", "ens la", "ens l'", "-nos-la", "'ns-la", "nosla", "nsla", "ens les",
    "ens les", "-nos-les", "'ns-les", "nosles", "nsles", "ens li", "ens li", "-nos-li", "'ns-li",
    "nosli", "nsli", "ens", "ens", "-nos", "'ns", "nos", "ns", "es", "s'", "-se", "'s", "se", "s",
    "et", "t'", "-te", "'t", "te", "t", "hi", "hi", "-hi", "-hi", "hi", "hi", "ho", "ho", "-ho",
    "-ho", "ho", "ho", "l'en", "el n'", "-l'en", "-l'en", "len", "len", "l'hi", "l'hi", "-l'hi",
    "-l'hi", "lhi", "lhi", "la hi", "la hi", "-la-hi", "-la-hi", "lahi", "lahi", "la", "l'", "-la",
    "-la", "la", "la", "la'n", "la n'", "-la'n", "-la'n", "lan", "lan", "les en", "les n'",
    "-les-en", "-les-en", "lesen", "lesen", "les hi", "les hi", "-les-hi", "-les-hi", "leshi",
    "leshi", "les", "les", "-les", "-les", "les", "les", "li hi", "li hi", "-li-hi", "-li-hi",
    "lihi", "lihi", "li ho", "li ho", "-li-ho", "-li-ho", "liho", "liho", "li la", "li l'",
    "-li-la", "-li-la", "lila", "lila", "li les", "li les", "-li-les", "-li-les", "liles", "liles",
    "li", "li", "-li", "-li", "li", "li", "li'l", "li l'", "-li'l", "-li'l", "lil", "lil", "li'ls",
    "li'ls", "-li'ls", "-li'ls", "lils", "lils", "li'n", "li n'", "-li'n", "-li'n", "lin", "lin",
    "m'hi", "m'hi", "-m'hi", "-m'hi", "mhi", "mhi", "m'ho", "m'ho", "-m'ho", "-m'ho", "mho", "mho",
    "me la", "me l'", "-me-la", "-me-la", "mela", "mela", "me les", "me les", "-me-les",
    "-me-les", "meles", "meles", "me li", "me li", "-me-li", "-me-li", "meli", "meli", "me'l",
    "me l'", "-me'l", "-me'l", "mel", "mel", "me'ls", "me'ls", "-me'ls", "-me'ls", "mels", "mels",
    "me'n", "me n'", "-me'n", "-me'n", "men", "men", "n'hi", "n'hi", "-n'hi", "-n'hi", "nhi",
    "nhi", "s'hi", "s'hi", "-s'hi", "-s'hi", "shi", "shi", "s'ho", "s'ho", "-s'ho", "-s'ho",
    "sho", "sho", "se la", "se l'", "-se-la", "-se-la", "sela", "sela", "se les", "se les",
    "-se-les", "-se-les", "seles", "seles", "se li", "se li", "-se-li", "-se-li", "seli", "seli",
    "se us", "se us", "-se-us", "-se-us", "seus", "seus", "se vos", "se vos", "-se-vos",
    "-se-vos", "sevos", "sevos", "se'l", "se l'", "-se'l", "-se'l", "sel", "sel", "se'ls",
    "se'ls", "-se'ls", "-se'ls", "sels", "sels", "se'm", "se m'", "-se'm", "-se'm", "sem", "sem",
    "se'n", "se n'", "-se'n", "-se'n", "sen", "sen", "se'ns", "se'ns", "-se'ns", "-se'ns", "sens",
    "sens", "se't", "se t'", "-se't", "-se't", "set", "set", "t'hi", "t'hi", "-t'hi", "-t'hi",
    "thi", "thi", "t'ho", "t'ho", "-t'ho", "-t'ho", "tho", "tho", "te la", "te l'", "-te-la",
    "-te-la", "tela", "tela", "te les", "te les", "-te-les", "-te-les", "teles", "teles",
    "te li", "te li", "-te-li", "-te-li", "teli", "teli", "te'l", "te l'", "-te'l", "-te'l",
    "tel", "tel", "te'ls", "te'ls", "-te'ls", "-te'ls", "tels", "tels", "te'm", "te m'", "-te'm",
    "-te'm", "tem", "tem", "te'n", "te n'", "-te'n", "-te'n", "ten", "ten", "te'ns", "te'ns",
    "-te'ns", "-te'ns", "tens", "tens", "us el", "us l'", "-vos-el", "-us-el", "vosel", "usel",
    "us els", "us els", "-vos-els", "-us-els", "vosels", "usels", "us em", "us m'", "-vos-em",
    "-us-em", "vosem", "usem", "us en", "us n'", "-vos-en", "-us-en", "vosen", "usen", "us ens",
    "us ens", "-vos-ens", "-us-ens", "vosens", "usens", "us hi", "us hi", "-vos-hi", "-us-hi",
    "voshi", "ushi", "us ho", "us ho", "-vos-ho", "-us-ho", "vosho", "usho", "us la", "us l'",
    "-vos-la", "-us-la", "vosla", "usla", "us les", "us les", "-vos-les", "-us-les", "vosles",
    "usles", "us li", "us li", "-vos-li", "-us-li", "vosli", "usli", "us", "us", "-vos", "-us",
    "vos", "us",
];

#[derive(Clone, Copy, PartialEq)]
enum PronounPosition {
    Davant = 0,
    DavantApos = 1,
    Darrere = 2,
    DarrereApos = 3,
}

fn transform_pronoun(input_pronom: &str, pos: PronounPosition) -> String {
    let mut i = 0;
    while i < PRONOMS_FEBLES.len() && !input_pronom.eq_ignore_ascii_case(PRONOMS_FEBLES[i]) {
        i += 1;
    }
    if i >= PRONOMS_FEBLES.len() {
        return input_pronom.to_string();
    }
    let pf_pos = 6 * (i / 6) + pos as usize;
    if pf_pos > PRONOMS_FEBLES.len() - 1 {
        // nonexistent pronoun, e.g. -t
        return String::new();
    }
    let pronom = PRONOMS_FEBLES[pf_pos];
    if pos == PronounPosition::Davant || (pos == PronounPosition::DavantApos && !pronom.ends_with('\''))
    {
        format!("{} ", pronom)
    } else {
        pronom.to_string()
    }
}

fn apostrophe_needed(word: &str) -> bool {
    matches_full(
        &Regex::new("(?i)h?[aeiouàèéíòóú].*".to_string()),
        word,
    )
}

fn apostrophe_needed_end(word: &str) -> bool {
    matches_full(&Regex::new("(?i).*[aei]".to_string()), word)
}

fn transform_davant(input_pronom: &str, next_word: &str) -> String {
    if apostrophe_needed(next_word) {
        transform_pronoun(input_pronom, PronounPosition::DavantApos)
    } else {
        transform_pronoun(input_pronom, PronounPosition::Davant)
    }
}

fn transform_darrere(input_pronom: &str, previous_word: &str) -> String {
    if apostrophe_needed_end(previous_word) {
        transform_pronoun(input_pronom, PronounPosition::DarrereApos)
    } else {
        transform_pronoun(input_pronom, PronounPosition::Darrere)
    }
}

fn dative_pronoun(persona_number: &str) -> Option<&'static str> {
    match persona_number {
        "1S" => Some("em"),
        "2S" => Some("et"),
        "3S" | "3C" => Some("li"),
        "1P" => Some("ens"),
        "2P" => Some("us"),
        "3P" => Some("els"),
        _ => None,
    }
}

fn add_en_apostrophe(k: &str) -> Option<&'static str> {
    Some(match k {
        "m'" => "me n'",
        "t'" => "te n'",
        "s'" => "se n'",
        "ens" => "ens n'",
        "us" => "us n'",
        "vos" => "vos n'",
        "li" => "li n'",
        "els" => "els n'",
        "se m'" => "se me n'",
        "se t'" => "se te n'",
        "se li" => "se li n'",
        "se'ns" => "se'ns n'",
        "se us" => "se us n'",
        "se vos" => "se vos n'",
        "se'ls" => "se'ls n'",
        "hi" => "n'hi ",
        "" => "n'",
        _ => return None,
    })
}

fn add_en(k: &str) -> Option<&'static str> {
    Some(match k {
        "em" => "me'n ",
        "et" => "te'n ",
        "es" | "se" => "se'n ",
        "ens" => "ens en ",
        "us" => "us en ",
        "li" => "li'n ",
        "els" => "els en ",
        "se'm" => "se me'n ",
        "se't" => "se te'n ",
        "se li" => "se li'n ",
        "se'ns" => "se'ns en ",
        "se us" => "se us en ",
        "se vos" => "se vos en ",
        "se'ls" => "se'ls en ",
        "hi" => "n'hi ",
        "" => "en ",
        _ => return None,
    })
}

fn add_hi_map(k: &str) -> Option<&'static str> {
    Some(match k {
        "em" => "m'hi",
        "et" => "t'hi",
        "es" | "se" => "s'hi",
        "ens" => "ens hi",
        "us" => "us hi",
        "li" => "li hi",
        "els" => "els hi",
        "" => "hi",
        _ => return None,
    })
}

fn remove_reflexive(k: &str) -> Option<&'static str> {
    Some(match k {
        "em" | "me" | "m'" | "et" | "te" | "t'" | "es" | "se" | "s'" | "ens" | "us" | "vos" => "",
        "se'm" | "se m'" => "em",
        "se't" => "et",
        "se t'" => "t'",
        "se l'" => "l'",
        "se la" => "la",
        "se li" => "li",
        "se'ns" => "ens",
        "se us" => "us",
        "se'ls" => "els",
        "s'ho" | "m'ho" | "t'ho" | "ens ho" | "us ho" | "vos ho" => "ho",
        "-me'l" | "-te'l" | "-se'l" | "-vos-el" | "-nos-el" => "-lo",
        "-me-la" | "-te-la" | "-se-la" | "-vos-la" | "-nos-la" => "-la",
        "-m'ho" | "-t'ho" | "-s'ho" | "-vos-ho" | "-nos-ho" => "-ho",
        _ => return None,
    })
}

fn add_reflexive_vowel(k: &str) -> Option<&'static str> {
    Some(match k {
        "1S" => "m'",
        "2S" => "t'",
        "3S" => "s'",
        "1P" => "ens ",
        "2P" => "us ",
        "3P" => "s'",
        _ => return None,
    })
}

fn add_reflexive_consonant(k: &str) -> Option<&'static str> {
    Some(match k {
        "1S" => "em ",
        "2S" => "et ",
        "3S" => "es ",
        "1P" => "ens ",
        "2P" => "us ",
        "3P" => "es ",
        _ => return None,
    })
}

fn add_reflexive_imperative(k: &str) -> Option<&'static str> {
    Some(match k {
        "2S" => "'t",
        "3S" => "'s",
        "1P" => "-nos",
        "2P" => "-vos",
        "3P" => "-se",
        _ => return None,
    })
}

fn add_es_en(k: &str) -> Option<&'static str> {
    Some(match k {
        "m'" | "em" | "me" => "se me'n ",
        "t'" | "et" | "te" => "se te'n ",
        "li" => "se li'n ",
        "ens" => "se'ns en ",
        "us" => "se us en ",
        "vos" => "se vos en ",
        "els" => "se'ls en ",
        _ => return None,
    })
}

fn add_es_en_apostrophe(k: &str) -> Option<&'static str> {
    Some(match k {
        "m'" | "em" | "me" => "se me n'",
        "t'" | "et" | "te" => "se te n'",
        "li" => "se li n'",
        "ens" => "se'ns n'",
        "us" => "se us n'",
        "vos" => "se vos n'",
        "els" => "se'ls n'",
        _ => return None,
    })
}

fn contains_reflexive_pronoun(pronouns: &str) -> bool {
    matches_full(
        &Regex::new("(?i).*([mts][e']|[e'][mts]|vos|us|ens|-nos|-vos).*".to_string()),
        pronouns,
    )
}

/// `PronomsFeblesHelper.pronomFeble`
const PRONOM_FEBLE_RE: &str = "P0.{6}|PP3CN000|PP3NN000|PP3..A00|PP[123]CP000|PP3CSD00";
/// `AdjustPronounsFilter.isPronoun` (slightly different variant set)
const IS_PRONOUN_RE: &str = "P0.{6}|PP3CN000|PP3NN000|PP3..A00|PP3CP000|PP3CSD00";

fn tok_matches_posre(token: &Token, pattern: &str) -> bool {
    let re = Regex::new(pattern.to_string());
    token
        .word()
        .tags()
        .iter()
        .any(|d| matches_full(&re, d.pos().as_str()))
}

fn tok_has_pos_start(token: &Token, prefix: &str) -> bool {
    token
        .word()
        .tags()
        .iter()
        .any(|d| d.pos().as_str().starts_with(prefix))
}

fn tok_has_any_lemma(token: &Token, lemmas: &[&str]) -> bool {
    token
        .word()
        .tags()
        .iter()
        .any(|d| lemmas.contains(&d.lemma().as_str()))
}

fn tok_first_reading(token: &Token) -> Option<(String, String)> {
    token
        .word()
        .tags()
        .iter()
        .next()
        .map(|d| (d.lemma().as_str().to_string(), d.pos().as_str().to_string()))
}

/// char-safe postag substring [a, b)
fn postag_sub(tag: &str, a: usize, b: usize) -> String {
    let chars: Vec<char> = tag.chars().collect();
    if chars.len() < b {
        return String::new();
    }
    chars[a..b].iter().collect()
}

/// First token at/after the match start (LT's `posWord` walk, index 0 being
/// the SENT_START token).
fn pos_word_start(ctx: &FilterCtx, tokens: &[&Token]) -> usize {
    let mut pos_word = 0;
    while pos_word < tokens.len()
        && (pos_word == 0 || tokens[pos_word].span().char().start < ctx.span.char().start)
    {
        pos_word += 1;
    }
    pos_word
}

/// The shared left walk of `AdjustPronounsFilter` / `AdjustVerbSuggestionsFilter`.
/// LT walks left over pronouns and the verbal group (GV chunk tags, set by
/// the ca/es disambiguator via `action="addchunk"`).
struct VerbWalk {
    pos_word: usize,
    to_left: usize,
    first_verb: String,
    first_verb_pos: usize,
    first_verb_inflected: bool,
    persona_number: String,
    persona_number_imperative: String,
    replacement_verb: String,
}

fn verb_walk(
    _ctx: &FilterCtx,
    tokens: &[&Token],
    pos_word: usize,
    synth: Option<&Synthesizer>,
    new_lemma: Option<&str>,
) -> VerbWalk {
    let mut walk = VerbWalk {
        pos_word,
        to_left: 0,
        first_verb: String::new(),
        first_verb_pos: 0,
        first_verb_inflected: false,
        persona_number: String::new(),
        persona_number_imperative: String::new(),
        replacement_verb: String::new(),
    };
    // change lemma if asked (only at the matched token)
    if let (Some(synth), Some(new_lemma)) = (synth, new_lemma) {
        let postags: Vec<String> = tokens[pos_word]
            .word()
            .tags()
            .iter()
            .filter(|d| d.pos().as_str().starts_with('V'))
            .map(|d| d.pos().as_str().to_string())
            .collect();
        let postag_refs: Vec<&str> = postags.iter().map(|s| s.as_str()).collect();
        let target_postag = synth.get_target_pos_tag(&postag_refs, "");
        if !target_postag.is_empty() {
            let forms = synth.synthesize_regex(new_lemma, &Regex::new(target_postag));
            if let Some(form) = forms.first() {
                walk.replacement_verb = form.clone();
            }
        }
    }
    let mut done = false;
    let mut in_pronouns = false;
    while !done && pos_word > walk.to_left {
        let current = tokens[pos_word - walk.to_left];
        let is_verb = tok_has_pos_start(current, "V");
        let is_pronoun = tok_matches_posre(current, IS_PRONOUN_RE);
        let is_in_gv = current.chunks().iter().any(|c| c == "GV");
        if is_pronoun {
            in_pronouns = true;
        }
        let accept = is_pronoun
            || (is_verb
                && !in_pronouns
                && !walk.first_verb_inflected
                && (walk.to_left == 0 || is_in_gv))
            || (is_in_gv && !walk.first_verb_inflected);
        if accept {
            if is_verb {
                walk.first_verb = current.word().as_str().to_string();
                walk.first_verb_pos = walk.to_left;
                walk.first_verb_inflected = tok_matches_posre(current, "V.[SI].*");
                if walk.first_verb_inflected {
                    if let Some((_, _, pos)) = reading_with_tag_regex(current, "V.[SI].*") {
                        walk.persona_number = postag_sub(&pos, 4, 6);
                    }
                }
                if tok_matches_posre(current, "V.M.*") {
                    if let Some((_, _, pos)) = reading_with_tag_regex(current, "V.M.*") {
                        walk.persona_number_imperative = postag_sub(&pos, 4, 6);
                    }
                }
            }
            walk.to_left += 1;
        } else {
            done = true;
            if walk.to_left > 0 {
                walk.to_left -= 1;
            }
        }
    }
    if pos_word == walk.to_left {
        // avoid the SENT_START token
        walk.to_left -= 1;
    }
    walk
}

/// pronouns in front of the verb (tokens in `[pos_word - to_left, pos_word - first_verb_pos)`)
fn build_pronouns_str(tokens: &[&Token], walk: &VerbWalk) -> String {
    let mut sb = String::new();
    let mut i = walk.pos_word.saturating_sub(walk.to_left);
    let end = walk.pos_word.saturating_sub(walk.first_verb_pos);
    while i < end {
        sb.push_str(tokens[i].word().as_str());
        if i + 1 < tokens.len() && tokens[i + 1].has_space_before() {
            sb.push(' ');
        }
        i += 1;
    }
    sb.trim().to_string()
}

fn build_verb_str(tokens: &[&Token], walk: &VerbWalk) -> String {
    let mut sb = String::new();
    let mut i = walk.pos_word.saturating_sub(walk.first_verb_pos);
    while i <= walk.pos_word {
        if i == walk.pos_word && !walk.replacement_verb.is_empty() {
            sb.push_str(&walk.replacement_verb);
        } else {
            sb.push_str(tokens[i].word().as_str());
        }
        if i + 1 < tokens.len() && tokens[i + 1].has_space_before() {
            sb.push(' ');
        }
        i += 1;
    }
    sb.trim().to_string()
}

/// `PronomsFeblesHelper.getTwoNextPronouns`: up to two clitic pronouns
/// attached after `from` (no whitespace between them).
fn get_two_next_pronouns(tokens: &[&Token], from: usize) -> (String, usize) {
    let mut pronoms = String::new();
    let mut num_pronouns = 0usize;
    if from < tokens.len() && !tokens[from].has_space_before() {
        if let Some(at) = reading_with_tag_regex(tokens[from], PRONOM_FEBLE_RE) {
            pronoms.push_str(&at.0);
            num_pronouns += 1;
            if from + 1 < tokens.len() && !tokens[from + 1].has_space_before() {
                if let Some(at2) = reading_with_tag_regex(tokens[from + 1], PRONOM_FEBLE_RE) {
                    pronoms.push_str(&at2.0);
                    num_pronouns += 1;
                }
            }
        }
    }
    (pronoms, num_pronouns)
}

/// `PronomsFeblesHelper.getPreviousPronouns`: clitic pronouns attached before
/// `to_index` (up to the governing infinitive / gerund / imperative).
fn get_previous_pronouns(tokens: &[&Token], to_index: usize) -> (String, usize) {
    let mut from_index = to_index;
    let mut num_pronouns = 0usize;
    let mut done = false;
    while from_index > 0 && !done {
        if reading_with_tag_regex(tokens[from_index], PRONOM_FEBLE_RE).is_some() {
            if from_index > 1
                && !tokens[from_index].has_space_before()
                && reading_with_tag_regex(tokens[from_index - 1], "V.[GNM].*").is_some()
            {
                done = true;
            } else if from_index > 2
                && !tokens[from_index].has_space_before()
                && !tokens[from_index - 1].has_space_before()
                && reading_with_tag_regex(tokens[from_index - 1], PRONOM_FEBLE_RE).is_some()
                && reading_with_tag_regex(tokens[from_index - 2], "V.[GNM].*").is_some()
            {
                done = true;
            }
            if !done {
                from_index -= 1;
                num_pronouns += 1;
            }
        } else {
            done = true;
        }
    }
    let mut pronouns = String::new();
    if num_pronouns > 0 {
        for j in from_index + 1..=to_index {
            if j > from_index + 1 && tokens[j].has_space_before() {
                pronouns.push(' ');
            }
            pronouns.push_str(tokens[j].word().as_str());
        }
    }
    (pronouns, num_pronouns)
}

fn do_add_pronoun_en(first_verb: &str, pronouns_str: &str, verb_str: &str) -> String {
    let transform = if apostrophe_needed(first_verb) {
        add_en_apostrophe
    } else {
        add_en
    };
    match transform(&pronouns_str.to_lowercase()) {
        Some(pr) => format!("{}{}", pr, verb_str.to_lowercase()),
        None => String::new(),
    }
}

fn do_add_pronoun_hi(pronouns_str: &str, verb_str: &str) -> String {
    match add_hi_map(&pronouns_str.to_lowercase()) {
        Some(pr) => format!("{} {}", pr, verb_str.to_lowercase()),
        None => String::new(),
    }
}

fn do_remove_pronoun_reflexive(
    pronouns_str: &str,
    verb_str: &str,
    pronouns_after: bool,
) -> String {
    let pr = remove_reflexive(&pronouns_str.to_lowercase());
    if pronouns_after {
        return match pr {
            Some(pr) => format!("{}{}", verb_str, pr),
            None => verb_str.to_string(),
        };
    }
    match pr {
        Some(pr) => format!("{} {}", pr, verb_str)
            .trim()
            .replace("' ", "'"),
        None => verb_str.to_string(),
    }
}

fn do_add_pronoun_reflexive(
    pronouns_str: &str,
    verb_str: &str,
    persona_number: &str,
    pronouns_after: bool,
) -> String {
    if pronouns_after {
        if contains_reflexive_pronoun(&pronouns_str.to_lowercase()) {
            return format!("{}{}", verb_str, pronouns_str);
        }
        if verb_str.ends_with('r') || verb_str.ends_with("re") {
            return format!("{}{}", verb_str, transform_darrere("-se", verb_str));
        }
        return verb_str.to_string();
    }
    if pronouns_str.is_empty() {
        let pronoun = if apostrophe_needed(verb_str) {
            add_reflexive_vowel(persona_number)
        } else {
            add_reflexive_consonant(persona_number)
        };
        return match pronoun {
            Some(p) => format!("{}{}", p, verb_str).trim().replace("' ", "'"),
            None => String::new(),
        };
    }
    format!("{} {}", pronouns_str, verb_str)
        .trim()
        .replace("' ", "'")
}

fn do_add_pronoun_reflexive_en(
    pronouns_str: &str,
    verb_str: &str,
    persona_number: &str,
    pronouns_after: bool,
) -> String {
    if pronouns_after {
        if contains_reflexive_pronoun(&pronouns_str.to_lowercase()) {
            return format!(
                "{}{}",
                verb_str,
                transform_darrere(&format!("{}'n", pronouns_str), verb_str)
            );
        }
        return format!("{}{}", verb_str, transform_darrere("-se'n", verb_str));
    }
    let needs_apostrophe = apostrophe_needed(verb_str);
    if pronouns_str.is_empty() {
        let pronoun = if needs_apostrophe {
            add_reflexive_vowel(persona_number)
                .and_then(|v| add_en_apostrophe(v.trim()))
        } else {
            add_reflexive_consonant(persona_number)
                .and_then(|v| add_en(v.trim()))
        };
        return match pronoun {
            Some(p) => format!("{}{}", p, verb_str).trim().replace("' ", "'"),
            None => String::new(),
        };
    }
    let pronoun = if needs_apostrophe {
        add_es_en_apostrophe(pronouns_str)
    } else {
        add_es_en(pronouns_str)
    };
    match pronoun {
        Some(p) => format!("{}{}", p, verb_str).trim().replace("' ", "'"),
        None => format!("{} {}", pronouns_str, verb_str)
            .trim()
            .replace("' ", "'"),
    }
}

fn do_add_pronoun_reflexive_imperative(
    pronouns_str: &str,
    verb_str: &str,
    persona_number: &str,
) -> String {
    if pronouns_str.is_empty() {
        if let Some(p) = add_reflexive_imperative(persona_number) {
            return format!("{}{}", verb_str, p).trim().to_string();
        }
    }
    String::new()
}

fn do_replace_em_en(pronouns_str: &str, verb_str: &str) -> String {
    if pronouns_str.eq_ignore_ascii_case("em") {
        return format!("en {}", verb_str);
    }
    if pronouns_str.eq_ignore_ascii_case("m'") {
        return format!("n'{}", verb_str);
    }
    if pronouns_str.eq_ignore_ascii_case("m'hi") {
        return format!("n'hi {}", verb_str);
    }
    String::new()
}

fn convert_pronouns_for_intransitive_verb(s: &str) -> String {
    s.replace("-se'l", "-se-li")
        .replace("se'l ", "se li ")
        .replace("l'", "li ")
        .replace("-lo", "-li")
        .replace("-la", "-li")
        .replace("la ", "li ")
        .replace("el ", "li ")
        .replace("ho", "hi")
}

fn fix_apostrophes(s: &str) -> String {
    let mut s = s.to_string();
    if matches_full(
        &Regex::new("(?i).*d'[^aeiouh].*".to_string()),
        &s,
    ) {
        s = s.replace("d'", "de ");
    }
    let re_missing =
        Regex::new("(?i)(.*)\\be([stm]) (h?[aeiouh].*)".to_string());
    if let Some(caps) = re_missing.captures(&s) {
        if let (Some(g1), Some(g2), Some(g3)) = (caps.get(1), caps.get(2), caps.get(3)) {
            s = format!("{}{}'{}", g1.as_str(), g2.as_str(), g3.as_str());
        }
    }
    let re_wrong = Regex::new("(?i)([mts])'([^aeiouh].*)".to_string());
    if let Some(caps) = re_wrong.captures(&s) {
        if let (Some(g1), Some(g2)) = (caps.get(1), caps.get(2)) {
            s = format!("e{} {}", g1.as_str(), g2.as_str());
        }
    }
    let re_hyphen = Regex::new("(.*)(-[stm])e-(h[oi])".to_string());
    if let Some(caps) = re_hyphen.captures(&s) {
        if let (Some(g1), Some(g2), Some(g3)) = (caps.get(1), caps.get(2), caps.get(3)) {
            s = format!("{}{}'{}", g1.as_str(), g2.as_str(), g3.as_str());
        }
    }
    s
}

/// `Catalan.adaptSuggestion`
pub(crate) fn ca_adapt_suggestion(s: &str) -> String {
    let capitalized = is_capitalized(s);
    let s = replace_regex_all(&Regex::new("\\b([Aa]|[Dd]e) e(ls?)\\b".to_string()), s, "$1$2");
    let s = replace_regex_all(
        &Regex::new("\\b([LDNSTMldnstm]['’]) ".to_string()),
        &s,
        "$1",
    );
    let s = replace_regex_all(
        &Regex::new("\\b([mtlsn])['’]([^1haeiouáàèéíòóúA-ZÀÈÉÍÒÓÚ“«\"])".to_string()),
        &s,
        "e$1 $2",
    );
    let s = replace_regex_all(
        &Regex::new("(?i)\\be?([mtsldn])e? (h?[aeiouàèéíòóú])".to_string()),
        &s,
        "$1'$2",
    );
    let s = replace_regex_all(
        &Regex::new("(?i)\\b(l)a ([aeoàúèéí][^ ])".to_string()),
        &s,
        "$1'$2",
    );
    let s = replace_regex_all(&Regex::new("\\b([mts]e) (['’])".to_string()), &s, "$1$2");
    let s = replace_regex_all(&Regex::new("\\bs'e(ns|ls)\\b".to_string()), &s, "se'$1");
    let s = replace_regex_all(
        &Regex::new("(?i)\\b(a|de|pe) (ls? )".to_string()),
        &s,
        "$1$2",
    );
    let mut s = s;
    if capitalized {
        s = uppercase_first(&s);
    }
    s.replace(" ,", ",")
}

fn replace_regex_all(regex: &Regex, text: &str, replacement: &str) -> String {
    regex.replace_all(text, replacement).to_string()
}

fn adjust_pronouns(ctx: &mut FilterCtx, _replacements: Vec<String>) -> Option<JavaOutcome> {
    let tokens = ctx.sentence_tokens();
    let pos_word = pos_word_start(ctx, &tokens);
    if pos_word >= tokens.len() {
        return None;
    }
    let actions: Vec<String> = ctx.arg("actions")?.split(',').map(|s| s.to_string()).collect();
    let new_lemma = ctx.arg("newLemma");
    let walk = verb_walk(ctx, &tokens, pos_word, ctx.synth, new_lemma.as_deref());
    if !walk.first_verb_inflected {
        return None;
    }
    let pronouns_str = build_pronouns_str(&tokens, &walk);
    let verb_str = build_verb_str(&tokens, &walk);
    let mut replacements: Vec<String> = Vec::new();
    for action in &actions {
        let replacement = match action.as_str() {
            "addPronounEn" => do_add_pronoun_en(&walk.first_verb, &pronouns_str, &verb_str),
            "removePronounReflexive" => {
                do_remove_pronoun_reflexive(&pronouns_str, &verb_str, false)
            }
            "replaceEmEn" => do_replace_em_en(&pronouns_str, &verb_str),
            "addPronounReflexive" => do_add_pronoun_reflexive(
                &pronouns_str,
                &verb_str,
                &walk.persona_number,
                false,
            ),
            "addPronounReflexiveHi" => do_add_pronoun_reflexive(
                &pronouns_str,
                &format!("hi {}", verb_str),
                &walk.persona_number,
                false,
            ),
            "addPronounReflexiveImperative" => do_add_pronoun_reflexive_imperative(
                &pronouns_str,
                &verb_str,
                &walk.persona_number_imperative,
            ),
            _ => String::new(),
        };
        if !replacement.is_empty() {
            replacements.push(
                preserve_case(
                    &replacement,
                    tokens[walk.pos_word - walk.to_left].word().as_str(),
                )
                .trim()
                .to_string(),
            );
        }
    }
    if replacements.is_empty() {
        return None;
    }
    Some(JavaOutcome {
        span: Span::from_positions(
            tokens[walk.pos_word - walk.to_left].span().start(),
            ctx.span.end(),
        ),
        replacements,
        message: None,
    })
}

fn adjust_verb_suggestions(ctx: &mut FilterCtx, replacements: Vec<String>) -> Option<JavaOutcome> {
    let tokens = ctx.sentence_tokens();
    let pos_word = pos_word_start(ctx, &tokens);
    if pos_word >= tokens.len() {
        return None;
    }
    let synth = ctx.synth?;
    let number_from_next_words = ctx.arg("numberFromNextWords").as_deref() == Some("true");
    let two_pronouns_after = get_two_next_pronouns(&tokens, pos_word + 1);
    let skip_walk = two_pronouns_after.1 > 0;
    let walk = if skip_walk {
        VerbWalk {
            pos_word,
            to_left: 0,
            first_verb: String::new(),
            first_verb_pos: 0,
            first_verb_inflected: false,
            persona_number: String::new(),
            persona_number_imperative: String::new(),
            replacement_verb: String::new(),
        }
    } else {
        verb_walk(ctx, &tokens, pos_word, None, None)
    };
    let mut replacement_verb = String::new();
    let mut out: Vec<String> = Vec::new();
    for original_suggestion in &replacements {
        let mut original_suggestion = original_suggestion.to_lowercase();
        let mut make_intransitive = false;
        if original_suggestion.ends_with(" [intr]") {
            original_suggestion =
                original_suggestion[..original_suggestion.len() - 7].to_string();
            make_intransitive = true;
        }
        let first_space_index = original_suggestion.find(' ');
        let (new_lemma, after_lemma) = match first_space_index {
            Some(i) => (
                original_suggestion[..i].to_string(),
                original_suggestion[i + 1..].to_string(),
            ),
            None => (original_suggestion.clone(), String::new()),
        };
        let mut desired_number = String::new();
        if !after_lemma.is_empty() && number_from_next_words {
            // LT analyzes afterLemma with a full JLanguageTool instance and
            // looks at the first token's number; approximate with the tagger
            if let Some(word) = after_lemma.split_whitespace().next() {
                desired_number = if ctx
                    .tagger
                    .get_tags_with_options(word, Some(false), Some(false))
                    .any(|d| d.pos().as_str().starts_with('S'))
                {
                    "S".to_string()
                } else {
                    "P".to_string()
                };
            }
        }
        if new_lemma == "haver" {
            desired_number = "S".to_string();
        }
        let mut action = "removePronounReflexive";
        let mut new_lemma = new_lemma;
        if new_lemma.ends_with("-se'n") {
            new_lemma = new_lemma[..new_lemma.len() - 5].to_string();
            action = "addPronounReflexiveEn";
        } else if new_lemma.ends_with("-se") {
            new_lemma = new_lemma[..new_lemma.len() - 3].to_string();
            action = "addPronounReflexive";
        } else if new_lemma.ends_with("-hi") {
            new_lemma = new_lemma[..new_lemma.len() - 3].to_string();
            action = "addPronounHi";
        } else if new_lemma.ends_with("-s'ho") {
            new_lemma = new_lemma[..new_lemma.len() - 5].to_string();
            action = "addPronounReflexiveHo";
        } else if new_lemma.ends_with("-s'hi") {
            new_lemma = new_lemma[..new_lemma.len() - 5].to_string();
            action = "addPronounReflexiveHi";
        }
        // synthesize with the new lemma
        let mut postags: Vec<String> = Vec::new();
        for d in tokens[pos_word].word().tags().iter() {
            let mut postag = d.pos().as_str().to_string();
            if !postag.starts_with('V') {
                continue;
            }
            if new_lemma == "haver" {
                postag = format!("VA{}", &postag[2..]);
            }
            if new_lemma == "ser" {
                postag = format!("VS{}", &postag[2..]);
            }
            if !desired_number.is_empty() {
                let c2 = postag_sub(&postag, 2, 3);
                let c5 = postag_sub(&postag, 5, 6);
                if c2 != "P" && (c5 == "S" || c5 == "P") {
                    postag = format!(
                        "{}{}{}",
                        &postag[..5],
                        desired_number,
                        &postag[6..]
                    );
                }
            }
            postags.push(postag);
        }
        let postag_refs: Vec<&str> = postags.iter().map(|s| s.as_str()).collect();
        let target_postag = synth.get_target_pos_tag(&postag_refs, "");
        if !target_postag.is_empty() {
            let forms = synth.synthesize_regex(&new_lemma, &Regex::new(target_postag));
            if let Some(form) = forms.first() {
                replacement_verb = form.clone();
            }
        }
        // rebuild the verb string, adjusting the number of the first verb
        let mut sb = String::new();
        let mut i = walk.pos_word.saturating_sub(walk.first_verb_pos);
        while i <= walk.pos_word {
            if i == walk.pos_word && !replacement_verb.is_empty() {
                sb.push_str(&replacement_verb);
            } else {
                let mut new_first_verb = tokens[i].word().as_str().to_string();
                if i == walk.pos_word - walk.first_verb_pos {
                    if let Some((_, lemma, postag)) = reading_with_tag_regex(tokens[i], "V.[SI].*") {
                        let number = postag_sub(&postag, 5, 6);
                        if number == "S"
                            || (number == "P"
                                && number != desired_number
                                && !desired_number.is_empty())
                        {
                            let new_postag = format!(
                                "{}{}{}",
                                &postag[..5],
                                desired_number,
                                &postag[6..]
                            );
                            let forms = synth.synthesize_regex(&lemma, &Regex::new(new_postag));
                            if let Some(form) = forms.first() {
                                new_first_verb = form.clone();
                            }
                        }
                    }
                }
                sb.push_str(&new_first_verb);
            }
            if i + 1 < tokens.len() && tokens[i + 1].has_space_before() {
                sb.push(' ');
            }
            i += 1;
        }
        let verb_str = sb.trim().to_lowercase();
        let mut pronouns_str = build_pronouns_str(&tokens, &walk);
        if !walk.first_verb_inflected {
            pronouns_str = two_pronouns_after.0.clone();
        }
        let pronouns_str = pronouns_str.to_lowercase();
        let mut pronouns_str_ref = pronouns_str.clone();
        let mut new_verb_str = verb_str.clone();
        let replacement = match action {
            "addPronounEn" => do_add_pronoun_en(&walk.first_verb, &pronouns_str, &verb_str),
            "removePronounReflexive" => {
                do_remove_pronoun_reflexive(&pronouns_str, &verb_str, !walk.first_verb_inflected)
            }
            "addPronounReflexiveEn" => do_add_pronoun_reflexive_en(
                &pronouns_str,
                &verb_str,
                &walk.persona_number,
                !walk.first_verb_inflected,
            ),
            "replaceEmEn" => {
                do_replace_em_en(&pronouns_str, &verb_str)
            }
            "addPronounReflexive" => do_add_pronoun_reflexive(
                &pronouns_str,
                &verb_str,
                &walk.persona_number,
                !walk.first_verb_inflected,
            ),
            "addPronounReflexiveHi" => do_add_pronoun_reflexive(
                "",
                &format!("hi {}", verb_str),
                &walk.persona_number,
                !walk.first_verb_inflected,
            ),
            "addPronounReflexiveHo" => {
                if walk.first_verb_inflected {
                    new_verb_str = format!("ho {}", verb_str);
                } else if !pronouns_str.is_empty() {
                    pronouns_str_ref = format!("{}-ho", pronouns_str);
                }
                do_add_pronoun_reflexive(
                    &pronouns_str_ref,
                    &new_verb_str,
                    &walk.persona_number,
                    !walk.first_verb_inflected,
                )
            }
            "addPronounHi" => do_add_pronoun_hi("", &verb_str),
            "addPronounReflexiveImperative" => do_add_pronoun_reflexive_imperative(
                &pronouns_str,
                &verb_str,
                &walk.persona_number_imperative,
            ),
            _ => String::new(),
        };
        if !replacement.is_empty() {
            let replacement = if make_intransitive {
                convert_pronouns_for_intransitive_verb(&replacement)
            } else {
                replacement
            };
            let replacement = fix_apostrophes(&replacement);
            out.push(
                preserve_case(
                    &format!("{} {}", replacement, after_lemma),
                    tokens[walk.pos_word - walk.to_left].word().as_str(),
                )
                .trim()
                .to_string(),
            );
        }
    }
    if out.is_empty() {
        return None;
    }
    Some(JavaOutcome {
        span: Span::from_positions(
            tokens[walk.pos_word - walk.to_left].span().start(),
            ctx.span.end(),
        ),
        replacements: out,
        message: None,
    })
}

fn anar_a_suggestions(ctx: &mut FilterCtx, _replacements: Vec<String>) -> Option<JavaOutcome> {
    let tokens = ctx.sentence_tokens();
    let init_pos = pos_word_start(ctx, &tokens);
    if init_pos + 2 >= tokens.len() {
        return None;
    }
    let verb_postag = reading_with_tag_regex(tokens[init_pos], "V.IP.*")?.2;
    let lemma = reading_with_tag_regex(tokens[init_pos + 2], "V.N.*")?.1;
    let new_postag = format!("V[MS]I[PF]{}", postag_sub(&verb_postag, 4, 8));
    let synth = ctx.synth?;
    let synth_forms = synth.synthesize_regex(&lemma, &Regex::new(new_postag));
    if synth_forms.is_empty() {
        return None;
    }
    let (pronoms_darrere, adjust_end_pos) = get_two_next_pronouns(&tokens, init_pos + 3);
    let mut replacements: Vec<String> = Vec::new();
    for verb in &synth_forms {
        let mut suggestion = String::new();
        if !pronoms_darrere.is_empty() {
            suggestion = transform_davant(&pronoms_darrere, verb);
        }
        suggestion.push_str(verb);
        replacements.push(preserve_case(&suggestion, tokens[init_pos].word().as_str()));
    }
    if replacements.is_empty() {
        return None;
    }
    let end_idx = init_pos + 2 + adjust_end_pos;
    if end_idx >= tokens.len() {
        return None;
    }
    Some(JavaOutcome {
        span: Span::from_positions(
            tokens[init_pos].span().start(),
            tokens[end_idx].span().end(),
        ),
        replacements,
        message: None,
    })
}

fn donar_temps_suggestions(ctx: &mut FilterCtx, _replacements: Vec<String>) -> Option<JavaOutcome> {
    let tokens = ctx.sentence_tokens();
    let pos_word = pos_word_start(ctx, &tokens);
    let synth = ctx.synth?;
    if pos_word >= tokens.len() {
        return None;
    }
    let pronom_postag = reading_with_tag_regex(tokens[pos_word], "P.*")?.2;
    let pronom_gender_number = format!(
        "{}{}",
        postag_sub(&pronom_postag, 2, 3),
        postag_sub(&pronom_postag, 4, 5)
    );
    let index_first_verb = pos_word + 1;
    let mut index_main_verb = index_first_verb;
    while index_main_verb < tokens.len()
        && !tok_has_any_lemma(tokens[index_main_verb], &["donar"])
    {
        index_main_verb += 1;
    }
    if index_main_verb + 1 >= tokens.len() {
        return None;
    }
    let verb_postag = reading_with_tag_regex(tokens[index_main_verb], "V.*")?.2;

    // haver-hi temps
    let synth_forms = synth.synthesize_regex(
        "haver",
        &Regex::new(format!("VA{}", postag_sub(&verb_postag, 2, 8))),
    );
    let mut suggestion1 = String::new();
    if !synth_forms.is_empty() {
        suggestion1.push_str("hi");
        let mut index = index_first_verb;
        while index < index_main_verb {
            if tokens[index].has_space_before() || suggestion1.chars().count() == 2 {
                suggestion1.push(' ');
            }
            suggestion1.push_str(tokens[index].word().as_str());
            index += 1;
        }
        suggestion1.push_str(&format!(" {} temps", synth_forms[0]));
    }
    let mut replacements: Vec<String> = Vec::new();
    let sugg1 = preserve_case(
        &suggestion1.replace("de haver", "d'haver"),
        tokens[pos_word].word().as_str(),
    );
    if !sugg1.is_empty() {
        replacements.push(sugg1);
    }

    // tenir temps
    let mut suggestion2 = String::new();
    if index_first_verb == index_main_verb {
        let synth_forms2 = synth.synthesize_regex(
            "tenir",
            &Regex::new(format!(
                "{}{}{}",
                postag_sub(&verb_postag, 0, 4),
                pronom_gender_number,
                postag_sub(&verb_postag, 6, 8)
            )),
        );
        if let Some(form) = synth_forms2.first() {
            suggestion2 = format!("{} temps", form);
        }
    } else if let Some((lemma2, postag2)) = tok_first_reading(tokens[index_first_verb]) {
        let synth_forms2 = synth.synthesize_regex(
            &lemma2,
            &Regex::new(format!(
                "{}{}{}",
                postag_sub(&postag2, 0, 4),
                pronom_gender_number,
                postag_sub(&postag2, 6, 8)
            )),
        );
        if let Some(form) = synth_forms2.first() {
            suggestion2 = form.clone();
            let mut index = index_first_verb + 1;
            while index < index_main_verb {
                if tokens[index].has_space_before() {
                    suggestion2.push(' ');
                }
                suggestion2.push_str(tokens[index].word().as_str());
                index += 1;
            }
            if let Some((lemma_main, postag_main)) = tok_first_reading(tokens[index_main_verb]) {
                let synth_forms3 = synth.synthesize_regex(&lemma_main, &Regex::new(postag_main));
                if let Some(form3) = synth_forms3.first() {
                    suggestion2.push_str(&format!(" {} temps", form3));
                } else {
                    suggestion2 = String::new();
                }
            }
        }
    }
    let sugg2 = preserve_case(&suggestion2, tokens[pos_word].word().as_str());
    if !sugg2.is_empty() {
        replacements.push(sugg2);
    }
    if replacements.is_empty() {
        return None;
    }
    Some(JavaOutcome {
        span: Span::from_positions(
            tokens[pos_word].span().start(),
            tokens[index_main_verb + 1].span().end(),
        ),
        replacements,
        message: None,
    })
}

fn olidarse_suggestions(ctx: &mut FilterCtx, replacements: Vec<String>) -> Option<JavaOutcome> {
    let tokens = ctx.sentence_tokens();
    let pos_word = pos_word_start(ctx, &tokens);
    let synth = ctx.synth?;
    if pos_word + 2 >= tokens.len() {
        return None;
    }
    let pronom_postag = reading_with_tag_regex(tokens[pos_word + 1], "P.*")?.2;
    let pronom_gender_number = format!(
        "{}{}",
        postag_sub(&pronom_postag, 2, 3),
        postag_sub(&pronom_postag, 4, 5)
    );
    let mut index_main_verb = pos_word + 2;
    while index_main_verb < tokens.len()
        && !tok_has_any_lemma(
            tokens[index_main_verb],
            &["oblidar", "descuidar", "passar"],
        )
    {
        index_main_verb += 1;
    }
    if index_main_verb >= tokens.len() {
        return None;
    }
    let (_, mut lemma, verb_postag) = reading_with_tag_regex(tokens[pos_word + 2], "V.*")?;
    if lemma == "passar" {
        lemma = "descuidar".to_string();
    }
    let synth_forms = synth.synthesize_regex(
        &lemma,
        &Regex::new(format!(
            "{}{}{}",
            postag_sub(&verb_postag, 0, 4),
            pronom_gender_number,
            postag_sub(&verb_postag, 6, 8)
        )),
    );
    let mut new_verb = synth_forms.first()?.clone();
    let mut i = pos_word + 3;
    while i < index_main_verb + 1 {
        if tokens[i].has_space_before() {
            new_verb.push(' ');
        }
        new_verb.push_str(
            &tokens[i]
                .word()
                .as_str()
                .replace("passar", "descuidar")
                .replace("passat", "descuidat")
                .replace("passant", "descuidant"),
        );
        i += 1;
    }
    let verb_vowel = apostrophe_needed(&new_verb);
    let mut word_after = String::new();
    if index_main_verb + 1 < tokens.len() {
        if let Some(at) = reading_with_tag_regex(
            tokens[index_main_verb + 1],
            "D.*|V.N.*|P[DI].*|NC.*",
        ) {
            word_after = at.0;
        }
        let next_word = tokens[index_main_verb + 1].word().as_str().to_lowercase();
        if ["com", "de", "d'", "que"].contains(&next_word.as_str()) {
            word_after = tokens[index_main_verb + 1].word().as_str().to_string();
        }
    }
    let use_en = word_after.is_empty()
        && !word_after.eq_ignore_ascii_case("de")
        && !word_after.eq_ignore_ascii_case("d'")
        && !word_after.eq_ignore_ascii_case("que");
    let transform: fn(&str) -> Option<&'static str> = if use_en {
        if verb_vowel {
            add_reflexive_en_vowel
        } else {
            add_reflexive_en_consonant
        }
    } else if verb_vowel {
        add_reflexive_vowel
    } else {
        add_reflexive_consonant
    };
    let mut sugg = transform(&pronom_gender_number).unwrap_or("").to_string();
    sugg.push_str(&new_verb);
    let mut characters_after_correction = 0usize;
    if word_after.eq_ignore_ascii_case("el") || word_after.eq_ignore_ascii_case("els") {
        sugg.push_str(&format!(" d{}", word_after.to_lowercase()));
        characters_after_correction = word_after.chars().count() + 1;
    } else if !word_after.is_empty()
        && !word_after.eq_ignore_ascii_case("de")
        && !word_after.eq_ignore_ascii_case("d'")
        && !word_after.eq_ignore_ascii_case("que")
    {
        let word_after_apostrophe = apostrophe_needed(&word_after);
        sugg.push_str(if word_after_apostrophe { " d'" } else { " de" });
        if word_after_apostrophe {
            characters_after_correction = 1;
        }
    }
    let mut out = vec![preserve_case(&sugg, tokens[pos_word].word().as_str())];
    for s in &replacements {
        let s = if characters_after_correction == 1 {
            format!("{} ", s)
        } else {
            s.clone()
        };
        out.push(ca_adapt_suggestion(&s));
    }
    let end = tokens[index_main_verb].span().end();
    let span_end = Position {
        byte: end.byte + characters_after_correction,
        char: end.char + characters_after_correction,
    };
    Some(JavaOutcome {
        span: Span::from_positions(tokens[pos_word].span().start(), span_end),
        replacements: out,
        message: Some(ctx.message.replace("passar", "descuidar")),
    })
}

fn add_reflexive_en_vowel(k: &str) -> Option<&'static str> {
    Some(match k {
        "1S" => "me n'",
        "2S" => "te n'",
        "3S" => "se n'",
        "1P" => "ens n'",
        "2P" => "us n'",
        "3P" => "se n'",
        _ => return None,
    })
}

fn add_reflexive_en_consonant(k: &str) -> Option<&'static str> {
    Some(match k {
        "1S" => "me'n ",
        "2S" => "te'n ",
        "3S" => "se'n ",
        "1P" => "ens en ",
        "2P" => "us en ",
        "3P" => "se'n ",
        _ => return None,
    })
}

fn portar_gerundi_suggestions(ctx: &mut FilterCtx, _replacements: Vec<String>) -> Option<JavaOutcome> {
    let tokens = ctx.sentence_tokens();
    let pos_word = pos_word_start(ctx, &tokens);
    let synth = ctx.synth?;
    if pos_word + 1 >= tokens.len() {
        return None;
    }
    let new_lemma = ctx.arg("newLemma").unwrap_or_default();
    let (_, atr1_lemma, atr1_postag) = reading_with_tag_regex(tokens[pos_word], "V.[IS].*")?;
    let atr1 = (atr1_lemma, atr1_postag);
    let atr2 = reading_with_tag_regex(tokens[pos_word + 1], "V.G.*")?;
    let lemma = if new_lemma.is_empty() {
        atr2.1.clone()
    } else {
        new_lemma
    };
    let mut replacements: Vec<String> = Vec::new();
    // he fet
    let synth_forms1 = synth.synthesize_regex(
        "haver",
        &Regex::new(format!("VA{}", postag_sub(&atr1.1, 2, atr1.1.chars().count()))),
    );
    let synth_forms2 = synth.synthesize_regex(&lemma, &Regex::new("V.P..SM.".to_string()));
    for f1 in &synth_forms1 {
        for f2 in &synth_forms2 {
            replacements.push(format!("{} {}", f1, f2));
        }
    }
    // faig
    let synth_forms3 = synth.synthesize_regex(
        &lemma,
        &Regex::new(format!("V.{}", postag_sub(&atr1.1, 2, atr1.1.chars().count()))),
    );
    if let Some(f3) = synth_forms3.first() {
        replacements.push(f3.clone());
    }
    if replacements.is_empty() {
        return None;
    }
    let next_pronouns = get_two_next_pronouns(&tokens, pos_word + 2);
    let previous_pronouns = if pos_word > 0 {
        get_previous_pronouns(&tokens, pos_word - 1)
    } else {
        (String::new(), 0)
    };
    let mut correct_start_index: isize = 0;
    let mut correct_end_index: usize = 0;
    for replacement in replacements.iter_mut() {
        let mut pronouns_suggestion = String::new();
        if !next_pronouns.0.is_empty() {
            pronouns_suggestion = transform_davant(&next_pronouns.0, replacement);
            correct_end_index = next_pronouns.1;
        } else if !previous_pronouns.0.is_empty() {
            pronouns_suggestion = transform_davant(&previous_pronouns.0, replacement);
            correct_start_index = -(previous_pronouns.1 as isize);
        }
        let sample = tokens[(pos_word as isize + correct_start_index) as usize].word().as_str();
        *replacement = preserve_case(&format!("{}{}", pronouns_suggestion, replacement), sample);
    }
    let start_idx = (pos_word as isize + correct_start_index) as usize;
    Some(JavaOutcome {
        span: Span::from_positions(
            tokens[start_idx].span().start(),
            tokens[pos_word + 1 + correct_end_index].span().end(),
        ),
        replacements,
        message: None,
    })
}

fn portar_temps_suggestions(ctx: &mut FilterCtx, _replacements: Vec<String>) -> Option<JavaOutcome> {
    let tokens = ctx.sentence_tokens();
    let pos_word = pos_word_start(ctx, &tokens);
    let synth = ctx.synth?;
    if pos_word + 1 >= tokens.len() {
        return None;
    }
    let verb_postag = reading_with_tag_regex(tokens[pos_word], "V.*")?.2;
    let new_postag = format!(
        "{}[30][S0].{}",
        postag_sub(&verb_postag, 0, 4),
        postag_sub(&verb_postag, 7, 8)
    );
    let synth_forms = synth.synthesize_regex("fer", &Regex::new(new_postag));
    if synth_forms.is_empty() {
        return None;
    }
    let mut suggestion = synth_forms[0].clone();
    // walk over PTime-chunked tokens (set by the disambiguator via addchunk)
    let mut i = pos_word + 1;
    while i < tokens.len() && tokens[i].chunks().iter().any(|c| c == "PTime") {
        if tokens[i].has_space_before() {
            suggestion.push(' ');
        }
        suggestion.push_str(tokens[i].word().as_str());
        i += 1;
    }
    let last_token_pos = i;
    if last_token_pos + 1 >= tokens.len() {
        return None;
    }
    let last_token = tokens[last_token_pos];
    let mut adjust_end_pos: isize = 0;
    if last_token.word().as_str() == "que" {
        suggestion.push_str(" que");
    } else if tok_has_pos_start(last_token, "VMG") || tok_has_pos_start(last_token, "VSG") {
        suggestion.push_str(" que ");
        let (pronoms, n) = get_two_next_pronouns(&tokens, last_token_pos + 1);
        adjust_end_pos += n as isize;
        let lemma2 = reading_with_tag_regex(last_token, "V.G.*")?.1;
        let synth_forms2 = synth.synthesize_regex(
            &lemma2,
            &Regex::new(format!("V.I{}", postag_sub(&verb_postag, 3, 8))),
        );
        if synth_forms2.is_empty() {
            return None;
        }
        if !pronoms.is_empty() {
            suggestion.push_str(&transform_davant(&pronoms, &synth_forms2[0]));
        }
        suggestion.push_str(&synth_forms2[0]);
    } else if last_token.word().as_str() == "sense"
        && (tok_has_pos_start(tokens[last_token_pos + 1], "VSN")
            || tok_has_pos_start(tokens[last_token_pos + 1], "VMN"))
    {
        suggestion.push_str(" que no ");
        adjust_end_pos += 1;
        let (pronoms, n) = get_two_next_pronouns(&tokens, last_token_pos + 2);
        adjust_end_pos += n as isize;
        let lemma2 = reading_with_tag_regex(tokens[last_token_pos + 1], "V.N.*")?.1;
        let synth_forms2 = synth.synthesize_regex(
            &lemma2,
            &Regex::new(format!("V.I{}", postag_sub(&verb_postag, 3, 8))),
        );
        if synth_forms2.is_empty() {
            return None;
        }
        if !pronoms.is_empty() {
            suggestion.push_str(&transform_davant(&pronoms, &synth_forms2[0]));
        }
        suggestion.push_str(&synth_forms2[0]);
    } else if ["així", "a", "en", "ací", "aquí", "ahí", "allí", "allà", "de"]
        .contains(&last_token.word().as_str())
        || tok_has_pos_start(last_token, "AQ")
        || tok_has_pos_start(last_token, "VMP")
    {
        let synth_forms2 = synth.synthesize_regex(
            "estar",
            &Regex::new(format!("V.I{}", postag_sub(&verb_postag, 3, 8))),
        );
        if synth_forms2.is_empty() {
            return None;
        }
        suggestion.push_str(&format!(" que {}", synth_forms2[0]));
        adjust_end_pos -= 1;
    } else {
        return None;
    }
    let replacement = preserve_case(&suggestion, tokens[pos_word].word().as_str());
    if replacement.is_empty() {
        return None;
    }
    let end_idx = (last_token_pos as isize + adjust_end_pos) as usize;
    if end_idx >= tokens.len() {
        return None;
    }
    Some(JavaOutcome {
        span: Span::from_positions(
            tokens[pos_word].span().start(),
            tokens[end_idx].span().end(),
        ),
        replacements: vec![replacement],
        message: None,
    })
}

// ===== Arabic filters (ArabicTagManager / ArabicSynthesizer ports) =====

/// Arabic POS tags encode morphological flags at fixed character positions,
/// e.g. `VW1;M3H-faU;WS-`. Mirrors `ArabicTagManager`.
pub(crate) fn ar_is_noun(postag: &str) -> bool {
    postag.starts_with('N')
}

pub(crate) fn ar_is_verb(postag: &str) -> bool {
    postag.starts_with('V')
}

pub(crate) fn ar_is_stopword(postag: &str) -> bool {
    postag.starts_with('P')
}

fn ar_is_adj(postag: &str) -> bool {
    postag.starts_with("NA")
}

fn ar_is_masdar(postag: &str) -> bool {
    postag.starts_with("NM")
}

fn ar_flag_pos(postag: &str, flag_type: &str) -> Option<usize> {
    let prefix = if ar_is_noun(postag) {
        "NOUN_"
    } else if ar_is_verb(postag) {
        "VERB_"
    } else if ar_is_stopword(postag) {
        "PARTICLE_"
    } else {
        return None;
    };
    let pos = match (prefix.to_string() + flag_type).as_str() {
        "NOUN_WORDTYPE" => 0,
        "NOUN_CATEGORY" => 1,
        "NOUN_GENDER" => 4,
        "NOUN_NUMBER" => 5,
        "NOUN_CASE" => 6,
        "NOUN_INFLECT_MARK" => 7,
        "NOUN_CONJ" => 9,
        "NOUN_JAR" => 10,
        "NOUN_PRONOUN" => 11,
        "VERB_WORDTYPE" => 0,
        "VERB_CATEGORY" => 1,
        "VERB_TRANS" => 2,
        "VERB_GENDER" => 4,
        "VERB_NUMBER" => 5,
        "VERB_PERSON" => 6,
        "VERB_INFLECT_MARK" => 7,
        "VERB_TENSE" => 8,
        "VERB_VOICE" => 9,
        "VERB_CASE" => 10,
        "VERB_CONJ" => 12,
        "VERB_ISTIQBAL" => 13,
        "VERB_PRONOUN" => 14,
        "PARTICLE_WORDTYPE" => 0,
        "PARTICLE_CATEGORY" => 1,
        "PARTICLE_OPTION" => 2,
        "PARTICLE_GENDER" => 4,
        "PARTICLE_NUMBER" => 5,
        "PARTICLE_CASE" => 6,
        "PARTICLE_CONJ" => 8,
        "PARTICLE_JAR" => 9,
        "PARTICLE_PRONOUN" => 10,
        _ => return None,
    };
    Some(pos)
}

pub(crate) fn ar_get_flag(postag: &str, flag_type: &str) -> char {
    match ar_flag_pos(postag, flag_type) {
        Some(pos) => postag.chars().nth(pos).unwrap_or('-'),
        None => '-',
    }
}

pub(crate) fn ar_set_flag(postag: &str, flag_type: &str, flag: char) -> String {
    if let Some(pos) = ar_flag_pos(postag, flag_type) {
        let mut chars: Vec<char> = postag.chars().collect();
        if pos < chars.len() {
            chars[pos] = flag;
            return chars.into_iter().collect();
        }
    }
    postag.to_string()
}

fn ar_is_definite(postag: &str) -> bool {
    ar_is_noun(postag) && ar_get_flag(postag, "PRONOUN") == 'L'
}

pub(crate) fn ar_is_majrour(postag: &str) -> bool {
    let flag = ar_get_flag(postag, "CASE");
    flag == 'I' || flag == '-'
}

fn ar_is_dual(postag: &str) -> bool {
    ar_get_flag(postag, "NUMBER") == '2'
}

fn ar_is_attached(postag: &str) -> bool {
    (ar_is_noun(postag) || ar_is_verb(postag)) && ar_get_flag(postag, "PRONOUN") == 'H'
}

pub(crate) fn ar_is_unattached_noun(postag: &str) -> bool {
    ar_is_noun(postag) && ar_get_flag(postag, "PRONOUN") != 'H' && !postag.ends_with('X')
}

fn ar_has_jar(postag: &str) -> bool {
    ar_is_noun(postag) && ar_get_flag(postag, "JAR") != '-'
}

fn ar_has_pronoun(postag: &str) -> bool {
    ar_get_flag(postag, "PRONOUN") == 'H'
}

fn ar_has_conjunction(postag: &str) -> bool {
    let flag = ar_get_flag(postag, "CONJ");
    (ar_is_noun(postag) && flag != '-')
        || (ar_is_verb(postag) && flag != '-')
        || (ar_is_stopword(postag) && flag != 'W')
}

fn ar_jar_prefix(postag: &str) -> &'static str {
    if postag.is_empty() || !ar_is_noun(postag) {
        return "";
    }
    match ar_get_flag(postag, "JAR") {
        'L' => "ل",
        'K' => "ك",
        'B' => "ب",
        _ => "",
    }
}

fn ar_conjunction_prefix(postag: &str) -> &'static str {
    match ar_get_flag(postag, "CONJ") {
        'F' => "ف",
        'W' => "و",
        _ => "",
    }
}

fn ar_definite_prefix(postag: &str) -> &'static str {
    if postag.is_empty() {
        return "";
    }
    if ar_is_noun(postag) && ar_get_flag(postag, "PRONOUN") == 'L' {
        if ar_has_jar(postag) && ar_jar_prefix(postag) == "ل" {
            "ل"
        } else {
            "ال"
        }
    } else {
        ""
    }
}

fn ar_pronoun_suffix(postag: &str) -> &'static str {
    if postag.is_empty() {
        return "";
    }
    match ar_get_flag(postag, "PRONOUN") {
        'b' => "ني",
        'c' => "نا",
        'd' => "ك",
        'e' => "كما",
        'f' => "كم",
        'g' => "كن",
        'H' => "ه",
        'i' => "ها",
        'j' => "هما",
        'k' => "هم",
        'n' => "هن",
        _ => "",
    }
}

/// `ArabicTagManager.setProcleticFlags`: neutralize prefix flags.
fn ar_set_procletic_flags(postag: &str) -> String {
    if postag.is_empty() {
        return String::new();
    }
    if ar_is_verb(postag) {
        let mut t = ar_set_flag(postag, "CONJ", '-');
        t = ar_set_flag(&t, "ISTIQBAL", '-');
        t
    } else if ar_is_noun(postag) {
        let mut t = ar_set_flag(postag, "CONJ", '-');
        t = ar_set_flag(&t, "JAR", '-');
        if ar_is_definite(postag) {
            t = ar_set_flag(&t, "PRONOUN", '-');
        }
        t
    } else if ar_is_stopword(postag) {
        let mut t = ar_set_flag(postag, "CONJ", '-');
        t = ar_set_flag(&t, "JAR", '-');
        t
    } else {
        postag.to_string()
    }
}

/// `ArabicTagManager.mergePosTag`
fn ar_merge_pos_tag(source: &str, target: &str) -> String {
    if source.is_empty() {
        return target.to_string();
    }
    if target.is_empty() {
        return source.to_string();
    }
    if ar_is_noun(source) && ar_is_noun(target) {
        if source.chars().count() != target.chars().count() {
            return source.to_string();
        }
        ar_set_flag(source, "CATEGORY", ar_get_flag(target, "CATEGORY"))
    } else if ar_is_verb(source) && ar_is_verb(target) {
        if source.chars().count() != target.chars().count() {
            return source.to_string();
        }
        let t = ar_set_flag(source, "CATEGORY", ar_get_flag(target, "CATEGORY"));
        ar_set_flag(&t, "TRANS", ar_get_flag(target, "TRANS"))
    } else if ar_is_stopword(source) && ar_is_stopword(target) {
        if source.chars().count() != target.chars().count() {
            return source.to_string();
        }
        let t = ar_set_flag(source, "CATEGORY", ar_get_flag(target, "CATEGORY"));
        ar_set_flag(&t, "OPTION", ar_get_flag(target, "OPTION"))
    } else if (ar_is_stopword(source) && (ar_is_verb(target) || ar_is_noun(target)))
        || ((ar_is_verb(source) || ar_is_noun(source)) && ar_is_stopword(target))
    {
        let mut t = target.to_string();
        if ar_has_pronoun(source) {
            t = ar_set_flag(&t, "PRONOUN", ar_get_flag(source, "PRONOUN"));
        }
        t
    } else if (ar_is_verb(source) && ar_is_noun(target)) || (ar_is_noun(source) && ar_is_verb(target)) {
        let mut t = target.to_string();
        if ar_has_pronoun(source) {
            t = ar_set_flag(&t, "PRONOUN", ar_get_flag(source, "PRONOUN"));
        }
        ar_set_flag(&t, "CONJ", ar_get_flag(source, "CONJ"))
    } else {
        target.to_string()
    }
}

/// `ArabicTagger.getProclitic` (prefix extracted from the word surface)
fn ar_get_proclitic(word: &str, postag: &str) -> String {
    if postag.is_empty() {
        return String::new();
    }
    let prefix_len = if ar_is_verb(postag) {
        let mut n = 0;
        if ar_get_flag(postag, "CONJ") == 'W' {
            n += 1;
        }
        if ar_get_flag(postag, "ISTIQBAL") == 'S' {
            n += 1;
        }
        n
    } else if ar_is_noun(postag) {
        let mut n = 0;
        if ar_get_flag(postag, "CONJ") != '-' {
            n += 1;
        }
        if ar_get_flag(postag, "JAR") != '-' {
            n += 1;
        }
        if ar_is_definite(postag) {
            if ar_get_flag(postag, "JAR") == 'L' {
                n += 1;
            } else {
                n += 2;
            }
        }
        n
    } else {
        return String::new();
    };
    word.chars().take(prefix_len).collect()
}

/// `ArabicTagger.getEnclitic` (pronoun suffix extracted from the word surface)
fn ar_get_enclitic(word: &str, postag: &str) -> String {
    if postag.is_empty() {
        return String::new();
    }
    if ar_get_flag(postag, "PRONOUN") != '-' {
        // faithful order of the Java if/else chain (longest endings first)
        if word.ends_with("ها") {
            return "ها".to_string();
        }
        if word.ends_with("هما") {
            return "هما".to_string();
        }
        if word.ends_with("ه") {
            return "ه".to_string();
        }
        if word.ends_with("هم") {
            return "هم".to_string();
        }
        if word.ends_with("هن") {
            return "هن".to_string();
        }
        if word.ends_with("كما") {
            return "كما".to_string();
        }
        if word.ends_with("كم") {
            return "كم".to_string();
        }
        if word.ends_with("كن") {
            return "كن".to_string();
        }
        if word.ends_with("ني") {
            return "ني".to_string();
        }
        if word.ends_with("نا") {
            return "نا".to_string();
        }
        if word.ends_with("ك") {
            return "ك".to_string();
        }
        if (word == "عني" || word == "مني") && word.ends_with("ني") {
            return "ني".to_string();
        }
        if (word == "عنا" || word == "منا") && word.ends_with("نا") {
            return "نا".to_string();
        }
        String::new()
    } else {
        ar_pronoun_suffix(postag).to_string()
    }
}

/// `ArabicSynthesizer.correctStem`
fn ar_correct_stem(stem: &str, postag: &str) -> String {
    let mut correct_stem = stem.to_string();
    if ar_is_attached(postag) {
        correct_stem = correct_stem.strip_suffix('ه').unwrap_or(&correct_stem).to_string();
    }
    if ar_is_definite(postag) {
        correct_stem = format!("{}{}", ar_definite_prefix(postag), correct_stem);
    }
    if ar_has_jar(postag) {
        correct_stem = format!("{}{}", ar_jar_prefix(postag), correct_stem);
    }
    if ar_has_conjunction(postag) {
        correct_stem = format!("{}{}", ar_conjunction_prefix(postag), correct_stem);
    }
    correct_stem
}

/// `ArabicSynthesizer.setEncliticMultiple`
fn ar_set_enclitic_multiple(
    synth: &Synthesizer,
    word: &str,
    lemma: &str,
    postag: &str,
    suffix: &str,
) -> Vec<String> {
    let default_wordlist = vec![format!("({})", word)];
    if postag.is_empty() {
        return default_wordlist;
    }
    let flag = if suffix.is_empty() { '-' } else { 'H' };
    let procletic = ar_get_proclitic(word, postag);
    let mut new_postag = ar_set_flag(postag, "PRONOUN", flag);
    new_postag = ar_set_procletic_flags(&new_postag);

    let stems = synth.lookup(lemma, &new_postag);
    let mut wordlist: Vec<String> = Vec::new();
    for stem0 in &stems {
        let stem = ar_correct_stem(stem0, &new_postag);
        let new_word = if ar_has_pronoun(&new_postag) && flag == 'H' {
            if stem.ends_with('ي') {
                if suffix == "ي" {
                    format!("{}{}", procletic, stem)
                } else {
                    String::new()
                }
            } else if stem.ends_with('ه') {
                format!(
                    "{}{}{}",
                    procletic,
                    stem.strip_suffix('ه').unwrap_or(&stem),
                    suffix
                )
            } else {
                format!("{}{}{}", procletic, stem, suffix)
            }
        } else {
            format!("{}{}", procletic, stem)
        };
        if !new_word.is_empty() {
            wordlist.push(new_word);
        }
    }
    if wordlist.is_empty() {
        wordlist.push(format!("({})", word));
    }
    wordlist
}

/// `ArabicSynthesizer.inflectLemmaLike`
fn ar_inflect_lemma_like(
    ctx: &FilterCtx,
    synth: &Synthesizer,
    target_lemma: &str,
    source_word: &str,
    source_postag: &str,
) -> Vec<String> {
    let readings = ar_tag_word(ctx, target_lemma);
    let mut has_lemma = false;
    for (lemma, _) in &readings {
        if lemma == target_lemma {
            has_lemma = true;
            break;
        }
    }
    if !has_lemma {
        return vec![format!("[{}]", target_lemma)];
    }
    let prefix = ar_get_proclitic(source_word, source_postag);
    let suffix = ar_get_enclitic(source_word, source_postag);
    let mut wordlist: Vec<String> = Vec::new();
    for (lemma, postag) in &readings {
        if lemma != target_lemma {
            continue;
        }
        let merged = ar_merge_pos_tag(source_postag, postag);
        let token_word = format!("{}{}", prefix, target_lemma);
        wordlist.extend(ar_set_enclitic_multiple(
            synth,
            &token_word,
            target_lemma,
            &merged,
            &suffix,
        ));
    }
    wordlist.sort();
    wordlist.dedup();
    wordlist
}

/// Tag a single word with the analyzer dictionary (approximating
/// `ArabicTagger.tag(word)`; also retries without diacritics).
fn ar_tag_word(ctx: &FilterCtx, word: &str) -> Vec<(String, String)> {
    let mut out: Vec<(String, String)> = ctx
        .tagger
        .get_tags_with_options(word, Some(false), Some(false))
        .map(|d| (d.lemma().as_str().to_string(), d.pos().as_str().to_string()))
        .collect();
    if out.is_empty() {
        let stripped: String = word
            .chars()
            .filter(|c| !matches!(*c, '\u{064B}'..='\u{0652}' | '\u{0670}'))
            .collect();
        if stripped != word {
            out = ctx
                .tagger
                .get_tags_with_options(&stripped, Some(false), Some(false))
                .map(|d| (d.lemma().as_str().to_string(), d.pos().as_str().to_string()))
                .collect();
        }
    }
    out
}

/// lemmas of a token's readings filtered by word class (`ArabicTagger.getLemmas`)
fn ar_get_lemmas(token: &Token, kind: &str) -> Vec<String> {
    let mut lemma_list: Vec<String> = Vec::new();
    for d in token.word().tags().iter() {
        let pos = d.pos().as_str();
        let matches_kind = match kind {
            "verb" => ar_is_verb(pos),
            "adj" => ar_is_adj(pos),
            "masdar" => ar_is_masdar(pos),
            _ => false,
        };
        if matches_kind && !lemma_list.contains(&d.lemma().as_str().to_string()) {
            lemma_list.push(d.lemma().as_str().to_string());
        }
    }
    lemma_list
}

/// parse a SimpleReplaceDataLoader word file: `key=value1|value2` lines
fn ar_parse_wordlist(data: &str) -> Vec<(String, Vec<String>)> {
    data.lines()
        .filter(|l| !l.trim_start().starts_with('#') && !l.trim().is_empty())
        .filter_map(|l| {
            let (k, v) = l.split_once('=')?;
            let values: Vec<String> = v.split('|').map(|s| s.to_string()).collect();
            Some((k.trim().to_string(), values))
        })
        .collect()
}

fn ar_wordlist_get<'a>(list: &'a [(String, Vec<String>)], key: &str) -> Option<&'a Vec<String>> {
    list.iter().find(|(k, _)| k == key).map(|(_, v)| v)
}

fn remove_tashkeel(word: &str) -> String {
    word.chars()
        .filter(|c| !matches!(*c, '\u{064B}'..='\u{0652}' | '\u{0670}'))
        .collect()
}

const AR_TEH_MARBUTA: char = 'ة';
const AR_FATHATAN: char = 'ً';
const AR_ALEF: char = 'ا';

fn ar_inflect_mafoul_mutlaq(word: &str) -> String {
    let mut newword = word.to_string();
    if word.ends_with(AR_TEH_MARBUTA) {
        newword.push(AR_FATHATAN);
    } else {
        newword.push(AR_FATHATAN);
        newword.push(AR_ALEF);
    }
    newword
}

fn ar_inflect_adjective_tanwin_nasb(word: &str, feminin: bool) -> String {
    let mut newword = word.to_string();
    if feminin {
        if word.ends_with(AR_TEH_MARBUTA) {
            newword.push(AR_FATHATAN);
        } else {
            newword.push(AR_TEH_MARBUTA);
            newword.push(AR_FATHATAN);
        }
    } else if word.ends_with(AR_TEH_MARBUTA) {
        newword = word.replace(AR_TEH_MARBUTA, "");
    } else {
        newword.push(AR_FATHATAN);
        newword.push(AR_ALEF);
    }
    newword
}

fn ar_masdar_to_verb(ctx: &mut FilterCtx, _replacements: Vec<String>) -> Option<JavaOutcome> {
    let tokens = ctx.matched_token_refs();
    if tokens.len() < 2 {
        return None;
    }
    let synth = ctx.synth?;
    let aux_verb_lemmas_all = ar_get_lemmas(tokens[0], "verb");
    // only authorized auxiliary lemmas are used
    let aux_verb_lemmas: Vec<&String> = aux_verb_lemmas_all
        .iter()
        .filter(|l| l.as_str() == "قَامَ")
        .collect();
    let masdar_lemmas = ar_get_lemmas(tokens[1], "masdar");

    static MASDAR2VERB: once_cell::sync::Lazy<Vec<(String, Vec<String>)>> =
        once_cell::sync::Lazy::new(|| {
            ar_parse_wordlist(include_str!("../../configs/ar/arabic_masdar_verb.txt"))
        });

    let mut verb_list: Vec<String> = Vec::new();
    for d in tokens[0].word().tags().iter() {
        let lemma = d.lemma().as_str();
        if !aux_verb_lemmas.iter().any(|l| l.as_str() == lemma) {
            continue;
        }
        for masdar_lemma in &masdar_lemmas {
            let verb_lemmas = ar_wordlist_get(&MASDAR2VERB, masdar_lemma)
                .or_else(|| ar_wordlist_get(&MASDAR2VERB, &remove_tashkeel(masdar_lemma)));
            if let Some(verb_lemmas) = verb_lemmas {
                for vrb in verb_lemmas {
                    verb_list.extend(ar_inflect_lemma_like(
                        ctx,
                        synth,
                        vrb,
                        tokens[0].word().as_str(),
                        d.pos().as_str(),
                    ));
                }
            }
        }
    }
    verb_list.sort();
    verb_list.dedup();
    Some(JavaOutcome {
        span: ctx.span.clone(),
        replacements: verb_list,
        message: None,
    })
}

fn ar_verb_to_mafoul_mutlaq(ctx: &mut FilterCtx, _replacements: Vec<String>) -> Option<JavaOutcome> {
    let tokens = ctx.matched_token_refs();
    if tokens.is_empty() {
        return None;
    }
    let verb = ctx.arg("verb")?;
    let adj = ctx.arg("adj")?;
    let verb_lemmas = ar_get_lemmas(tokens[0], "verb");

    static VERB2MASDAR: once_cell::sync::Lazy<Vec<(String, Vec<String>)>> =
        once_cell::sync::Lazy::new(|| {
            ar_parse_wordlist(include_str!("../../configs/ar/arabic_verb_masdar.txt"))
        });

    let inflected_adj_masc = ar_inflect_adjective_tanwin_nasb(&adj, false);
    let inflected_adj_fem = ar_inflect_adjective_tanwin_nasb(&adj, true);

    let mut inflected_masdar_list: Vec<String> = Vec::new();
    let mut inflected_adj_list: Vec<String> = Vec::new();
    for lemma in &verb_lemmas {
        let msdr_list = ar_wordlist_get(&VERB2MASDAR, lemma)
            .or_else(|| ar_wordlist_get(&VERB2MASDAR, &remove_tashkeel(lemma)));
        if let Some(msdr_list) = msdr_list {
            for msdr in msdr_list {
                let inflected_masdar = ar_inflect_mafoul_mutlaq(msdr);
                inflected_masdar_list.push(inflected_masdar);
                let inflected_adj = if msdr.ends_with(AR_TEH_MARBUTA) {
                    inflected_adj_fem.clone()
                } else {
                    inflected_adj_masc.clone()
                };
                inflected_adj_list.push(inflected_adj);
            }
        }
    }
    let mut suggestions: Vec<String> = Vec::new();
    for (i, msdr) in inflected_masdar_list.iter().enumerate() {
        let sug_phrase = format!("{} {} {}", verb, msdr, inflected_adj_list[i]);
        if !suggestions.contains(&sug_phrase) {
            suggestions.push(sug_phrase);
        }
    }
    Some(JavaOutcome {
        span: ctx.span.clone(),
        replacements: suggestions,
        message: None,
    })
}

fn ar_adjective_to_exclamation(
    ctx: &mut FilterCtx,
    _replacements: Vec<String>,
) -> Option<JavaOutcome> {
    let _adj = ctx.arg("adj")?;
    let noun = ctx.arg("noun")?;
    let adj_pos: usize = ctx.arg("adj_pos")?.parse().ok()?;
    let tokens = ctx.matched_token_refs();
    let adj_token_index = adj_pos.saturating_sub(1);
    if adj_token_index >= tokens.len() {
        return None;
    }
    let adj_lemmas = ar_get_lemmas(tokens[adj_token_index], "adj");

    static ADJ2COMP: once_cell::sync::Lazy<Vec<(String, Vec<String>)>> =
        once_cell::sync::Lazy::new(|| {
            ar_parse_wordlist(include_str!("../../configs/ar/arabic_adjective_exclamation.txt"))
        });

    let mut comp_list: Vec<String> = Vec::new();
    for adj_lemma in &adj_lemmas {
        if let Some(comparatives) =
            ar_wordlist_get(&ADJ2COMP, adj_lemma).or_else(|| ar_wordlist_get(&ADJ2COMP, &remove_tashkeel(adj_lemma)))
        {
            comp_list.extend(comparatives.iter().cloned());
        }
    }
    comp_list.sort();
    comp_list.dedup();

    let mut suggestions: Vec<String> = Vec::new();
    for comp in &comp_list {
        let mut suggestion = comp.clone();
        if noun.is_empty() {
            // nothing to append
        } else if ["هو", "هي", "هم", "هما", "أنا"].contains(&noun.as_str()) {
            let attached = match noun.as_str() {
                "أنا" => "ني",
                "نحن" => "نا",
                "هو" => "ه",
                "هي" => "ها",
                "هم" => "هم",
                "هن" => "هن",
                "أنتما" => "كما",
                "أنتم" => "كم",
                "أنتن" => "كن",
                _ => "",
            };
            suggestion.push_str(attached);
        } else {
            if !comp.ends_with(" ب") {
                suggestion.push(' ');
            }
            suggestion.push_str(&noun);
        }
        suggestions.push(suggestion);
    }
    Some(JavaOutcome {
        span: ctx.span.clone(),
        replacements: suggestions,
        message: None,
    })
}

// ===== English AdverbFilter =====

fn adverb2adj(adverb: &str) -> Option<&'static str> {
    Some(match adverb {
        "well" => "good",
        "fast" | "hard" | "late" | "early" | "daily" | "straight" => adverb_per(adverb),
        "simply" => "simple",
        "cheaply" => "cheap",
        "quickly" => "quick",
        "slowly" => "slow",
        "easily" => "easy",
        "angrily" => "angry",
        "happily" => "happy",
        "luckily" => "lucky",
        "terribly" => "terrible",
        "tragically" => "tragic",
        "economically" => "economic",
        "greatly" => "great",
        "highly" => "high",
        "generally" => "general",
        "differently" => "different",
        "rightly" => "right",
        "largely" => "large",
        "really" => "real",
        "philosophically" => "philosophical",
        "directly" => "direct",
        "clearly" => "clear",
        "merely" => "mere",
        "exactly" => "exact",
        "recently" => "recent",
        "rapidly" => "rapid",
        "suddenly" => "sudden",
        "extremely" => "extreme",
        "properly" => "proper",
        "politically" => "political",
        "probably" => "probable",
        "self-consciously" => "self-conscious",
        "successfully" => "successful",
        "unusually" => "unusual",
        "obviously" => "obvious",
        "currently" => "current",
        "residentially" => "residential",
        "fully" => "full",
        "accidentally" => "accidental",
        "medicinally" => "medicinal",
        "automatically" => "automatic",
        "completely" => "complete",
        "chronologically" => "chronological",
        "accurately" => "accurate",
        "necessarily" => "necessary",
        "temporarily" => "temporary",
        "significantly" => "significant",
        "hastily" => "hasty",
        "immediately" => "immediate",
        "rarely" => "rare",
        "totally" => "total",
        "literally" => "literal",
        "gently" => "gentle",
        "finally" => "final",
        "increasingly" => "increasing",
        "decreasingly" => "decreasing",
        "considerably" => "considerable",
        "effectively" => "effective",
        "briefly" => "brief",
        "exceedingly" => "exceeding",
        "physically" => "physical",
        "enthusiastically" => "enthusiastic",
        "incredibly" => "incredible",
        "permanently" => "permanent",
        "entirely" => "entire",
        "surely" => "sure",
        "positively" => "positive",
        "negatively" => "negative",
        "devastatingly" => "devastating",
        "relatively" => "relative",
        "absolutely" => "absolute",
        "socially" => "social",
        "industriously" => "industrious",
        "solely" => "sole",
        "asynchronously" => "asynchronous",
        "fortunately" => "fortunate",
        "unfortunately" => "unfortunate",
        "ideally" => "ideal",
        "privately" => "private",
        "unreasonably" => "unreasonable",
        "personally" => "personal",
        "basically" => "basic",
        "definitely" => "definite",
        "potentially" => "potential",
        "manually" => "manual",
        "continuously" => "continuous",
        "sadly" => "sad",
        "eventually" => "eventual",
        "possibly" => "possible",
        "visually" => "visual",
        "predominantly" | "predominately" => "predominant",
        "quietly" => "quiet",
        "slightly" => "slight",
        "cleverly" => "clever",
        "roughly" => "rough",
        "environmentally" => "environmental",
        "geographically" => "geographical",
        "usually" => "usual",
        "normally" => "normal",
        "deliciously" => "delicious",
        "steadily" => "steady",
        "actively" => "active",
        "schematically" => "schematic",
        "mindfully" => "mindful",
        "statistically" => "statistical",
        "culturally" => "cultural",
        "vicariously" => "vicarious",
        "vividly" => "vivid",
        "partially" | "partly" => "partial",
        "seriously" => "serious",
        "non-verbally" => "non-verbal",
        "nonverbally" => "nonverbal",
        "verbally" => "verbal",
        "shortly" => "short",
        "mildly" => "mild",
        "secretly" => "secret",
        "especially" => "especial",
        "specially" => "special",
        "previously" => "previous",
        "whitely" => "white",
        "traditionally" => "traditional",
        "individually" => "individual",
        "carefully" => "careful",
        "essentially" => "essential",
        "originally" => "original",
        "alarmingly" => "alarming",
        "newly" => "new",
        "wrongfully" => "wrongful",
        "structurally" => "structural",
        "globally" => "global",
        "pacifically" => "pacific",
        "seemingly" => "seeming",
        "seamlessly" => "seamless",
        "sustainably" => "sustainable",
        "momentarily" => "momentary",
        "coldly" => "cold",
        "densely" => "dense",
        "grimly" => "grim",
        "calmly" => "calm",
        "racially" => "racial",
        "widely" => "wide",
        "heavily" => "heavy",
        "authentically" => "authentic",
        "honestly" => "honest",
        "desperately" => "desperate",
        "immensely" => "immense",
        "apparently" => "apparent",
        "straightforwardly" => "straightforward",
        "anatomically" => "anatomical",
        "uniquely" => "unique",
        "systemically" => "systemic",
        "jokily" => "jokey",
        "critically" => "critical",
        "equally" => "equal",
        "strongly" => "strong",
        "purposely" => "intentional",
        "thoroughly" => "thorough",
        "outwardly" | "outwards" => "outward",
        "horizontally" => "horizontal",
        "vertically" => "vertical",
        "technically" => "technical",
        "swiftly" => "swift",
        "accessibly" => "accessible",
        "occasionally" => "occasional",
        "specifically" => "specific",
        "subtly" => "subtle",
        "actually" => "actual",
        "particularly" => "particular",
        "gloomily" => "gloomy",
        "nicely" => "nice",
        "progressively" => "progressive",
        "genuinely" => "genuine",
        "characteristically" | "uncharacteristically" => {
            if adverb == "characteristically" {
                "characteristic"
            } else {
                "uncharacteristic"
            }
        }
        "deeply" => "deep",
        "spiritually" => "spiritual",
        "purely" => "pure",
        "satisfyingly" => "satisfying",
        "indolently" => "indolent",
        "obliquely" => "oblique",
        "preferably" => "preferable",
        "oddly" => "odd",
        "professionally" => "professional",
        "indispensably" => "indispensable",
        "dispensably" => "dispensable",
        "consistently" => "consistent",
        "truly" => "true",
        "commonly" => "common",
        "safely" => "safe",
        "evolutionarily" => "evolutionary",
        "internally" => "internal",
        "magically" => "magical",
        "annually" => "annual",
        "brightly" => "bright",
        "officially" => "official",
        "inofficially" => "inofficial",
        "perfectly" => "perfect",
        "overly" => "over",
        "tropically" => "tropical",
        "brilliantly" => "brilliant",
        "exclusively" => "exclusive",
        "commercially" => "commercial",
        "mischievously" => "mischievous",
        "weirdly" => "weird",
        "routinely" => "routine",
        "gruffly" => "gruff",
        "naturally" => "natural",
        "lightly" => "light",
        "haphazardly" => "haphazard",
        "lovingly" => "loving",
        "sagely" => "sage",
        "systematically" => "systematical",
        "academically" => "academical",
        "jokingly" => "joking",
        "primarily" => "primary",
        "secondarily" => "secondary",
        "peacefully" => "peaceful",
        "thankfully" => "thankful",
        "reliably" => "reliable",
        "unreliably" => "unreliable",
        "infinitesimally" => "infinitesimal",
        "hugely" => "huge",
        "strictly" => "strict",
        "morally" => "moral",
        "involuntarily" => "involuntary",
        "voluntarily" => "voluntary",
        "vanishingly" => "vanishing",
        "typically" => "typical",
        "playfully" => "playful",
        "wonderfully" => "wonderful",
        "roguishly" => "roguish",
        "emotionally" => "emotional",
        "efficiently" => "efficient",
        "unkindly" => "unkind",
        "mentally" => "mental",
        "credibly" => "credible",
        "seductively" => "seductive",
        "rashly" => "rash",
        "periodically" => "periodical",
        "comparatively" => "comparative",
        "confidentially" => "confidential",
        "dominantly" => "dominant",
        "forcibly" => "forcible",
        "formerly" => "former",
        "financially" => "financial",
        "urgently" => "urgent",
        "inherently" => "inherent",
        "historically" => "historical",
        "tightly" => "tight",
        "greedily" => "greedy",
        "fluently" => "fluent",
        "ordinarily" => "ordinary",
        "inevitably" => "inevitable",
        "liquidly" => "liquid",
        "supremely" => "supreme",
        "initially" => "initial",
        "unjustly" => "unjust",
        "justly" => "just",
        "plausibly" => "plausible",
        "amiably" => "amiable",
        "massively" => "massive",
        "lowly" => "low",
        "notoriously" => "notorious",
        "meaningfully" => "meaningful",
        "approximately" => "approximate",
        "extraordinarily" => "extraordinary",
        "warmly" => "warm",
        "nearly" => "near",
        "strategically" => "strategical",
        "endlessly" => "endless",
        "virtually" => "virtual",
        "regularly" => "regular",
        "deliberately" => "deliberate",
        "reasonably" => "reasonable",
        "similarly" => "similar",
        "flexibly" => "flexible",
        "softly" => "soft",
        "responsibly" => "responsible",
        "irresponsibly" => "irresponsible",
        "sweetly" => "sweet",
        "comfortably" => "comfortable",
        "uncomfortably" => "uncomfortable",
        "intricately" => "intricate",
        "unnecessarily" => "unnecessary",
        "obstinately" => "obstinate",
        "reportedly" => "reported",
        "loosely" => "loose",
        "profusely" => "profuse",
        "mortally" => "mortal",
        "dynamically" => "dynamical",
        "illegally" => "illegal",
        "legally" => "legal",
        "undoubtedly" => "undoubted",
        "humanly" => "human",
        "likewise" => "similar",
        "intrinsically" => "intrinsic",
        "substantially" => "substantial",
        "suspiciously" => "suspicious",
        "generationally" => "generational",
        "loudly" => "loud",
        "moderately" => "moderate",
        "gravely" => "grave",
        "temporally" => "temporal",
        "digitally" => "digital",
        "finely" => "fine",
        "respectfully" => "respectful",
        "questioningly" => "questioning",
        "diagonally" => "diagonal",
        "additionally" => "additional",
        "sexually" => "sexual",
        "remarkably" => "remarkable",
        "acutely" => "acute",
        "linearly" => "linear",
        "perfunctorily" => "perfunctory",
        "unbelievably" => "unbelievable",
        "merrily" => "merry",
        "beneath" => "below",
        "lest" => "least",
        "either" => "other",
        "nasally" => "nasal",
        "concretely" => "concrete",
        "intuitively" => "intuitive",
        "please" => "pleasing",
        "intermediately" => "intermediate",
        "powerfully" => "powerful",
        "fairly" => "fair",
        "wholly" => "whole",
        "keenly" => "keen",
        "unconsciously" => "unconscious",
        "consciously" => "conscious",
        "humanely" => "humane",
        "honorably" => "honorable",
        "rudely" => "rude",
        "incorrectly" => "incorrect",
        "correctly" => "correct",
        "mistakenly" => "mistaken",
        "wrongly" => "wrong",
        "morosely" => "morose",
        "worryingly" => "worrying",
        "drastically" => "drastical",
        "willingly" => "willing",
        "additively" => "additive",
        "drolly" => "droll",
        "statically" => "statical",
        "hopefully" => "hopeful",
        "untruthfully" => "untruthful",
        "truthfully" => "truthful",
        "attractively" => "attractive",
        "supposedly" => "supposed",
        "overwhelmingly" => "overwhelming",
        "imperfectly" => "imperfect",
        "deftly" => "deft",
        "wildly" => "wild",
        "sheepishly" => "sheepish",
        "hotly" => "hot",
        "genetically" => "genetic",
        "inexplicably" => "inexplicable",
        "explicably" => "explicable",
        "domestically" => "domestical",
        "invisibly" => "invisible",
        "visibly" => "visible",
        "noteworthily" => "noteworthy",
        "unexpectably" => "unexpectable",
        "expectably" => "expectable",
        "foreseeably" => "foreseeable",
        "unforeseeably" => "unforeseeable",
        "distinctly" => "distinct",
        "unequivocally" => "unequivocal",
        "signally" => "signal",
        "medically" => "medical",
        "certainly" => "certain",
        "beautifully" => "beautiful",
        "firmly" => "firm",
        "electrically" => "electrical",
        "gradually" => "gradual",
        "grossly" => "gross",
        "memorably" => "memorable",
        "unmemorably" => "unmemorable",
        "shelly" => "shell",
        "strangely" => "strange",
        "unhealthily" => "unhealthy",
        "healthily" => "healthy",
        "harshly" => "harsh",
        "proudly" => "proud",
        "lately" => "late",
        "remotely" => "remote",
        "longly" => "long",
        "politely" => "polite",
        "ethically" => "ethical",
        "noticeably" => "noticeable",
        "unnoticeably" => "unnoticeable",
        "consequently" => "consequent",
        "snugly" => "snug",
        "mainly" => "main",
        "popularly" => "popular",
        "improperly" => "improper",
        "deliverly" => "delivery",
        "rushingly" => "rushing",
        "gravitationally" => "gravitational",
        "cruelly" => "cruel",
        "optimally" => "optimal",
        "fictionally" => "fictional",
        "manageably" => "manageable",
        "unmanageably" => "unmanageable",
        "fashionably" => "fashionable",
        "secondly" => "second",
        "thirdly" => "third",
        "curtly" => "curt",
        "secretively" => "secretive",
        "surprisingly" => "surprising",
        "sociologically" => "sociological",
        "severely" => "severe",
        "ruffianly" => "ruffian",
        "bigly" => "big",
        "frequently" => "frequent",
        "irrationally" => "irrational",
        "rationally" => "rational",
        "riotously" => "riotous",
        "excruciatingly" => "excruciating",
        "intensively" => "intensive",
        "separately" => "separate",
        "favorably" => "favorable",
        "favourably" => "favourable",
        "unfavorably" => "unfavorable",
        "unfavourably" => "unfavourable",
        "fittingly" => "fitting",
        "orally" => "oral",
        "jointly" => "joint",
        "methodically" => "methodical",
        "ecologically" => "ecological",
        "irrepressibly" => "irrepressible",
        "repressibly" => "repressible",
        "heartily" => "hearty",
        "smoothly" => "smooth",
        "dreamily" => "dreamy",
        "indirectly" => "indirect",
        "fascinatingly" => "fascinating",
        "scientifically" => "scientific",
        "unhappily" => "unhappy",
        "publicly" => "public",
        "healthfully" => "healthful",
        "genially" => "genial",
        "ineludibly" => "ineludible",
        "tenderly" => "tender",
        "arguably" => "arguable",
        "comparably" => "comparable",
        "procedurally" => "procedural",
        "interchangeably" => "interchangeable",
        "conceivably" => "conceivable",
        "resignedly" => "resigned",
        "vehemently" => "vehement",
        "horribly" => "horrible",
        "teasingly" => "teasing",
        "figuratively" => "figurative",
        "excitingly" => "exciting",
        "haltingly" => "halting",
        "phonetically" => "phonetic",
        "proverbially" => "proverbial",
        "informally" => "informal",
        "cozily" => "cozy",
        "cosily" => "cosy",
        "constantly" => "constant",
        "rightfully" => "rightful",
        "reluctantly" => "reluctant",
        "externally" => "external",
        "intellectually" => "intellectual",
        "dramatically" => "dramatic",
        "freshly" => "fresh",
        "casually" => "casual",
        "unevenly" => "uneven",
        "enormously" => "enormous",
        "callously" => "callous",
        "imperiously" => "imperious",
        "messily" => "messy",
        "alternatively" => "alternative",
        "gladly" => "glad",
        "adversely" => "adverse",
        "petulantly" => "petulant",
        "shakily" => "shaky",
        "menacingly" => "menacing",
        "consensually" => "consensual",
        "bitterly" => "bitter",
        "terminally" => "terminal",
        "faintly" => "faint",
        "brusquely" => "brusque",
        "humbly" => "humble",
        "promptly" => "prompt",
        "identically" => "identical",
        "militarily" => "military",
        "neatly" => "neat",
        "insanely" => "insane",
        "analytically" => "analytical",
        "firstly" => "first",
        "twice" => "second",
        _ => return None,
    })
}

fn adverb_per(adverb: &str) -> &'static str {
    match adverb {
        "fast" => "fast",
        "hard" => "hard",
        "late" => "late",
        "early" => "early",
        "daily" => "daily",
        "straight" => "straight",
        _ => "",
    }
}

fn adverb_filter_en(ctx: &mut FilterCtx, replacements: Vec<String>) -> Option<JavaOutcome> {
    let adverb = ctx.arg("adverb")?;
    let noun = ctx.arg("noun")?;
    if let Some(adjective) = adverb2adj(&adverb) {
        if adjective != adverb {
            return keep(ctx, vec![format!("{} {}", adjective, noun)]);
        }
    }
    // no mapping: the original suggestion is kept untouched
    keep(ctx, replacements)
}
