//! Ports of LanguageTool's Java-only built-in rules that have no XML
//! representation. Currently: `UppercaseSentenceStartRule` (LT 6.5) and its
//! Ukrainian subclass `UkrainianUppercaseSentenceStartRule`.

use crate::types::{Sentence, Span, Suggestion};
use crate::utils::regex::Regex;
use once_cell::sync::Lazy;

/// Cross-sentence context of the text-level built-ins, mirroring the state
/// `UppercaseSentenceStartRule` carries while iterating over the sentences of
/// a text (`lastParagraphString`, `isPrevSentenceNumberedList`).
pub(crate) struct TextRuleContext {
    /// This sentence is the only sentence of the checked text
    /// (LT skips single-word one-sentence texts).
    pub is_only_sentence: bool,
    /// The last significant token of the previous sentence, if any.
    pub prev_last_token: Option<String>,
    /// The previous sentence looked like a numbered list item ("1. foo").
    pub prev_numbered_list: bool,
}

impl Default for TextRuleContext {
    fn default() -> Self {
        TextRuleContext {
            is_only_sentence: false,
            prev_last_token: None,
            prev_numbered_list: false,
        }
    }
}

// Java's Pattern.matches() is a full match; the anchors keep onig equivalent.
static NUMERALS_EN: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"\A(?:[a-z]|m{0,4}(?:c[md]|d?c{0,3})(?:x[cl]|l?x{0,3})(?:i[xv]|v?i{0,3}))\z".to_string())
});
static ONLY_LOWERCASE_START: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"\A[a-z][A-Z].*\z".to_string()));
static CAMEL_CASE: Lazy<Regex> = Lazy::new(|| Regex::new(r"\A[a-z]+[A-Z][A-Za-z]+\z".to_string()));
static NO_PROTOCOL_URL: Lazy<Regex> = Lazy::new(|| {
    Regex::new(
        r"\A(?:[a-zA-Z0-9][a-zA-Z0-9-]+\.)?[a-zA-Z0-9][a-zA-Z0-9-]+\.[a-zA-Z0-9][a-zA-Z0-9-]+/.*\z".to_string(),
    )
});
static E_MAIL: Lazy<Regex> = Lazy::new(|| {
    Regex::new(
        r#"(?<!:)\A@?\b[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))\b\z"#.to_string(),
    )
});
static DIGIT_DOT: Lazy<Regex> = Lazy::new(|| Regex::new(r"\A\d+\. .*\z".to_string()));
static LINEBREAK_DIGIT_DOT: Lazy<Regex> =
    Lazy::new(|| Regex::new(r"\A.*\n\d+\. \z".to_string()));

static EXCEPTIONS: Lazy<[&'static str; 7]> =
    Lazy::new(|| ["n", "w", "x86", "ⓒ", "ø", "cc", "pH"]);

/// Whether the text starts with "1. " (numbered list detection for the next sentence).
pub(crate) fn is_numbered_list_item(sentence_text: &str) -> bool {
    DIGIT_DOT.is_match(sentence_text) || LINEBREAK_DIGIT_DOT.is_match(sentence_text)
}

/// The last significant (non trailing whitespace/quote) token of a sentence,
/// as `UppercaseSentenceStartRule` tracks between sentences.
pub(crate) fn last_significant_token(sentence: &Sentence) -> Option<String> {
    let tokens: Vec<_> = sentence.iter().collect();
    if tokens.is_empty() {
        return None;
    }
    let last = tokens[tokens.len() - 1].word().as_str().to_string();
    if is_whitespace_or_quote(&last) && tokens.len() >= 2 {
        Some(tokens[tokens.len() - 2].word().as_str().to_string())
    } else {
        Some(last)
    }
}

fn is_whitespace_or_quote(token: &str) -> bool {
    // Java WHITESPACE_OR_QUOTE is a single-char class used with matches()
    let mut chars = token.chars();
    match chars.next() {
        Some(c) => chars.next().is_none() && is_ws_or_quote_char(c),
        None => false,
    }
}

fn is_ws_or_quote_char(c: char) -> bool {
    matches!(
        c,
        ' ' | '"' | '\'' | '„' | '«' | '»' | '‘' | '’' | '“' | '”' | '\n'
    )
}

fn is_quote_start(token: &str, lang: Option<&str>) -> bool {
    let base = [
        "\"", "'", "„", "»", "«", "“", "‘", "¡", "¿",
    ];
    if lang == Some("pt") {
        // pt-BR uses dashes to introduce dialogue
        matches!(token, "\"" | "'" | "„" | "»" | "«" | "“" | "‘" | "¡" | "¿" | "-" | "–" | "—")
    } else {
        base.contains(&token)
    }
}

fn is_dutch_initial(token: &str) -> bool {
    matches!(token, "k" | "m" | "n" | "r" | "s" | "t")
}

fn is_url(token: &str) -> bool {
    for protocol in ["http", "https", "ftp"] {
        if token.starts_with(&format!("{protocol}://")) {
            return true;
        }
    }
    token.starts_with("www.") || NO_PROTOCOL_URL.is_match(token)
}

fn is_email(token: &str) -> bool {
    E_MAIL.is_match(token)
}

fn contains_digit(token: &str) -> bool {
    token.chars().any(|c| c.is_ascii_digit())
}

fn is_ukrainian_list_letter(token: &str) -> bool {
    let mut chars = token.chars();
    match chars.next() {
        Some(c) => {
            chars.next().is_none() && matches!(c, 'а'..='я' | 'і' | 'ї' | 'є' | 'ґ')
        }
        None => false,
    }
}

/// Port of `StringTools.uppercaseFirstChar` (the one-argument version; LT's
/// rule does not use the Dutch IJ variant).
fn uppercase_first_char(s: &str) -> String {
    if s.is_empty() {
        return s.to_string();
    }
    let chars: Vec<char> = s.chars().collect();
    if chars.len() == 1 {
        // Java: str.toUpperCase(Locale.ENGLISH) - a full string uppercasing
        return chars[0].to_uppercase().collect();
    }
    let len = chars.len() - 1;
    let mut pos = 0;
    while len > pos && !chars[pos].is_alphanumeric() {
        pos += 1;
    }
    let first = chars[pos];
    // Java's Character.toUpperCase is a single-character mapping: characters
    // whose uppercase form is multi-char (e.g. ß -> SS) are returned unchanged
    let upper = {
        let mut it = first.to_uppercase();
        let len = it.len();
        match (len, it.next()) {
            (1, Some(c)) => c,
            _ => first,
        }
    };
    let mut out: String = chars[..pos].iter().collect();
    out.push(upper);
    out.extend(chars[pos + 1..].iter());
    out
}

fn is_immunized(token: &crate::types::Token) -> bool {
    token.chunks().iter().any(|c| c == "_immunized")
}

fn is_spelling_ignored_token(token: &crate::types::Token) -> bool {
    token
        .chunks()
        .iter()
        .any(|c| c == "_ignore_spelling" || c == "_immunized")
}

/// Port of `UppercaseSentenceStartRule.match` for a single sentence with the
/// text-level state of the previous sentence.
pub(crate) fn uppercase_sentence_start(
    sentence: &Sentence,
    lang: Option<&str>,
    ctx: &TextRuleContext,
) -> Vec<Suggestion> {
    let tokens: Vec<_> = sentence.iter().collect();
    // LT bails out if there is no real token next to SENT_START
    if tokens.is_empty() {
        return Vec::new();
    }
    // Not useful to complain about a single-word single-sentence text
    if ctx.is_only_sentence && tokens.len() == 1 {
        return Vec::new();
    }

    let first = tokens[0].word().as_str();
    let second = tokens.get(1).map(|t| t.word().as_str());

    // real-token index LT checks (its matchTokenPos minus the SENT_START token)
    let mut idx = 0usize;
    // ignore leading quote characters (tokens.length >= 3 there => 2 real tokens)
    if tokens.len() >= 2 && is_quote_start(first, lang) {
        idx = 1;
    }
    // 't kofschip-style Dutch contractions: "'k heb..." -> check the third token
    if lang == Some("nl")
        && tokens.len() > 2
        && first == "'"
        && second.map_or(false, is_dutch_initial)
    {
        idx = 2;
    }

    let check_token = tokens[idx].word().as_str();

    // UkrainianUppercaseSentenceStartRule: lists like "а) б) в)"
    if lang == Some("uk") && idx == 0 && tokens.len() > 1 {
        if is_ukrainian_list_letter(check_token) && tokens[1].word().as_str() == ")" {
            return Vec::new();
        }
    }

    // the last significant token of this sentence (trailing whitespace or
    // quote ignored)
    let mut last_token = tokens[tokens.len() - 1].word().as_str().to_string();
    if is_whitespace_or_quote(&last_token) && tokens.len() >= 2 {
        last_token = tokens[tokens.len() - 2].word().as_str().to_string();
    }

    let mut prevent_error = false;
    if ctx.prev_last_token.as_deref() == Some(",") || ctx.prev_last_token.as_deref() == Some(";") {
        prevent_error = true;
    }
    if contains_digit(check_token) {
        prevent_error = true;
    }
    // Java's SENTENCE_END1 = "[.?!…]|": matches only the empty string or a
    // single end punctuation. If the previous sentence ended with a real
    // word, this sentence must end with ./?/!/… for the error to count.
    let prev_is_empty_or_punct = match ctx.prev_last_token.as_deref() {
        None => true,
        Some("") => true,
        Some(t) => t.chars().count() == 1 && matches!(t, "." | "?" | "!" | "…"),
    };
    if !prev_is_empty_or_punct && !matches!(last_token.as_str(), "." | "?" | "!" | "…") {
        prevent_error = true;
    }
    // allow lowercase enumerations like "a)" and "iv."
    if idx + 1 < tokens.len() {
        let next = tokens[idx + 1].word().as_str();
        if NUMERALS_EN.is_match(check_token) && (next == "." || next == ")") {
            prevent_error = true;
        }
    }
    if ctx.prev_numbered_list
        || is_url(check_token)
        || is_email(check_token)
        || is_immunized(tokens[0])
    {
        prevent_error = true;
    }

    if !check_token.is_empty() {
        let first_char = check_token.chars().next().unwrap();
        let capitalized = uppercase_first_char(check_token);
        if capitalized != check_token
            && !prevent_error
            && first_char.is_lowercase()
            && !ONLY_LOWERCASE_START.is_match(check_token)
            && !EXCEPTIONS.contains(&check_token)
            && !CAMEL_CASE.is_match(check_token)
        {
            let span = Span::from_positions(tokens[idx].span().start(), tokens[idx].span().end());
            return vec![Suggestion::new(
                "UPPERCASE_SENTENCE_START".to_string(),
                "This sentence does not start with an uppercase letter.".to_string(),
                span,
                vec![capitalized],
            )];
        }
    }

    Vec::new()
}

/// Port of LT's `KhmerSpaceBeforeRule` (AbstractSpaceBeforeRule): a missing
/// real space before the conjunctions ដើម្បី/និង/ពីព្រោះ. LT iterates its
/// token list from index 1 (index 0 is the artificial SENT_START, which
/// never equals " " or "("), so a sentence-initial conjunction fires too;
/// a preceding "(" suppresses the match.
pub(crate) fn khmer_space_before(sentence: &Sentence) -> Vec<Suggestion> {
    const CONJUNCTIONS: [&str; 3] = ["ដើម្បី", "និង", "ពីព្រោះ"];
    let tokens: Vec<_> = sentence.iter().collect();
    let mut out = Vec::new();
    for (idx, token) in tokens.iter().enumerate() {
        let word = token.word().as_str();
        if !CONJUNCTIONS.contains(&word) {
            continue;
        }
        // LT's previous token is " " exactly when real whitespace precedes;
        // our has_space_before covers any whitespace char. "(" suppresses.
        let prev_is_open_paren = idx > 0 && tokens[idx - 1].word().as_str() == "(";
        let missing_space = idx == 0 || (!token.has_space_before() && !prev_is_open_paren);
        if !missing_space {
            continue;
        }
        let span = Span::from_positions(token.span().start(), token.span().end());
        out.push(Suggestion::new(
            "KM_SPACE_BEFORE_CONJUNCTION".to_string(),
            "Missing space before conjunction.".to_string(),
            span,
            vec![format!(" {}", word)],
        ));
    }
    out
}
// ---------------------------------------------------------------------------
// MorfologikSpellerRule / SpellingCheckRule port (LT 6.5)
// ---------------------------------------------------------------------------

/// A word list embedded from `configs/speller/<lang>/`.
fn wordlist(text: &'static str) -> Vec<&'static str> {
    text.lines()
        .map(|l| l.split('#').next().unwrap_or("").trim())
        .filter(|l| !l.is_empty())
        .collect()
}

macro_rules! spelling_lists {
    ($lang:literal, $file:literal) => {
        static_ref! {
            Vec<&'static str> = wordlist(include_str!(concat!("./speller_data/", $lang, "/", $file)))
        }
    };
    ($lang:literal, $file:literal, $($rest:literal),+) => {
        static_ref! {
            Vec<&'static str> = wordlist(concat!(
                include_str!(concat!("./speller_data/", $lang, "/", $file)),
                "\n",
                $(include_str!(concat!("./speller_data/", $lang, "/", $rest)), "\n"),+
            ))
        }
    };
}

// include_str with a trailing "\n" appended so concatenated files don't bleed
// their last word together; `wordlist` strips it again via line splitting.
macro_rules! static_ref {
    ($ty:ty = $expr:expr) => {{
        static LIST: Lazy<$ty> = Lazy::new(|| $expr);
        &*LIST
    }};
}

struct SpellingRuleConfig {
    id: &'static str,
    latin_script: bool,
    /// wordsToBeIgnored: ignore.txt + spelling.txt + spelling_custom.txt
    ignore: &'static [&'static str],
    /// wordsToBeProhibited: prohibit.txt + prohibit_custom.txt
    prohibit: &'static [&'static str],
}

/// Rule id the WORD_REPEAT family emits for a language (LT renames it
/// per language). Keep in sync with `builtin_suggestions`.
pub(crate) fn repeat_rule_id(lang: Option<&str>) -> &'static str {
    match lang {
        Some("de") => "GERMAN_WORD_REPEAT_RULE",
        Some("es") => "SPANISH_WORD_REPEAT_RULE",
        Some("fr") => "FRENCH_WORD_REPEAT_RULE",
        Some("uk") => "UKRAINIAN_WORD_REPEAT_RULE",
        Some("ca") => "CATALAN_WORD_REPEAT_RULE",
        Some("pt") => "PORTUGUESE_WORD_REPEAT_RULE",
        Some("it") => "ITALIAN_WORD_REPEAT_RULE",
        Some("en") => "ENGLISH_WORD_REPEAT_RULE",
        Some("ar") => "ARABIC_WORD_REPEAT_RULE",
        Some("fa") => "PERSIAN_WORD_REPEAT_RULE",
        _ => "WORD_REPEAT_RULE",
    }
}

/// Languages whose LT 6.5 configuration registers NO WordRepeatRule
/// (verified empirically: noun-repeat probes never fire) — the port must
/// not emit a repeat suggestion there either.
const NO_REPEAT_LANGS: &[&str] = &["ast", "crh", "da", "ja", "km", "ta", "tl", "zh"];

/// LT `WordRepeatRule` and per-language subclasses: adjacent identical
/// words. English only flags case-different repetitions (its subclass
/// suppresses identical lowercase repeats like "word word"); German has
/// case-pair exceptions.
pub(crate) fn word_repeat(sentence: &Sentence, lang: Option<&str>) -> Vec<Suggestion> {
    let lang = match lang {
        Some(l) if !NO_REPEAT_LANGS.contains(&l) => l,
        _ => return Vec::new(),
    };
    let tokens: Vec<_> = sentence.iter().collect();
    if tokens.len() < 2 {
        return Vec::new();
    }
    let mut out = Vec::new();
    for pair in tokens.windows(2) {
        let (a, b) = (pair[0], pair[1]);
        let ta = a.word().as_str();
        let tb = b.word().as_str();
        if !b.has_space_before() || ta.chars().count() < 2 {
            continue;
        }
        if !ta.chars().all(char::is_alphabetic) || !ta.eq_ignore_ascii_case(tb) {
            continue;
        }
        if lang == "en" && ta == tb {
            // EnglishWordRepeatRule: only case-different repetitions
            continue;
        }
        // GermanWordRepeatRule.ignore: case-specific exceptions
        if lang == "de" && matches!((ta, tb), ("Sie", "sie") | ("sie", "Sie") | ("Waren", "waren") | ("waren", "Waren")) {
            continue;
        }
        let span = Span::from_positions(a.span().start(), b.span().end());
        out.push(Suggestion::new(
            repeat_rule_id(Some(lang)).to_string(),
            "Possible typo: you repeated a word".to_string(),
            span,
            vec![ta.to_string()],
        ));
    }
    out
}

/// Inventory of the rule ids the built-in (Java-only in LT) families can
/// emit for this language. Mirrors the arms of `Rules::builtin_suggestions`;
/// keep in sync when porting new builtin families.
pub(crate) fn builtin_ids(
    lang: Option<&str>,
    filter_data: &crate::rule::filter_data::FilterData,
) -> Vec<String> {
    let mut out = Vec::new();
    if filter_data.speller.is_some()
        && lang.map_or(false, |l| spelling_config(l).is_some())
    {
        out.push(spelling_config(lang.unwrap()).unwrap().id.to_string());
    }
    out.push("UPPERCASE_SENTENCE_START".to_string());
    if lang == Some("km") {
        out.push("KM_SPACE_BEFORE_CONJUNCTION".to_string());
    }
    out.push(repeat_rule_id(lang).to_string());
    for id in text_family_ids(lang) {
        out.push(id.to_string());
    }
    if let Some(tables) = filter_data.simple_replace.as_ref() {
        for t in tables {
            out.push(t.prefix.clone());
        }
    }
    out
}

/// Ids of the text-level families (`DOUBLE_PUNCTUATION`, `WHITESPACE_RULE`,
/// `COMMA_PARENTHESIS_WHITESPACE`, `UNPAIRED_BRACKETS`/`EN_UNPAIRED_QUOTES`)
/// that LT 6.5 runs for this language at the default level — must mirror
/// the gate tables of the four ported functions.
pub(crate) fn text_family_ids(lang: Option<&str>) -> Vec<&'static str> {
    let mut out = Vec::new();
    let lang = match lang {
        Some(l) => l,
        None => return out,
    };
    if let Some(cfg) = text_cfg(lang) {
        if !cfg.dbl.is_empty() {
            out.push(cfg.dbl);
        }
        if cfg.ws {
            out.push("WHITESPACE_RULE");
        }
        for id in [cfg.comma, cfg.comma_close_bracket, cfg.comma_open_bracket] {
            if !id.is_empty() && !out.contains(&id) {
                out.push(id);
            }
        }
        if let Some(u) = cfg.unp {
            if u.brackets && !out.contains(&u.bracket_id) {
                out.push(u.bracket_id);
            }
            if u.quotes && !out.contains(&u.quote_id) {
                out.push(u.quote_id);
            }
        }
    }
    if lang == "en" {
        out.push("EN_A_VS_AN");
    }
    if lang == "ar" {
        out.push("ARABIC_QM_WHITESPACE");
        out.push("ARABIC_SC_WHITESPACE");
        out.push("AR_DIACRITICS_REPLACE");
    }
    if lang == "de" {
        out.push("DE_VERBAGREEMENT");
        out.push("DE_SUBJECT_VERB_AGREEMENT");
        out.push("DE_CASE");
    }
    if lang == "ru" {
        out.push("RU_COMPOUNDS");
        out.push("RU_VERB_CONJUGATION");
    }
    if lang == "br" {
        out.push("BR_TOPO");
    }
    if lang == "fr" {
        out.push("FRENCH_WHITESPACE");
    }
    out
}

fn spelling_config(lang: &str) -> Option<SpellingRuleConfig> {
    Some(match lang {
        "es" => SpellingRuleConfig {
            id: "MORFOLOGIK_RULE_ES",
            latin_script: true,
            ignore: spelling_lists!("es", "ignore.txt", "spelling.txt", "replace_words.txt", "corpus_extra.txt"),
            prohibit: spelling_lists!("es", "prohibit.txt"),
        },
        "ca" => SpellingRuleConfig {
            id: "MORFOLOGIK_RULE_CA_ES",
            latin_script: true,
            ignore: spelling_lists!("ca", "ignore.txt", "replace_words.txt"),
            prohibit: spelling_lists!("ca", "prohibit.txt"),
        },
        "fr" => SpellingRuleConfig {
            id: "FR_SPELLING_RULE",
            latin_script: true,
            ignore: spelling_lists!("fr", "ignore.txt", "spelling.txt", "spelling_custom.txt", "replace_words.txt"),
            prohibit: spelling_lists!("fr", "prohibit.txt"),
        },
        "nl" => SpellingRuleConfig {
            id: "MORFOLOGIK_RULE_NL_NL",
            latin_script: true,
            ignore: spelling_lists!("nl", "corpus_extra.txt"),
            prohibit: &[],
        },
        // de: GermanSpellerRule (fires as GERMAN_SPELLER_RULE with
        // language=de-DE; the generic "de" code does not run it). The
        // wordlist fuses the de_DE.dict dump with LT's ignore/spelling/
        // spelling_merged/added lists, and unknown words are accepted when
        // they decompose into known parts (German compounds).
        // LT quirk: the German speller runs under the specific code de-DE
        // only (language=de does NOT run it); the port mirrors that gate.
        "de-DE" => SpellingRuleConfig {
            id: "GERMAN_SPELLER_RULE",
            latin_script: true,
            ignore: &[],
            prohibit: &[],
        },
        // it: Morfologik it_IT dictionary + hunspell ignore/spelling lists
        // (the local LT 6.5 fires MORFOLOGIK_RULE_IT_IT)
        "it" => SpellingRuleConfig {
            id: "MORFOLOGIK_RULE_IT_IT",
            latin_script: true,
            ignore: spelling_lists!("it", "ignore.txt"),
            prohibit: &[],
        },
        // ga: Morfologik ga_IE dictionary (local LT fires MORFOLOGIK_RULE_GA_IE)
        "ga" => SpellingRuleConfig {
            id: "MORFOLOGIK_RULE_GA_IE",
            latin_script: true,
            ignore: &[],
            prohibit: &[],
        },
        // br: Morfologik br_FR dictionary (local LT fires MORFOLOGIK_RULE_BR_FR)
        "br" => SpellingRuleConfig {
            id: "MORFOLOGIK_RULE_BR_FR",
            latin_script: true,
            ignore: spelling_lists!("br", "corpus_extra.txt"),
            prohibit: &[],
        },
        // gl: Galician speller with the generic HunspellRule id
        // (local LT fires HUNSPELL_RULE for gl)
        "gl" => SpellingRuleConfig {
            id: "HUNSPELL_RULE",
            latin_script: true,
            ignore: spelling_lists!("gl", "table_words.txt", "corpus_extra.txt"),
            prohibit: &[],
        },
        // spellers that fire in the local LT 6.5 corpus measurement
        // (wordlists dumped from their morfologik/hunspell dictionaries)
        // eo: Esperanto hunspell (eo.dic + accept lists)
        "eo" => SpellingRuleConfig {
            id: "HUNSPELL_RULE",
            latin_script: true,
            ignore: &[],
            prohibit: &[],
        },
        // da/sv: DanishSwedishHunspellRule (generic HUNSPELL_RULE id),
        // dictionaries expanded from da_DK.dic/sv_SE.dic (scripts/unmunch.py)
        "da" => SpellingRuleConfig {
            id: "HUNSPELL_RULE",
            latin_script: true,
            ignore: spelling_lists!("da", "ignore.txt", "spelling.txt"),
            prohibit: &[],
        },
        "sv" => SpellingRuleConfig {
            id: "HUNSPELL_RULE",
            latin_script: true,
            ignore: spelling_lists!("sv", "ignore.txt", "spelling.txt"),
            prohibit: &[],
        },
        "crh" => SpellingRuleConfig {
            id: "MORFOLOGIK_RULE_CRH_UA",
            latin_script: true,
            ignore: &[],
            prohibit: &[],
        },
        "ro" => SpellingRuleConfig {
            id: "MORFOLOGIK_RULE_RO_RO",
            latin_script: true,
            ignore: &[],
            prohibit: &[],
        },
        "tl" => SpellingRuleConfig {
            id: "MORFOLOGIK_RULE_TL",
            latin_script: true,
            ignore: &[],
            prohibit: &[],
        },
        "el" => SpellingRuleConfig {
            id: "MORFOLOGIK_RULE_EL_GR",
            latin_script: false,
            ignore: &[],
            prohibit: &[],
        },
        "sk" => SpellingRuleConfig {
            id: "MORFOLOGIK_RULE_SK_SK",
            latin_script: true,
            ignore: &[],
            prohibit: &[],
        },
        "sl" => SpellingRuleConfig {
            id: "MORFOLOGIK_RULE_SL_SI",
            latin_script: true,
            ignore: &[],
            prohibit: &[],
        },
        "ast" => SpellingRuleConfig {
            id: "MORFOLOGIK_RULE_AST",
            latin_script: true,
            ignore: &[],
            prohibit: &[],
        },
        "be" => SpellingRuleConfig {
            id: "MORFOLOGIK_RULE_BE_BY",
            latin_script: false,
            ignore: spelling_lists!("be", "replace_words.txt", "corpus_extra.txt"),
            prohibit: &[],
        },
        "ar" => SpellingRuleConfig {
            id: "HUNSPELL_RULE_AR",
            latin_script: false,
            ignore: spelling_lists!("ar", "ignore.txt", "spelling.txt", "spelling_custom.txt", "corpus_extra.txt"),
            prohibit: spelling_lists!("ar", "prohibit.txt", "prohibit_custom.txt"),
        },
        "en" => SpellingRuleConfig {
            id: "MORFOLOGIK_RULE_EN_US",
            latin_script: true,
            ignore: spelling_lists!(
                "en",
                "ignore.txt",
                "spelling.txt",
                "corpus_extra.txt",
                "spelling_custom.txt",
                "spelling_en-US.txt",
                "replace_words.txt"
            ),
            prohibit: spelling_lists!("en", "prohibit.txt", "prohibit_custom.txt"),
        },
        "pt" => SpellingRuleConfig {
            id: "MORFOLOGIK_RULE_PT",
            latin_script: true,
            ignore: spelling_lists!("pt", "replace_words.txt"),
            prohibit: &[],
        },
        "ru" => SpellingRuleConfig {
            id: "MORFOLOGIK_RULE_RU_RU",
            latin_script: false,
            ignore: spelling_lists!("ru", "ignore.txt", "spelling.txt", "replace_words.txt"),
            prohibit: spelling_lists!("ru", "prohibit.txt"),
        },
        "uk" => SpellingRuleConfig {
            id: "MORFOLOGIK_RULE_UK_UA",
            latin_script: false,
            ignore: spelling_lists!("uk", "ignore.txt", "spelling.txt", "replace_words.txt"),
            prohibit: &[],
        },
        // MorfologikPolishSpellerRule: pl/hunspell/pl_PL.dict + ignore/
        // spelling/prohibit lists; compound-adjective suppression
        // (isNotCompound) not ported yet
        "pl" => SpellingRuleConfig {
            id: "MORFOLOGIK_RULE_PL_PL",
            latin_script: true,
            ignore: spelling_lists!("pl", "ignore.txt", "spelling.txt", "replace_words.txt"),
            prohibit: spelling_lists!("pl", "prohibit.txt"),
        },
        // LT's KhmerHunspellRule: generic id, khmer script, hunspell
        // km_KH dictionary as the accepted-words wordlist
        "km" => SpellingRuleConfig {
            id: "HUNSPELL_RULE",
            latin_script: false,
            ignore: spelling_lists!("km", "ignore.txt"),
            prohibit: &[],
        },
        _ => return None,
    })
}

/// Port of `SpellingCheckRule.ignoreWord` + `MorfologikSpellerRule.canBeIgnored`.
fn spelling_ignored(word: &str, config: &SpellingRuleConfig) -> bool {
    if word.chars().count() > 200 {
        return true;
    }
    // Tokens with no letters cannot have spelling errors
    let has_letter = if config.latin_script {
        word.chars().any(|c| c.is_alphabetic() && is_latin(c))
    } else {
        word.chars().any(char::is_alphabetic)
    };
    if !has_letter {
        return true;
    }
    // LT's spellers ignore mixed digit-letter tokens ("25°C", "3rd")
    if word.chars().any(|c| c.is_ascii_digit()) {
        return true;
    }
    let is_ignored = |w: &str| config.ignore.contains(&w);
    if word.ends_with('.') && !is_ignored(word) {
        return is_ignored(word.strip_suffix('.').unwrap_or(word));
    }
    is_ignored(word)
}

/// German compound acceptance: `Bundesbildungsministerin` style words not
/// present as-is in the dictionary are correct when they decompose into
/// dictionary parts. Hyphenated compounds need every segment known; plain
/// compounds use a greedy left-longest split (min 3 chars per part).
fn german_compound_known(word: &str, speller: &crate::rule::filter_data::SpellerDict) -> bool {
    if word.contains('-') {
        // whole compound first ("E-Mail", "Kfz-Versicherung")
        if speller.is_known(word) || speller.is_known(&word.to_lowercase()) {
            return true;
        }
        // segments must be known; a short unknown segment is accepted when
        // joined with a neighbour it forms a dictionary word ("E-Mail")
        let segs: Vec<&str> = word
            .split('-')
            .map(|s| s.trim_end_matches('.'))
            .collect();
        let known = |s: &str| speller.is_known(s) || speller.is_known(&s.to_lowercase());
        let mut ok = true;
        for seg in &segs {
            if known(seg) {
                continue;
            }
            let joined = segs.iter().any(|other| {
                other != seg && (known(&format!("{}-{}", seg, other)) || known(&format!("{}-{}", other, seg)))
            });
            if !joined {
                ok = false;
                break;
            }
        }
        return ok;
    }
    // German Fugenelemente: the dict form of a compound head may carry a
    // linker suffix ("Enddefekt" = "Ende" + "defekt")
    fn head_ok(head: &str, speller: &crate::rule::filter_data::SpellerDict) -> bool {
        let lower = head.to_lowercase();
        if speller.is_known(head) || speller.is_known(&lower) {
            return true;
        }
        for linker in ["e", "en", "n", "s", "es"] {
            let mut joined = String::with_capacity(lower.len() + linker.len());
            joined.push_str(&lower);
            joined.push_str(linker);
            if speller.is_known(&joined) {
                return true;
            }
        }
        false
    }
    fn decompose(w: &str, speller: &crate::rule::filter_data::SpellerDict, depth: u8) -> bool {
        if depth == 0 || w.chars().count() < 6 {
            return false;
        }
        // greedy left-longest: try the longest dictionary prefix first
        let mut split_points: Vec<usize> = Vec::new();
        for (i, _) in w.char_indices().skip(2) {
            if i + 3 <= w.len() {
                split_points.push(i);
            }
        }
        split_points.reverse();
        for i in split_points {
            let head = &w[..i];
            let tail = &w[i..];
            if tail.chars().count() < 3 {
                continue;
            }
            if head_ok(head, speller)
                && (speller.is_known(tail) || speller.is_known(&tail.to_lowercase()))
            {
                return true;
            }
            if head_ok(head, speller) && decompose(tail, speller, depth - 1) {
                return true;
            }
        }
        false
    }
    decompose(word, speller, 4)
}

fn is_latin(c: char) -> bool {
    // approximate Java's \p{script=latin}
    let cp = c as u32;
    (0x41..=0x5A).contains(&cp)
        || (0x61..=0x7A).contains(&cp)
        || ((0xC0..=0xFF).contains(&cp) && cp != 0xD7 && cp != 0xF7)
        || (0x100..=0x24F).contains(&cp)
        || (0x1E00..=0x1EFF).contains(&cp)
}

/// Java's \\p{L} — Unicode general category L* exactly. Khmer vowel signs
/// and diacritics (U+17B4..U+17DB, U+17DD) are marks/signs, not letters:
/// LT's HunspellRule.tokenizeText splits runs on them, so hunspell checks
/// each consonant cluster of a Khmer word ("បញ្ញត្តិ" → "បញ" "ញត" "តិ"),
/// flagging every cluster missing from the km_KH dictionary. Outside the
/// Khmer block is_alphabetic() is used (identical for Latin/digits).
fn is_java_letter(c: char) -> bool {
    match c as u32 {
        0x1780..=0x17B3 | 0x17D7 | 0x17DC => true, // Khmer letters
        0x17B4..=0x17D6 | 0x17D8..=0x17DB | 0x17DD => false, // marks/signs/punct
        _ => c.is_alphabetic(),
    }
}

/// Port of `MorfologikSpellerRule.match` (simplified: plain whole-token lookup,
/// no wrong-split detection, no compound handling).
pub(crate) fn morfologik_spelling(
    sentence: &Sentence,
    lang: Option<&str>,
    speller: Option<&crate::rule::filter_data::SpellerDict>,
) -> Vec<Suggestion> {
    let speller = match speller {
        Some(s) if !s.is_empty() => s,
        _ => return Vec::new(),
    };
    let lang = match lang {
        Some(l) => l,
        None => return Vec::new(),
    };
    let config = match spelling_config(lang) {
        Some(c) => c,
        None => return Vec::new(),
    };

    // LT hunspell-dictionary languages (da/sv/eo/km/ar/gl): delegate the
    // dictionary lookup and suggestion generation to the system libhunspell
    // loaded with LT's own .aff/.dic (embedded in the bin). Probed live:
    // reproduces LT 6.6's is_known and suggestion lists. Falls back to the
    // FST approximation when the library is unavailable.
    let hu_data = speller
        .hunspell
        .as_ref()
        .filter(|d| d.is_present())
        .filter(|_| crate::hunspell::available());
    let known = |w: &str| -> bool {
        if let Some(d) = hu_data {
            if let Some(r) = crate::hunspell::spell(lang, d, w) {
                return r;
            }
        }
        speller.is_known(w)
    };

    let tokens: Vec<_> = sentence.iter().collect();
    let mut out = Vec::new();

    // es/ca/fr call setIgnoreTaggedWords(): words the tagger knows are
    // never spelling errors. pt does NOT (probed: LT flags the tagger-known
    // 'conosco' — a pt-BR form absent from the pt-PT-45 dictionary)
    let ignore_tagged = matches!(lang, "es" | "ca" | "fr");

    for (idx, token) in tokens.iter().enumerate() {
        let word = token.word().as_str();
        if word.is_empty() || is_url(word) || is_email(word) || is_spelling_ignored_token(token) {
            continue;
        }
        // km: LT's KhmerHunspellRule splits tokens into \\p{L} runs (Khmer
        // vowel signs/diacritics are split points) and hunspell-checks each
        // consonant cluster — the generic whole-token path below would flag
        // whole Khmer words instead. Wrong-split matches are skipped: they
        // share this rule id with a direct flag, so id agreement is unaffected.
        if lang == "km" {
            let base = token.span().start();
            // (byte_start, byte_end, char_start, char_end) per letter run
            let mut runs: Vec<(usize, usize, usize, usize)> = Vec::new();
            let mut open: Option<(usize, usize)> = None;
            let mut ci = 0usize;
            for (bi, c) in word.char_indices() {
                if is_java_letter(c) {
                    if open.is_none() {
                        open = Some((bi, ci));
                    }
                } else if let Some((bs, cs)) = open.take() {
                    runs.push((bs, bi, cs, ci));
                }
                ci += 1;
            }
            if let Some((bs, cs)) = open {
                runs.push((bs, word.len(), cs, ci));
            }
            for (bs, be, cs, ce) in runs {
                let run = &word[bs..be];
                let prohibited = config.prohibit.contains(&run);
                let run_known = known(run);
                // HunspellRule.isMisspelled: (!spell && !ignoreWord) || isProhibited
                if !prohibited && (run_known || config.ignore.contains(&run)) {
                    continue;
                }
                let replacements: Vec<String> = match hu_data.and_then(|d| {
                    crate::hunspell::suggest(lang, d, run)
                }) {
                    Some(list) => list
                        .into_iter()
                        .filter(|c: &String| !config.prohibit.contains(&c.as_str()))
                        .collect(),
                    None => Vec::new(),
                };
                out.push(Suggestion::new(
                    config.id.to_string(),
                    "Possible spelling mistake found.".to_string(),
                    Span::new(base.byte + bs..base.byte + be, base.char + cs..base.char + ce),
                    replacements,
                ));
            }
            continue;
        }
        if spelling_ignored(word, &config)
            // fragments of hyphenated splits and single characters (our
            // tokenizer splits where LT's does not)
            || word.chars().count() < 2
            || word.starts_with('-')
            || word.ends_with('-')
        {
            continue;
        }
        // ru: LT's speller accepts hyphenated forms whose joined form is a
        // dictionary word (по-немногу -> понемногу), so RU_COMPOUNDS can
        // flag them instead
        if lang == "ru" && word.contains('-') {
            let joined: String = word.chars().filter(|c| *c != '-').collect();
            if !joined.is_empty() && speller.is_known(&joined) {
                continue;
            }
        }
        // English speller: words containing a dot (domain-like
        // "wordpress.com") are ignored, and hyphenated words are checked
        // part by part, flagging only an unknown part
        if lang == "en" && word.contains('.') {
            continue;
        }
        // en: tokens containing an apostrophe (contractions like "don't",
        // suffix tokens like 'RE, quoted 'word) are not spelling-checked
        if lang == "en" && (word.contains('\'') || word.contains('\u{2019}')) {
            continue;
        }
        // English speller: hyphenated words are not checked as a whole
        // (LT checks dictionary compounds; empirically even unknown parts
        // like "well-xzyzing" are not flagged)
        if lang == "en" && word.contains('-') {
            continue;
        }
        // a reading with a real part-of-speech (not the empty pseudo-reading
        // and not UNKNOWN) makes the word "tagged"
        if ignore_tagged
            && token.word().tags().iter().any(|t| {
                let pos = t.pos().as_str();
                // real part-of-speech only: the tokenizer encodes sentence
                // boundaries as meta tags (SENT_END) and punctuation as
                // _PUNCT — LT keeps those off the tagger readings, and
                // setIgnoreTaggedWords must not treat them as "tagged"
                // (probed: 'mundo. qqqxyz' must fire MORFOLOGIK on the
                // last token, LT does)
                !pos.is_empty() && pos != "UNKNOWN" && !pos.starts_with('_') && pos != "SENT_END"
            })
        {
            continue;
        }
        // ArabicHunspellSpellerRule strips tashkeel before every lookup
        let word = if lang == "ar" {
            crate::tokenizer::ar_stem::remove_tashkeel(word)
        } else {
            word.to_string()
        };
        let word = word.as_str();
        let prohibited = config.prohibit.contains(&word);
        if known(word) && !prohibited {
            continue;
        }
        // GermanSpellerRule accepts unknown words that decompose into
        // dictionary parts (compounds): hyphen segments, or a greedy
        // left-longest split with >=2 known parts of >=3 chars each.
        if config.id == "GERMAN_SPELLER_RULE"
            && !prohibited
            && german_compound_known(word, speller)
        {
            continue;
        }
        // eo: real hunspell expansion since the 6.6 re-baseline
        // (data2/eo/filters/speller.txt = eo.dic unmunch, 832k forms); the
        // old vowel-less-junk heuristic under-fired 224 corpus sentences
        let span = Span::from_positions(token.span().start(), token.span().end());

        // hunspell languages: LT's suggestion list is hunspell's own
        // (phonetic/ngram/compound candidates in hunspell's order); no
        // tier escalation, no run-on splits, no convert-case
        if let Some(list) = hu_data.and_then(|d| crate::hunspell::suggest(lang, d, word)) {
            let replacements: Vec<String> = list
                .into_iter()
                .filter(|c: &String| !config.prohibit.contains(&c.as_str()))
                .collect();
            let is_sentence_start = idx == 0 && idx < tokens.len() - 1;
            let replacements = if is_sentence_start {
                replacements
                    .into_iter()
                    .map(|r| {
                        if r == r.to_lowercase() {
                            uppercase_first_char(&r)
                        } else {
                            r
                        }
                    })
                    .collect()
            } else {
                replacements
            };
            out.push(Suggestion::new(
                config.id.to_string(),
                "Possible spelling mistake found.".to_string(),
                span,
                replacements,
            ));
            continue;
        }

        // suggestion candidates in escalating tiers (LT speller1/2/3):
        // the FIRST non-empty distance tier wins — probed: 'casx'/'peroo'
        // (d1 candidates exist → no d2s in LT's list), 'hatsap' (no d1 →
        // all-d2 list), 'recieve'/'seperate' (en: transposition d1 tier
        // only). Residual: some inputs mix tiers ('HATSAA' shows hasta d1
        // + hafsa d2) — morfologik's FSA-walk candidate semantics, see
        // delta_log.md
        //
        // el: morfologik's SUFFIX-encoded automaton mixes char-distance
        // tiers ('λαθος' → d1 'λάθος' + d2 'λάλος' + case variants in ONE
        // list) — delegate to the persistent ElSuggest Java sidecar
        // (LT's own morfologik speller, distance 2, cap 18 = probed LT
        // behavior); fall back to the FST tiers when it is unavailable.
        let mut replacements: Vec<String> = Vec::new();
        let mut similar: Vec<String> = Vec::new();
        if lang == "el" {
            if let Some(cands) =
                crate::morfologik_sidecar::MorfologikSidecar::suggest("el", word)
            {
                similar = cands;
            }
        }
        if similar.is_empty() {
            for tier in 1..=3 {
                let found = if speller.ignore_diacritics {
                    speller.find_similar_ignoring_diacritics(word, tier)
                } else {
                    speller.find_similar(word, tier)
                };
                if !found.is_empty() {
                    similar = found;
                    break;
                }
            }
        }
        for candidate in similar {
            if config.prohibit.contains(&candidate.as_str()) {
                continue;
            }
            if !replacements.contains(&candidate) {
                replacements.push(candidate);
            }
        }
        // LT replaceRunOnWordCandidates: a run-on word splits into two
        // dictionary words ('acomputer' -> ['computer', 'a computer']).
        // Probed live vs LT 6.6: en appends splits, es does NOT
        // ('quieroiracasa'/'casagrande'/'elperro' get dictionary candidates
        // only — the port's extra 'mier coles' split was the smoke diff).
        // Gated to en until another language is probed to do this.
        let char_len = word.chars().count();
        if lang == "en" && char_len >= 3 {
            let lower_word = word.to_lowercase();
            let chars: Vec<(usize, char)> = lower_word.char_indices().collect();
            for (ci, &(bi, _)) in chars.iter().enumerate() {
                if ci == 0 || bi == 0 {
                    continue;
                }
                let (left, right) = (&lower_word[..bi], &lower_word[bi..]);
                if !left.is_empty()
                    && !right.is_empty()
                    && speller.is_known(left)
                    && speller.is_known(right)
                {
                    let split = format!("{} {}", left, right);
                    if !replacements.contains(&split) {
                        replacements.push(split);
                    }
                }
            }
        }
        // morfologik convert-case: re-case suggestions to the misspelled
        // word's casing (HATSAP -> HAFSA..., Hatsap -> Hasta...)
        if speller.convert_case {
            let has_upper = word.chars().any(|c| c.is_uppercase());
            let all_upper =
                has_upper && word.chars().count() > 1 && word.chars().all(|c| !c.is_lowercase());
            let capitalized = has_upper && !all_upper;
            if all_upper {
                for r in replacements.iter_mut() {
                    // "do not use capitalized word if it's mixed case":
                    // keep mixed-case dictionary forms as-is (WHATSAPP is
                    // uppercased, WhatsApp stays mixed; Hafsa is only
                    // capitalized so it becomes HAFSA) — morfologik
                    // StringTools.isMixedCase
                    let mixed = r.chars().skip(1).any(|c| c.is_uppercase())
                        && r.chars().any(|c| c.is_lowercase());
                    if !mixed {
                        *r = r.to_uppercase();
                    }
                }
            } else if capitalized {
                for r in replacements.iter_mut() {
                    let c = uppercase_first_char(r);
                    if c != *r {
                        *r = c;
                    }
                }
            }
        }
        // Capitalize the word at the sentence start (if it is not the last
        // token), like MorfologikSpellerRule does
        let is_sentence_start = idx == 0 && idx < tokens.len() - 1;
        if is_sentence_start {
            for r in replacements.iter_mut() {
                if *r == r.to_lowercase() {
                    let capitalized = uppercase_first_char(r);
                    if capitalized != *r {
                        *r = capitalized;
                    }
                }
            }
            // re-casing can collide ('Λάζος' from 'Λάζος' and 'λάζος') —
            // LT dedupes after capitalizing (el sentence-start probe:
            // 18 mid-sentence candidates -> 17 distinct capitalized)
            replacements.dedup_by(|a, b| a == b);
            let mut seen: Vec<String> = Vec::new();
            replacements.retain(|r| {
                if seen.contains(r) {
                    false
                } else {
                    seen.push(r.clone());
                    true
                }
            });
        }

        out.push(Suggestion::new(
            config.id.to_string(),
            "Possible spelling mistake found.".to_string(),
            span,
            replacements,
        ));
    }

    out
}

// ---------------------------------------------------------------------------
// Text-level builtin families ported from LT 6.5 (task-4).
// Language gating follows what the local LT 6.5 server fires at the
// default level (probed 2026-09-28, see delta_log).
// ---------------------------------------------------------------------------

/// LT `DoublePunctuationRule`: two consecutive equal punctuation marks
/// ("..", ",,"). "..." (ellipsis) and numeric contexts are allowed; fr also
/// allows "..," "?.." "!..".
/// LT `SpanishQuestionMarkDeletionRule`-family behavior (id
/// `ES_QUESTION_MARK`): a question/exclamation sentence must open with
/// ¿/¡. Flags the first word with a `¿`/`¡`-prefixed suggestion.
pub(crate) fn es_question_mark(sentence: &Sentence, lang: Option<&str>) -> Vec<Suggestion> {
    if lang != Some("es") {
        return Vec::new();
    }
    let tokens: Vec<_> = sentence.iter().collect();
    if tokens.is_empty() {
        return Vec::new();
    }
    let last = tokens[tokens.len() - 1].word().as_str();
    let opener = if last.ends_with('?') {
        '\u{bf}' // ¿
    } else if last.ends_with('!') {
        '\u{a1}' // ¡
    } else {
        return Vec::new();
    };
    let first = tokens[0].word().as_str();
    if first.starts_with('\u{bf}') || first.starts_with('\u{a1}') {
        return Vec::new();
    }
    // numbers and non-words at sentence start: LT's rule needs a word
    if !first.chars().next().map_or(false, |c| c.is_alphabetic()) {
        return Vec::new();
    }
    let start = tokens[0].span().start();
    let end = tokens[0].span().end();
    let replacement = format!("{}{}", opener, first);
    vec![Suggestion::new(
        "ES_QUESTION_MARK".to_string(),
        "Falta el signo de apertura.".to_string(),
        Span::new(start.byte..end.byte, start.char..end.char),
        vec![replacement],
    )]
}

/// LT `RussianVerbConjugationRule` (id `RU_VERB_CONJUGATION`):
/// pronoun + present-tense verb with mismatched person ("Он работаю").
/// Span covers pronoun+verb like LT's. Bounded: verb endings map to a
/// clear person class (-у/-ю 1sg, -ешь/-ёшь 2sg, -ет/-ёт 3sg, -ем/-ём 1pl,
/// -ете/-ёте 2pl, -ут/-ют/-ат/-ят 3pl); other endings (past, infinitive,
/// imperative) are skipped.
pub(crate) fn ru_verb_conjugation(sentence: &Sentence, lang: Option<&str>) -> Vec<Suggestion> {
    if lang != Some("ru") {
        return Vec::new();
    }
    let tokens: Vec<_> = sentence.iter().collect();
    let pronoun_person = |w: &str| -> Option<usize> {
        match w.to_lowercase().as_str() {
            "я" => Some(0),
            "ты" => Some(1),
            "он" | "она" | "оно" => Some(2),
            "мы" => Some(3),
            "вы" => Some(4),
            "они" => Some(5),
            _ => None,
        }
    };
    let verb_person = |w: &str| -> Option<usize> {
        let lw = w.to_lowercase();
        if lw.len() < 5 {
            return None;
        }
        // descartar infinitivos y pretéritos
        if lw.ends_with("ться") || lw.ends_with("сть") || lw.ends_with("ти") || lw.ends_with("л") || lw.ends_with("ла") || lw.ends_with("ло") || lw.ends_with("ли") {
            return None;
        }
        if lw.ends_with("ешь") || lw.ends_with("ёшь") {
            Some(1)
        } else if lw.ends_with("ет") || lw.ends_with("ёт") {
            Some(2)
        } else if lw.ends_with("ем") || lw.ends_with("ём") {
            Some(3)
        } else if lw.ends_with("ете") || lw.ends_with("ёте") {
            Some(4)
        } else if lw.ends_with("ут") || lw.ends_with("ют") || lw.ends_with("ат") || lw.ends_with("ят") {
            Some(5)
        } else if lw.ends_with("у") || lw.ends_with("ю") {
            Some(0)
        } else {
            None
        }
    };
    let mut out = Vec::new();
    for pair in tokens.windows(2) {
        let (pron, verb) = (pair[0].word().as_str(), pair[1].word().as_str());
        let (pp, vp) = match (pronoun_person(pron), verb_person(verb)) {
            (Some(p), Some(v)) => (p, v),
            _ => continue,
        };
        if pp == vp {
            continue;
        }
        let s = pair[0].span().start();
        let e = pair[1].span().end();
        out.push(Suggestion::new(
            "RU_VERB_CONJUGATION".to_string(),
            "Verb conjugation does not match the pronoun.".to_string(),
            Span::new(s.byte..e.byte, s.char..e.char),
            Vec::new(),
        ));
    }
    out
}

/// LT `TopoReplaceRule` (id `BR_TOPO`, br): French place names mapped to
/// their Breton spellings (rules/br/topo.txt, `French=Breton` lines).
pub(crate) fn br_topo(sentence: &Sentence, lang: Option<&str>) -> Vec<Suggestion> {
    if lang != Some("br") {
        return Vec::new();
    }
    let map = static_ref!(
        std::collections::HashMap<String, String> = {
            let mut m = std::collections::HashMap::new();
            for line in include_str!("builtin_data/br_topo.txt").lines() {
                let line = line.trim();
                if line.is_empty() || line.starts_with('#') || !line.contains('=') {
                    continue;
                }
                let (fr, brz) = line.split_once('=').unwrap();
                let (fr, brz) = (fr.trim(), brz.trim());
                if !fr.is_empty() && !brz.is_empty() {
                    m.insert(fr.to_string(), brz.to_string());
                }
            }
            m
        }
    );
    let tokens: Vec<_> = sentence.iter().collect();
    let mut out = Vec::new();
    for token in &tokens {
        let word = token.word().as_str();
        if let Some(brz) = map.get(word) {
            let s = token.span().start();
            let e = token.span().end();
            out.push(Suggestion::new(
                "BR_TOPO".to_string(),
                "Brezhoneg: anv-lec'h brezhonek.".to_string(),
                Span::new(s.byte..e.byte, s.char..e.char),
                vec![brz.clone()],
            ));
        }
    }
    out
}

/// LT `GermanCaseRule` (id `DE_CASE`): a run of consecutive capitalized
/// definite articles (Der/Die/Das) — after the leading pair the rest of
/// the run must be lowercase. Empirically: a 2-token run flags its second
/// member, runs of 3+ flag from the third member on.
pub(crate) fn de_case(sentence: &Sentence, lang: Option<&str>) -> Vec<Suggestion> {
    if lang != Some("de") {
        return Vec::new();
    }
    let tokens: Vec<_> = sentence.iter().collect();
    let is_cap_article = |w: &str| matches!(w, "Der" | "Die" | "Das");
    let mut out = Vec::new();
    let mut i = 1;
    while i < tokens.len() {
        if !is_cap_article(tokens[i].word().as_str()) {
            i += 1;
            continue;
        }
        let start = i;
        while i < tokens.len() && is_cap_article(tokens[i].word().as_str()) {
            i += 1;
        }
        let run = &tokens[start..i];
        let from = if run.len() >= 3 { 2 } else { 1 };
        for token in run.iter().skip(from) {
            let lower = token.word().as_str().to_lowercase();
            let s = token.span().start();
            let e = token.span().end();
            out.push(Suggestion::new(
                "DE_CASE".to_string(),
                "Kleinschreibung der Artikelreihe.".to_string(),
                Span::new(s.byte..e.byte, s.char..e.char),
                vec![lower],
            ));
        }
    }
    out
}

/// LT `RussianCompoundRule` (id `RU_COMPOUNDS`, resource
/// ru/compounds.txt): entries marked `+` are words that must be written
/// joined — the hyphenated form ("по-немногу") gets flagged with the
/// joined form ("понемногу") as the suggestion.
pub(crate) fn ru_compounds(sentence: &Sentence, lang: Option<&str>) -> Vec<Suggestion> {
    if lang != Some("ru") {
        return Vec::new();
    }
    let map = static_ref!(
        std::collections::HashMap<String, String> = {
            let mut m = std::collections::HashMap::new();
            for line in include_str!("builtin_data/ru/compounds.txt").lines() {
                let line = line.trim();
                if line.is_empty() || line.starts_with('#') || !line.ends_with('+') {
                    continue;
                }
                let hyphenated = line.trim_end_matches('+');
                if hyphenated.contains('-') {
                    m.insert(
                        hyphenated.to_string(),
                        hyphenated.chars().filter(|c| *c != '-').collect(),
                    );
                }
            }
            m
        }
    );
    let tokens: Vec<_> = sentence.iter().collect();
    let mut out = Vec::new();
    for token in &tokens {
        let word = token.word().as_str();
        // case-sensitive lookup with sentence-start capitalization fallback
        let sugg = match map.get(word) {
            Some(s) => Some(s.clone()),
            None => {
                // capitalize the lookup key if the token starts a sentence
                let mut chars = word.chars();
                match chars.next() {
                    Some(first) if first.is_uppercase() => {
                        let lower: String =
                            first.to_lowercase().collect::<Vec<_>>().into_iter().chain(chars).collect();
                        map.get(&lower).map(|s| {
                            let mut out = String::new();
                            if let Some(c) = s.chars().next() {
                                out.extend(c.to_uppercase());
                                out.push_str(&s[c.len_utf8()..]);
                            }
                            out
                        })
                    }
                    _ => None,
                }
            }
        };
        if let Some(joined) = sugg {
            let start = token.span().start();
            let end = token.span().end();
            out.push(Suggestion::new(
                "RU_COMPOUNDS".to_string(),
                "Escribir sin guion.".to_string(),
                Span::new(start.byte..end.byte, start.char..end.char),
                vec![joined],
            ));
        }
    }
    out
}

/// LT `ArabicDiacriticsRule` (id `AR_DIACRITICS_REPLACE`,
/// AbstractSimpleReplaceRule2 over rules/ar/diacritics.txt): plain
/// (undiacritized) words and short phrases that have a canonical
/// diacritized form get flagged with the diacritized replacement.
pub(crate) fn arabic_diacritics(sentence: &Sentence, lang: Option<&str>) -> Vec<Suggestion> {
    if lang != Some("ar") {
        return Vec::new();
    }
    // phrase (Vec of words) -> primary diacritized suggestion
    let map = static_ref!(
        std::collections::HashMap<Vec<String>, String> = {
            let mut m = std::collections::HashMap::new();
            for line in include_str!("builtin_data/ar/diacritics.txt").lines() {
                let line = line.trim();
                if line.is_empty() || line.starts_with('#') || !line.contains('=') {
                    continue;
                }
                let (key, vals) = line.split_once('=').unwrap();
                let first = vals.split('\t').next().unwrap_or("");
                let first = first.split('|').next().unwrap_or("").trim();
                if first.is_empty() {
                    continue;
                }
                m.insert(
                    key.split_whitespace().map(str::to_string).collect::<Vec<_>>(),
                    first.to_string(),
                );
            }
            m
        }
    );
    let tokens: Vec<_> = sentence.iter().collect();
    let mut out = Vec::new();
    let mut i = 0;
    while i < tokens.len() {
        // try the longest phrase first (max 3 words in the data)
        for len in (1..=3usize).rev() {
            if i + len > tokens.len() {
                continue;
            }
            let phrase: Vec<String> = tokens[i..i + len]
                .iter()
                .map(|t| t.word().as_str().to_string())
                .collect();
            if let Some(sugg) = map.get(&phrase) {
                let start = tokens[i].span().start();
                let end = tokens[i + len - 1].span().end();
                out.push(Suggestion::new(
                    "AR_DIACRITICS_REPLACE".to_string(),
                    "Sustituir por la forma diacritizada.".to_string(),
                    Span::new(start.byte..end.byte, start.char..end.char),
                    vec![sugg.clone()],
                ));
                i += len;
                break;
            }
        }
        i += 1;
    }
    out
}

/// LT `SubjectVerbAgreementRule` (id `DE_SUBJECT_VERB_AGREEMENT`):
/// "Die Kinder ist ..." — plural subject (die + plural noun) with a
/// singular form of *sein*; suggests the plural form on the verb span.
/// Bounded approximation of LT's POS-based rule: determiner `die` +
/// noun ending in the productive German plural markers er/en.
pub(crate) fn de_subject_verb_agreement(sentence: &Sentence, lang: Option<&str>) -> Vec<Suggestion> {
    if lang != Some("de") {
        return Vec::new();
    }
    let tokens: Vec<_> = sentence.iter().collect();
    let mut out = Vec::new();
    for i in 1..tokens.len().saturating_sub(1) {
        let det = tokens[i - 1].word().as_str();
        let noun = tokens[i].word().as_str();
        let verb = tokens[i + 1].word().as_str();
        if (det != "Die" && det != "die") || noun.chars().count() < 4 {
            continue;
        }
        let plural = noun.ends_with("er") || noun.ends_with("en");
        if !plural {
            continue;
        }
        let replacement = match verb {
            "ist" | "bin" | "bist" => "sind",
            _ => continue,
        };
        let start = tokens[i + 1].span().start();
        let end = tokens[i + 1].span().end();
        out.push(Suggestion::new(
            "DE_SUBJECT_VERB_AGREEMENT".to_string(),
            "Subjekt und Verb stimmen nicht überein.".to_string(),
            Span::new(start.byte..end.byte, start.char..end.char),
            vec![replacement.to_string()],
        ));
    }
    out
}

/// LT `VerbAgreementRule` (id `DE_VERBAGREEMENT`): personal pronoun
/// followed by a finite verb with the wrong person ("Ich sind", "Wir
/// sagt"). Bounded approximation: sein/haben/werden paradigms exact;
/// regular verbs by ending class (e/st/t/est/et/en). `ihr`/`Sie` are
/// skipped like LT does; `sie` accepts both singular and plural forms.
pub(crate) fn de_verbagreement(sentence: &Sentence, lang: Option<&str>) -> Vec<Suggestion> {
    if lang != Some("de") {
        return Vec::new();
    }
    // (1sg, 2sg, 3sg, 1pl, 2pl)
    const SEIN: [&str; 5] = ["bin", "bist", "ist", "sind", "seid"];
    const HABEN: [&str; 5] = ["habe", "hast", "hat", "haben", "habt"];
    const WERDEN: [&str; 5] = ["werde", "wirst", "wird", "werden", "werdet"];
    // person index for each pronoun (None = skip); sentence-start
    // capitalized forms are accepted via lowercase comparison
    let person = |p: &str| -> Option<usize> {
        let p = p.to_lowercase();
        match p.as_str() {
            "ich" => Some(0),
            "du" => Some(1),
            "er" | "es" => Some(2),
            "sie" => None, // ambiguous 3sg/3pl: only via table below
            "wir" => Some(3),
            _ => None,
        }
    };
    let tokens: Vec<_> = sentence.iter().collect();
    let mut out = Vec::new();
    for i in 0..tokens.len().saturating_sub(1) {
        let pron = tokens[i].word().as_str();
        let verb = tokens[i + 1].word().as_str();
        if verb.is_empty() {
            continue;
        }
        // correct form from the irregular tables
        let table_form = if SEIN.contains(&verb) {
            Some(SEIN)
        } else if HABEN.contains(&verb) {
            Some(HABEN)
        } else if WERDEN.contains(&verb) {
            Some(WERDEN)
        } else {
            None
        };
        if let Some(table) = table_form {
            let correct: Option<usize> = match person(pron) {
                Some(p) => Some(p),
                None if pron.to_lowercase() == "sie" => {
                    // she -> 3sg, they -> 1pl row index 3
                    if table[2] == verb || table[3] == verb {
                        None // agrees with one of the readings
                    } else {
                        Some(2)
                    }
                }
                None => None,
            };
            if let Some(p) = correct {
                if table[p] != verb {
                    let start = tokens[i].span().start();
                    let end = tokens[i + 1].span().end();
                    out.push(Suggestion::new(
                        "DE_VERBAGREEMENT".to_string(),
                        "Subjekt und Verb stimmen nicht überein.".to_string(),
                        Span::new(start.byte..end.byte, start.char..end.char),
                        vec![format!("{} {}", pron, table[p])],
                    ));
                }
            }
            continue;
        }
        // regular verb: classify by ending
        let Some(p) = person(pron) else { continue };
        let stem: &str;
        let observed: usize; // 0=1sg e, 1=2sg st, 2=3sg t, 3=pl en
        if let Some(s) = verb.strip_suffix("en") {
            stem = s;
            observed = 3;
        } else if let Some(s) = verb.strip_suffix("est") {
            stem = s;
            observed = 1;
        } else if let Some(s) = verb.strip_suffix("st") {
            stem = s;
            observed = 1;
        } else if let Some(s) = verb.strip_suffix("et") {
            stem = s;
            observed = 2;
        } else if let Some(s) = verb.strip_suffix('e') {
            stem = s;
            observed = 0;
        } else if let Some(s) = verb.strip_suffix('t') {
            stem = s;
            observed = 2;
        } else {
            continue; // not recognizably finite: skip
        }
        if stem.is_empty() {
            continue;
        }
        let expected: &str = match p {
            0 => "e",
            1 => "st",
            2 => "t",
            _ => "en",
        };
        if observed == p {
            continue;
        }
        // avoid nonsense like flagging 1sg "e" verbs after 3sg pronoun when
        // the observed form already matches the expected ending
        let same_ending = match (observed, p) {
            (0, 0) | (1, 1) | (2, 2) | (3, 3) => true,
            _ => false,
        };
        if same_ending {
            continue;
        }
        let replacement = format!("{}{}", stem, expected);
        // skip unlikely rewrites that reproduce the observed form
        if replacement == verb {
            continue;
        }
        let start = tokens[i].span().start();
        let end = tokens[i + 1].span().end();
        out.push(Suggestion::new(
            "DE_VERBAGREEMENT".to_string(),
            "Subjekt und Verb stimmen nicht überein.".to_string(),
            Span::new(start.byte..end.byte, start.char..end.char),
            vec![format!("{} {}", pron, replacement)],
        ));
    }
    out
}

/// LT `FrenchQuestionWhitespaceRule` family (id `FRENCH_WHITESPACE`):
/// French punctuation !, ?, ;, : must be preceded by a (narrow)
/// non-breaking space. Fires when there is NO space at all; the
/// suggestion inserts U+202F before !/?/; and U+00A0 before `:`.
pub(crate) fn french_whitespace(sentence: &Sentence, lang: Option<&str>) -> Vec<Suggestion> {
    if lang != Some("fr") {
        return Vec::new();
    }
    let tokens: Vec<_> = sentence.iter().collect();
    let text = sentence.text();
    let mut out = Vec::new();
    for i in 1..tokens.len() {
        let punct = tokens[i].word().as_str();
        let space = match punct {
            "!" | "?" | ";" => '\u{202f}',
            ":" | "\u{bb}" => '\u{a0}',
            _ => continue,
        };
        // only fire when the punctuation is glued to the previous word
        let bs_prev_end = tokens[i - 1].span().end().byte;
        let bs_start = tokens[i].span().start().byte;
        if bs_start != bs_prev_end {
            continue;
        }
        let prev_word = tokens[i - 1].word().as_str();
        // sentence-start punctuation and numbers ("10:30") are excluded
        if prev_word.is_empty()
            || prev_word.chars().any(|c| c.is_ascii_digit())
            || punct == ":" && prev_word.chars().all(|c| !c.is_alphabetic())
        {
            continue;
        }
        let _ = text;
        let start = tokens[i - 1].span().start();
        let end = tokens[i].span().end();
        let replacement = format!("{}{}{}", prev_word, space, punct);
        out.push(Suggestion::new(
            "FRENCH_WHITESPACE".to_string(),
            "Espace insécable devant la ponctuation.".to_string(),
            Span::new(start.byte..end.byte, start.char..end.char),
            vec![replacement],
        ));
    }
    // opening guillemet « glued to the following word: insert U+00A0 after
    for i in 0..tokens.len().saturating_sub(1) {
        if tokens[i].word().as_str() != "\u{ab}" {
            continue;
        }
        let glued = tokens[i + 1].span().start().byte == tokens[i].span().end().byte;
        if !glued {
            continue;
        }
        let next_word = tokens[i + 1].word().as_str();
        if next_word.is_empty() || next_word.chars().any(|c| c.is_ascii_digit()) {
            continue;
        }
        let start = tokens[i].span().start();
        let end = tokens[i + 1].span().end();
        let replacement = format!("\u{ab}\u{a0}{}", next_word);
        out.push(Suggestion::new(
            "FRENCH_WHITESPACE".to_string(),
            "Espace insécable après le guillemet.".to_string(),
            Span::new(start.byte..end.byte, start.char..end.char),
            vec![replacement],
        ));
    }
    out
}

/// LT ar-specific whitespace rules: `ARABIC_QM_WHITESPACE` (space before
/// the Arabic question mark ؟) and `ARABIC_SC_WHITESPACE` (space before
/// the Arabic semicolon ؛). Probed on LT 6.5: both flag the whitespace
/// run before the punctuation with a join suggestion.
pub(crate) fn arabic_punct_whitespace(sentence: &Sentence, lang: Option<&str>) -> Vec<Suggestion> {
    if lang != Some("ar") {
        return Vec::new();
    }
    let tokens: Vec<_> = sentence.iter().collect();
    let text = sentence.text();
    let mut out = Vec::new();
    for i in 0..tokens.len().saturating_sub(1) {
        let bs = tokens[i].span().end().byte;
        let be = tokens[i + 1].span().start().byte;
        let gap = if be > bs && be <= text.len() {
            &text[bs..be]
        } else {
            ""
        };
        if gap.is_empty() || !gap.chars().all(char::is_whitespace) || gap.contains('\n') {
            continue;
        }
        let next = tokens[i + 1].word().as_str();
        let id = if next.starts_with('\u{61f}') {
            Some("ARABIC_QM_WHITESPACE")
        } else if next.starts_with('\u{61b}') {
            Some("ARABIC_SC_WHITESPACE")
        } else {
            None
        };
        if let Some(id) = id {
            let cs = tokens[i].span().end().char;
            let ce = tokens[i + 1].span().start().char;
            out.push(Suggestion::new(
                id.to_string(),
                "Whitespace before punctuation.".to_string(),
                Span::new(bs..be, cs..ce),
                vec![next.chars().take(1).collect()],
            ));
        }
    }
    out
}

/// LT `AvsAnRule` (`EN_A_VS_AN`): a/an selection based on the sound of the
/// following word, using LT's own det_a/det_an exception lists.
pub(crate) fn a_vs_an(sentence: &Sentence, lang: Option<&str>) -> Vec<Suggestion> {
    if lang != Some("en") {
        return Vec::new();
    }
    let det_a: &std::collections::HashSet<String> = static_ref!(
        std::collections::HashSet<String> = wordlist(
            include_str!("builtin_data/en/det_a.txt"))
            .into_iter()
            .map(|w: &'static str| w.trim().to_lowercase())
            .collect()
    );
    let det_an: &std::collections::HashSet<String> = static_ref!(
        std::collections::HashSet<String> = wordlist(
            include_str!("builtin_data/en/det_an.txt"))
            .into_iter()
            .map(|w: &'static str| w.trim().to_lowercase())
            .collect()
    );
    let tokens: Vec<_> = sentence.iter().collect();
    let mut out = Vec::new();
    for i in 0..tokens.len().saturating_sub(1) {
        let word = tokens[i].word().as_str();
        let article = match word {
            "a" | "A" => "an",
            "an" | "An" => "a",
            _ => continue,
        };
        let next_raw = tokens[i + 1].word().as_str();
        let next = next_raw.trim_matches(|c: char| !c.is_alphanumeric());
        let first = match next.chars().next() {
            Some(c) => c,
            None => continue,
        };
        if first.is_ascii_digit() {
            // numbers: sound is ambiguous ("an 8" but "a 5")
            continue;
        }
        let lower = next.to_lowercase();
        let all_caps = next.chars().any(|c| c.is_alphabetic())
            && next.chars().all(|c| !c.is_lowercase());
        // hyphenated words ("one-time"): decide by the first segment
        let first_segment = next.split(['-', '\u{2011}']).next().unwrap_or(next);
        let seg_lower = first_segment.to_lowercase();
        let needs_an = if det_an.contains(&lower) {
            true
        } else if det_a.contains(&lower) || det_a.contains(&seg_lower) {
            false
        } else if det_an.contains(&seg_lower) {
            true
        } else if all_caps {
            // abbreviations: ambiguous sound
            continue;
        } else {
            "aeiou".contains(first.to_ascii_lowercase())
        };
        if (word.eq_ignore_ascii_case("a") && needs_an)
            || (word.eq_ignore_ascii_case("an") && !needs_an)
        {
            let start = tokens[i].span().start();
            let end = tokens[i].span().end();
            out.push(Suggestion::new(
                "EN_A_VS_AN".to_string(),
                "Use a different article.".to_string(),
                Span::new(start.byte..end.byte, start.char..end.char),
                vec![article.to_string()],
            ));
        }
    }
    out
}

// ---------------------------------------------------------------------------
// Text-level builtin families (task-4), empirically aligned with the local
// LT 6.5: per-language rule IDs, glue semantics for unpaired brackets
// (brackets separated from words by whitespace are the comma-whitespace
// family's business in most languages), and language-specific comma-/
// bracket-whitespace ids. Probe tables in delta_log (`family_battery`).

struct UnpairedCfg {
    brackets: bool,
    quotes: bool,
    guillemets: bool,
    bracket_id: &'static str,
    quote_id: &'static str,
    /// only fire when the bracket/quote is glued to an adjacent word;
    /// LT's UnpairedBracketsRule ignores whitespace-separated brackets
    /// (then CommaWhitespaceRule fires instead). de/km flag spaced ones.
    glued_only: bool,
    /// replicate LT's GenericUnpairedQuotesRule (TextLevelRule) instead of
    /// the simple odd/even `"" pairing — needed where the language rule
    /// pairs single quotes too and skips POS/NNP/contraction-tagged
    /// apostrophes (en: EN_UNPAIRED_QUOTES)
    lt_generic_quotes: bool,
}

struct TextCfg {
    unp: Option<UnpairedCfg>,
    dbl: &'static str,
    ws: bool,
    /// whitespace before , ; : . ! ?
    comma: &'static str,
    /// whitespace before ) ] }
    comma_close_bracket: &'static str,
    /// whitespace after ( [ {
    comma_open_bracket: &'static str,
}

fn text_cfg(lang: &str) -> Option<TextCfg> {
    let generic_unp = |brackets: bool, quotes: bool| UnpairedCfg {
        brackets,
        quotes,
        guillemets: false,
        bracket_id: "UNPAIRED_BRACKETS",
        quote_id: "UNPAIRED_BRACKETS",
        glued_only: true,
                lt_generic_quotes: false,
    };
    let cws = "COMMA_PARENTHESIS_WHITESPACE";
    Some(match lang {
        "ca" | "eo" | "ga" | "sl" | "tl" | "ast" | "da" | "sv" | "nl" => TextCfg {
            unp: Some(generic_unp(true, true)),
            dbl: "DOUBLE_PUNCTUATION",
            ws: !matches!(lang, "nl"),
            // nl: no check before , ; : but brackets-whitespace fires CWS
            comma: if lang == "nl" { "" } else { cws },
            comma_close_bracket: cws,
            comma_open_bracket: cws,
        },
        "es" => TextCfg {
            unp: Some(UnpairedCfg {
                brackets: true,
                quotes: true,
                guillemets: true,
                bracket_id: "ES_UNPAIRED_BRACKETS",
                quote_id: "ES_UNPAIRED_BRACKETS",
                glued_only: true,
                lt_generic_quotes: false,
            }),
            dbl: "DOUBLE_PUNCTUATION",
            ws: true,
            comma: cws,
            comma_close_bracket: cws,
            comma_open_bracket: cws,
        },
        "el" => TextCfg {
            unp: Some(UnpairedCfg {
                brackets: true,
                quotes: true,
                guillemets: false,
                bracket_id: "EL_UNPAIRED_BRACKETS",
                quote_id: "EL_UNPAIRED_BRACKETS",
                glued_only: true,
                lt_generic_quotes: false,
            }),
            dbl: "DOUBLE_PUNCTUATION",
            ws: true,
            comma: cws,
            comma_close_bracket: cws,
            comma_open_bracket: cws,
        },
        "pl" => TextCfg {
            unp: Some(UnpairedCfg {
                brackets: true,
                quotes: true,
                guillemets: false,
                bracket_id: "PL_UNPAIRED_BRACKETS",
                quote_id: "PL_UNPAIRED_BRACKETS",
                glued_only: true,
                lt_generic_quotes: false,
            }),
            dbl: "DOUBLE_PUNCTUATION",
            ws: true,
            comma: cws,
            comma_close_bracket: cws,
            comma_open_bracket: cws,
        },
        "ru" => TextCfg {
            unp: Some(UnpairedCfg {
                brackets: true,
                quotes: true,
                guillemets: false,
                bracket_id: "RU_UNPAIRED_BRACKETS",
                quote_id: "RU_UNPAIRED_BRACKETS",
                glued_only: true,
                lt_generic_quotes: false,
            }),
            dbl: "DOUBLE_PUNCTUATION",
            ws: true,
            comma: cws,
            comma_close_bracket: cws,
            comma_open_bracket: cws,
        },
        // en has two rules: EN_UNPAIRED_BRACKETS (brackets) and
        // EN_UNPAIRED_QUOTES (straight + typographic quotes, single-quote
        // pairing, POS/NNP/_apostrophe_contraction_ possessive skip —
        // replicates GenericUnpairedQuotesRule with the English overrides)
        "en" => TextCfg {
            unp: Some(UnpairedCfg {
                brackets: true,
                quotes: true,
                guillemets: false,
                bracket_id: "EN_UNPAIRED_BRACKETS",
                quote_id: "EN_UNPAIRED_QUOTES",
                glued_only: true,
                lt_generic_quotes: true,
            }),
            dbl: "DOUBLE_PUNCTUATION",
            ws: false,
            comma: cws,
            comma_close_bracket: cws,
            comma_open_bracket: cws,
        },
        // de flags spaced brackets as UNPAIRED too (no glue rule) and has
        // a separate quotes rule id
        "de" => TextCfg {
            unp: Some(UnpairedCfg {
                brackets: true,
                quotes: true,
                guillemets: false,
                bracket_id: "UNPAIRED_BRACKETS",
                quote_id: "DE_UNPAIRED_QUOTES",
                glued_only: false,
                lt_generic_quotes: false,
            }),
            dbl: "DE_DOUBLE_PUNCTUATION",
            ws: false,
            comma: cws,
            comma_close_bracket: "",
            comma_open_bracket: "",
        },
        // km flags every unpaired bracket/quote (glued or not) and has no
        // comma-whitespace family
        "km" => TextCfg {
            unp: Some(UnpairedCfg {
                brackets: true,
                quotes: true,
                guillemets: false,
                bracket_id: "KM_UNPAIRED_BRACKETS",
                quote_id: "KM_UNPAIRED_BRACKETS",
                glued_only: false,
                lt_generic_quotes: false,
            }),
            dbl: "",
            ws: false,
            comma: "",
            comma_close_bracket: "",
            comma_open_bracket: "",
        },
        // ar: bracket-whitespace reports under the Arabic id
        "ar" => TextCfg {
            unp: Some(UnpairedCfg {
                brackets: true,
                quotes: true,
                guillemets: false,
                bracket_id: "UNPAIRED_BRACKETS",
                quote_id: "UNPAIRED_BRACKETS",
                glued_only: true,
                lt_generic_quotes: false,
            }),
            dbl: "ARABIC_DOUBLE_PUNCTUATION",
            ws: true,
            comma: cws,
            comma_close_bracket: "ARABIC_SC_WHITESPACE",
            comma_open_bracket: "ARABIC_SC_WHITESPACE",
        },
        // gl: ws before basic punctuation is SPACE_BEFORE_PUNCTUATION, no
        // check after '('
        "gl" => TextCfg {
            unp: Some(generic_unp(true, true)),
            dbl: "DOUBLE_PUNCTUATION",
            ws: true,
            comma: "SPACE_BEFORE_PUNCTUATION",
            comma_close_bracket: cws,
            comma_open_bracket: "",
        },
        // pt: SPACE_BEFORE_PUNCTUATION2 for basic punctuation
        "pt" => TextCfg {
            unp: Some(generic_unp(true, true)),
            dbl: "DOUBLE_PUNCTUATION",
            ws: false,
            comma: "SPACE_BEFORE_PUNCTUATION2",
            comma_close_bracket: cws,
            comma_open_bracket: "",
        },
        // brackets only (no straight-quote pairing)
        "fr" | "it" | "ro" | "sk" => TextCfg {
            unp: Some(generic_unp(true, false)),
            dbl: "DOUBLE_PUNCTUATION",
            ws: matches!(lang, "it" | "ro" | "sk"),
            comma: cws,
            comma_close_bracket: cws,
            comma_open_bracket: cws,
        },
        // no unpaired family
        "be" | "br" | "crh" | "ta" | "fa" => TextCfg {
            unp: None,
            dbl: if lang == "fa" {
                "PERSIAN_DOUBLE_PUNCTUATION"
            } else {
                "DOUBLE_PUNCTUATION"
            },
            ws: true,
            comma: cws,
            comma_close_bracket: cws,
            comma_open_bracket: cws,
        },
        "uk" => TextCfg {
            unp: None,
            dbl: "",
            ws: true,
            comma: cws,
            comma_close_bracket: cws,
            comma_open_bracket: cws,
        },
        _ => return None,
    })
}

/// LT `DoublePunctuationRule` family (ids vary per language).
pub(crate) fn double_punctuation(sentence: &Sentence, lang: Option<&str>) -> Vec<Suggestion> {
    let lang = match lang {
        Some(l) => l,
        _ => return Vec::new(),
    };
    let cfg = match text_cfg(lang) {
        Some(c) if !c.dbl.is_empty() => c,
        _ => return Vec::new(),
    };
    let tokens: Vec<_> = sentence.iter().collect();
    let mut out = Vec::new();
    for i in 1..tokens.len() {
        let token = tokens[i].word().as_str();
        let prev = tokens[i - 1].word().as_str();
        let prev_prev = if i >= 2 {
            tokens[i - 2].word().as_str()
        } else {
            ""
        };
        let next = tokens.get(i + 1).map(|t| t.word().as_str()).unwrap_or("");
        let flagged = match token {
            "," | ";" => token == prev,
            "." if prev == "." => {
                // "..." and longer runs of dots are an ellipsis
                !(prev_prev == "." || next == ".")
            }
            _ => false,
        };
        if !flagged {
            continue;
        }
        // numeric contexts like "1.2" / "1,000": never flag next to digits
        let prev_prev_digit = prev_prev.chars().any(|c| c.is_ascii_digit());
        let next_digit = next.chars().any(|c| c.is_ascii_digit());
        if prev_prev_digit || next_digit {
            continue;
        }
        // FrenchDoublePunctuationRule exceptions
        if lang == "fr" && matches!(next, ",") {
            continue;
        }
        let start = tokens[i - 1].span().start();
        let end = tokens[i].span().end();
        out.push(Suggestion::new(
            cfg.dbl.to_string(),
            "Double punctuation.".to_string(),
            Span::new(start.byte..end.byte, start.char..end.char),
            vec![token.to_string()],
        ));
    }
    out
}

/// LT `MultipleWhitespaceRule` (id `WHITESPACE_RULE`): a run of 2+ spaces
/// between two non-whitespace tokens.
pub(crate) fn multiple_whitespace(sentence: &Sentence, lang: Option<&str>) -> Vec<Suggestion> {
    let cfg = match lang.map_or_else(|| None, |l| text_cfg(l)) {
        Some(c) if c.ws => c,
        _ => return Vec::new(),
    };
    // the tokenizer does not emit whitespace tokens: runs of spaces live in
    // the gaps between consecutive tokens
    let tokens: Vec<_> = sentence.iter().collect();
    let text = sentence.text();
    let mut out = Vec::new();
    for i in 0..tokens.len().saturating_sub(1) {
        let bs = tokens[i].span().end().byte;
        let be = tokens[i + 1].span().start().byte;
        if be <= bs {
            continue;
        }
        let gap = &text[bs.min(text.len())..be.min(text.len())];
        let spaces = gap.chars().filter(|c| *c == ' ').count();
        if spaces < 2 || !gap.chars().all(|c| c == ' ') {
            continue;
        }
        let cs = tokens[i].span().end().char;
        let ce = tokens[i + 1].span().start().char;
        out.push(Suggestion::new(
            "WHITESPACE_RULE".to_string(),
            "Multiple whitespace.".to_string(),
            Span::new(bs..be, cs..ce),
            vec![" ".to_string()],
        ));
    }
    out
}

/// LT `CommaWhitespaceRule` family: whitespace directly before closing
/// punctuation / after opening punctuation. Ids vary per language
/// (SPACE_BEFORE_PUNCTUATION for gl, SPACE_BEFORE_PUNCTUATION2 for pt,
/// ARABIC_SC_WHITESPACE for brackets in ar) and some languages check only
/// a subset of the positions.
/// LT `CommaWhitespaceRule` (core Java) for languages that use the base
/// rule verbatim (en). Token-gap translation: LT iterates tokens including
/// whitespace; we walk non-whitespace tokens and use the gaps. Spans include
/// the punctuation (LT flags from prev-token start to current-token end).
/// Handles: space after "(", space around quotes (with single-space before,
/// two suggestions), missing space after ",", space before ")" "," "."
/// with the digit/domain/file-extension/duplicate-comma exceptions.
fn comma_whitespace_java(sentence: &Sentence, id: &'static str) -> Vec<Suggestion> {
    let tokens: Vec<_> = sentence.iter().collect();
    let text = sentence.text();
    let gap = |i: usize| -> &str {
        // whitespace gap AFTER token i (empty when glued or out of range)
        if i + 1 >= tokens.len() {
            return "";
        }
        let bs = tokens[i].span().end().byte;
        let be = tokens[i + 1].span().start().byte;
        if be > bs && be <= text.len() {
            &text[bs..be]
        } else {
            ""
        }
    };
    let is_ws_gap = |g: &str| !g.is_empty() && g.chars().all(char::is_whitespace);
    let is_quote = |w: &str| {
        matches!(w, "'" | "\"" | "\u{2019}" | "\u{201d}" | "\u{201c}" | "\u{ab}" | "\u{bb}")
    };
    let contains_digit = |w: &str| w.chars().any(|c| c.is_ascii_digit());
    let is_domain = |w: &str| {
        w == "com" || w == "org" || w == "net" || w == "int" || w == "edu" || w == "gov" || w == "mil" || (w.len() == 2 && w.chars().all(|c| c.is_ascii_lowercase()))
    };
    let is_file_ext = |w: &str| {
        // ([a-z]{3,4}|[A-Z]{3,4}|ai|mp[34]|MP[34])(-.+)?
        let base = w.split('-').next().unwrap_or("");
        let lower_ok = (3..=4).contains(&base.len()) && base.chars().all(|c| c.is_ascii_lowercase());
        let upper_ok = (3..=4).contains(&base.len()) && base.chars().all(|c| c.is_ascii_uppercase());
        let special = matches!(base, "ai" | "mp3" | "mp4" | "MP3" | "MP4");
        (lower_ok || upper_ok || special) && (base.len() == w.len() || w.contains('-'))
    };
    let mut out = Vec::new();
    let push = |out: &mut Vec<Suggestion>, from_b: usize, to_b: usize, from_c: usize, to_c: usize, repl: Vec<String>| {
        out.push(Suggestion::new(
            id.to_string(),
            "Whitespace.".to_string(),
            Span::new(from_b..to_b, from_c..to_c),
            repl,
        ));
    };
    for i in 0..tokens.len() {
        let w = tokens[i].word().as_str();
        let sb = tokens[i].span().start();
        let se = tokens[i].span().end();
        let prev_w = if i > 0 { tokens[i - 1].word().as_str() } else { "" };
        let prev_sb = if i > 0 { tokens[i - 1].span().start() } else { sb };
        let prev_se = if i > 0 { tokens[i - 1].span().end() } else { sb };
        let g_after = gap(i);
        let g_before = if i > 0 { gap(i - 1) } else { "" };
        let next_w = if i + 1 < tokens.len() { tokens[i + 1].word().as_str() } else { "" };
        // 1) whitespace after "("
        if is_ws_gap(g_after) && w == "(" && !(w == "[" && g_after == " " && next_w == "]") {
            push(&mut out, sb.byte, tokens[i + 1].span().start().byte, sb.char, tokens[i + 1].span().start().char, vec!["(".to_string()]);
            continue;
        }
        // 2) whitespace after a quote with exactly one space before it
        if is_ws_gap(g_after) && is_quote(w) && is_ws_gap(g_before) && g_before == " " {
            push(
                &mut out,
                prev_se.byte,
                tokens[i + 1].span().start().byte,
                prev_se.char,
                tokens[i + 1].span().start().char,
                vec![format!("{} ", w), format!(" {}", w)],
            );
            continue;
        }
        // 3) missing space after "," (glued next, not quote/hyphen/comma,
        // no digits around, not duplicate comma)
        if !is_ws_gap(g_after)
            && w == ","
            && i + 1 < tokens.len()
            && !is_quote(next_w)
            && !(next_w == "-" || next_w == ",")
            && (i == 0 || (!contains_digit(prev_w) && prev_w != ","))
            && !contains_digit(next_w)
        {
            push(
                &mut out,
                sb.byte,
                tokens[i + 1].span().end().byte,
                sb.char,
                tokens[i + 1].span().end().char,
                vec![format!(", {}", next_w)],
            );
            continue;
        }
        // 4) whitespace before ")" / "," / "."
        if is_ws_gap(g_before) {
            if w == ")" && !(w == "]" && g_before == " " && prev_w == "[") {
                push(&mut out, prev_se.byte, se.byte, prev_se.char, se.char, vec![")".to_string()]);
            } else if w == "," {
                // duplicate comma handled by DOUBLE_PUNCTUATION instead
                if next_w != "," {
                    let repl = if !next_w.is_empty() && !is_ws_gap(g_after) {
                        vec![", ".to_string()]
                    } else {
                        vec![",".to_string()]
                    };
                    push(&mut out, prev_se.byte, se.byte, prev_se.char, se.char, repl);
                }
            } else if w == "." && !is_domain(next_w) && !is_file_ext(next_w) {
                // "figures such as .5" and ellipsis, or "./command"
                let next2 = if i + 2 < tokens.len() { tokens[i + 2].word().as_str() } else { "" };
                let digit_or_dot = !next_w.is_empty()
                    && (next_w.starts_with('.') || next_w.chars().next().map_or(false, |c| c.is_ascii_digit()));
                let slash_cmd = next_w == "/" && !next2.is_empty() && next2.chars().all(|c| c.is_ascii_alphabetic());
                if !digit_or_dot && !slash_cmd {
                    push(&mut out, prev_se.byte, se.byte, prev_se.char, se.char, vec![".".to_string()]);
                }
            }
        }
    }
    out
}

pub(crate) fn comma_parenthesis_whitespace(
    sentence: &Sentence,
    lang: Option<&str>,
) -> Vec<Suggestion> {
    let cfg = match lang.map_or_else(|| None, |l| text_cfg(l)) {
        Some(c) => c,
        None => return Vec::new(),
    };
    if lang == Some("en") {
        // en uses the base LT CommaWhitespaceRule verbatim
        return comma_whitespace_java(sentence, cfg.comma);
    }
    let tokens: Vec<_> = sentence.iter().collect();
    let text = sentence.text();
    let mut out = Vec::new();
    for i in 0..tokens.len().saturating_sub(1) {
        let bs = tokens[i].span().end().byte;
        let be = tokens[i + 1].span().start().byte;
        let gap = if be > bs && be <= text.len() {
            &text[bs..be]
        } else {
            ""
        };
        if gap.is_empty() || !gap.chars().all(char::is_whitespace) || gap.contains('\n') {
            continue;
        }
        let cs = tokens[i].span().end().char;
        let ce = tokens[i + 1].span().start().char;
        let prev_word = tokens[i].word().as_str();
        let next_word = tokens[i + 1].word().as_str();
        // whitespace directly before closing punctuation
        let basic = next_word.starts_with(',')
            || next_word.starts_with(';')
            || next_word.starts_with(':')
            || next_word.starts_with('.')
            || next_word.starts_with('!')
            || next_word.starts_with('?');
        let close_bracket =
            next_word.starts_with(')') || next_word.starts_with(']') || next_word.starts_with('}');
        // whitespace directly after an opening bracket
        let open_bracket = prev_word == "(" || prev_word == "[" || prev_word == "{";
        let (id, replacement) = if basic && !cfg.comma.is_empty() {
            (cfg.comma, vec![next_word.chars().take(1).collect()])
        } else if close_bracket && !cfg.comma_close_bracket.is_empty() {
            (cfg.comma_close_bracket, vec![")".to_string()])
        } else if open_bracket && !cfg.comma_open_bracket.is_empty() {
            (cfg.comma_open_bracket, Vec::new())
        } else {
            continue;
        };
        out.push(Suggestion::new(
            id.to_string(),
            "Probable falta de espacio.".to_string(),
            Span::new(bs..be, cs..ce),
            replacement,
        ));
    }
    out
}

/// LT `*UnpairedBracketsRule` families: unmatched (), [], {} (and straight
/// double quotes where the language pairs them). Per-language ids, and the
/// glue rule: in most languages the bracket must be attached to a word —
/// whitespace-separated brackets are flagged by the comma-whitespace family
/// instead. de and km flag spaced brackets themselves.
/// LT GenericUnpairedQuotesRule (TextLevelRule) + EnglishUnpairedQuotesRule
/// overrides, per-sentence form. Pairs quotes per symbol (“"", '"", '"", '‘‘)
/// and skips apostrophes whose token carries POS / NNP /
/// _apostrophe_contraction_ (possessives, contractions, proper nouns get
/// those tags from the disambiguation pass). Verbatim port of the Java
/// control flow incl. the inch-quote heuristic and lastApostropheSymbol.
/// LT `GenericUnpairedBracketsRule` + `SpanishUnpairedBracketsRule`
/// overrides (ES_UNPAIRED_BRACKETS): symbols [ ( { “ « " ' ‘ / ] ) } ” » " ' ’,
/// stack pairing with ambiguous-symbol whitespace/special-case rules,
/// Spanish exceptions (apostrophe postags, Saxon s, degrees º/°, initial »,
/// `)` after a word without opener), bibliographic `)` exceptions, the
/// symmetric-middle case, `a) item` list suppression, opener flagging only
/// when the sentence ends ./?! and move-symbol suggestions. Per-sentence
/// (LT pairs across the whole paragraph — documented divergence for
/// multi-sentence texts).
fn generic_unpaired_brackets_es(sentence: &Sentence, cfg: &UnpairedCfg) -> Vec<Suggestion> {
    const STARTS: [&str; 8] = ["[", "(", "{", "\u{201c}", "\u{ab}", "\"", "'", "\u{2018}"];
    const ENDS: [&str; 8] = ["]", ")", "}", "\u{201d}", "\u{bb}", "\"", "'", "\u{2019}",
    ];
    let tokens: Vec<_> = sentence.iter().collect();
    let text = sentence.text();
    let numerals = |w: &str| -> bool {
        // (?i)\d{1,2}?[a-z']*|M*(D?C{0,3}|C[DM])(L?X{0,3}|X[LC])(V?I{0,3}|I[VX])$
        // evaluated as a full match; ported via the regex crate
        use std::sync::OnceLock;
        use crate::utils::regex::Regex as PortRegex;
        static RE: OnceLock<PortRegex> = OnceLock::new();
        let re = RE.get_or_init(|| PortRegex::new(
            r"(?i)^(?:\d{1,2}?[a-z']*|M*(?:D?C{0,3}|C[DM])(?:L?X{0,3}|X[LC])(?:V?I{0,3}|I[VX]))$".to_string()));
        re.is_match(w)
    };
    let number = |w: &str| -> bool {
        // \d[\d., ]+\d|\d{1,2} full match
        let d = w.chars().all(|c| c.is_ascii_digit());
        if d && w.chars().count() <= 2 && !w.is_empty() {
            return true;
        }
        let cs: Vec<char> = w.chars().collect();
        cs.len() >= 3
            && cs.first().map_or(false, |c| c.is_ascii_digit())
            && cs.last().map_or(false, |c| c.is_ascii_digit())
            && cs[1..cs.len() - 1].iter().all(|c| c.is_ascii_digit() || matches!(c, '.' | ',' | ' '))
    };
    let ascii_punct = |w: &str| -> bool {
        !w.is_empty() && w.chars().all(|c| c.is_ascii_punctuation())
    };
    // [\p{Punct}…–—] full token
    let punctuation = |w: &str| -> bool {
        !w.is_empty()
            && w.chars().all(|c| c.is_ascii_punctuation() || matches!(c, '\u{2026}' | '\u{2013}' | '\u{2014}'))
    };
    // [ldmnstLDMNST]'|[–—\p{Punct}&&[^.]] full token
    let punct_no_dot = |w: &str| -> bool {
        if w.is_empty() {
            return false;
        }
        if matches!(w, "\u{2013}" | "\u{2014}") {
            return true;
        }
        let b = w.as_bytes();
        if b.len() == 2 && b[1] == b'\'' && b"ldmnstLDMNST".contains(&b[0]) {
            return true;
        }
        w.chars().all(|c| c.is_ascii_punctuation() && c != '.')
    };
    // stack entries: (token index, is_opening, symbol)
    let mut stack: Vec<(usize, bool, &'static str)> = Vec::new();
    for m in 0..tokens.len() {
        let w = tokens[m].word().as_str();
        for j in 0..STARTS.len() {
            if w != STARTS[j] && w != ENDS[j] {
                continue;
            }
            let ambiguous = STARTS[j] == ENDS[j];
            let preceded_by_ws = if ambiguous {
                m == 0
                    || tokens[m].has_space_before()
                    || (m > 0 && (punct_no_dot(tokens[m - 1].word().as_str()) || STARTS.contains(&tokens[m - 1].word().as_str())))
            } else {
                true
            };
            let special_case = if ambiguous && m + 1 < tokens.len() {
                let next = tokens[m + 1].word().as_str();
                tokens[m + 1].has_space_before()
                    || punctuation(next)
                    || ENDS.contains(&next)
                    || (m > 0 && tokens[m - 1].word().as_str().ends_with('-'))
                    || next.starts_with('-')
                    || next == "s"
            } else {
                // non-ambiguous end symbols are always closing candidates
                // (LT getSpecialCase only restricts ambiguous symbols)
                !ambiguous || m + 1 >= tokens.len()
            };
            // Spanish isNoException overrides
            let no_exception = {
                let mut ok = true;
                // base class smileys / URL paren
                if m > 0 {
                    let prev = tokens[m - 1].word().as_str();
                    if prev.starts_with("http://") || prev.starts_with("https://") {
                        if prev.contains('(') {
                            ok = false;
                        }
                    }
                    if m >= 2 {
                        let pp = tokens[m - 2].word().as_str();
                        let p = prev;
                        if pp == ":" && p == "-" && (w == ")" || w == "(") {
                            ok = false;
                        }
                        if pp == ";" && p == "-" && (w == ")" || w == "(") {
                            ok = false;
                        }
                    }
                    if (prev == ":" || prev == ";") && !tokens[m].has_space_before() && (w == ")" || w == "(") {
                        ok = false;
                    }
                }
                // apostrophe with noun/adjective postag
                if (w == "'" || w == "\u{2019}")
                    && tokens[m]
                        .word()
                        .tags()
                        .iter()
                        .any(|t| {
                            let p = t.pos().as_str();
                            p.starts_with('N') || p.starts_with('A') || p == "_allow_apostrophe"
                        })
                {
                    ok = false;
                }
                // Saxon genitive 's
                if m + 1 < tokens.len() && (w == "'" || w == "\u{2019}") && tokens[m + 1].word().as_str().eq_ignore_ascii_case("s") {
                    ok = false;
                }
                // degrees/minutes/seconds "12' 34\""
                if (w == "\"" || w == "'")
                    && m > 0
                    && !tokens[m].has_space_before()
                    && number(tokens[m - 1].word().as_str())
                    && ((m > 2 && (tokens[m - 2].word().as_str().contains('\u{ba}') || tokens[m - 2].word().as_str().contains('\u{b0}')))
                        || (m > 4 && (tokens[m - 4].word().as_str().contains('\u{ba}') || tokens[m - 4].word().as_str().contains('\u{b0}'))))
                {
                    ok = false;
                }
                // » as first token
                if m == 0 && w == "\u{bb}" {
                    ok = false;
                }
                // ) after a plain word without an opener
                if m > 0 && w == ")" {
                    let mut has_opener = false;
                    let mut k = 1usize;
                    while m > k {
                        let t = tokens[m - k].word().as_str();
                        if t == ")" {
                            break;
                        }
                        if t == "(" {
                            has_opener = true;
                            break;
                        }
                        k += 1;
                    }
                    if !has_opener {
                        let prev = tokens[m - 1].word().as_str();
                        if prev.chars().all(|c| c.is_ascii_alphabetic())
                            && !prev.is_empty()
                            || prev.chars().all(|c| c.is_ascii_digit()) && !prev.is_empty()
                        {
                            ok = false;
                        }
                    }
                }
                ok
            };
            // opening
            if no_exception && preceded_by_ws && w == STARTS[j] {
                stack.push((m, true, STARTS[j]));
                break;
            }
            // closing (special case or last token of the sentence)
            let is_sentence_end = m + 1 >= tokens.len();
            if no_exception && (special_case || is_sentence_end) && w == ENDS[j] {
                // bibliographic (2014). exception
                let numeral_paren = (m > 1
                    && ENDS[j] == ")"
                    && (m == 3 || tokens[m - 2].has_space_before())
                    && tokens[m - 1].word().as_str() == "."
                    && numerals(tokens[m - 2].word().as_str())
                    && !(stack.last().map_or(false, |(_, _, s)| *s == "(")))
                    || (m > 0
                        && ENDS[j] == ")"
                        && numerals(tokens[m - 1].word().as_str())
                        && !(stack.last().map_or(false, |(_, _, s)| *s == "(")));
                if numeral_paren {
                    break;
                }
                match stack.last() {
                    None => {
                        stack.push((m, false, ENDS[j]));
                        break;
                    }
                    Some((_, _, s)) if *s == STARTS[j] => {
                        stack.pop();
                        break;
                    }
                    Some(_) => {
                        // every es end symbol is unique in ENDS
                        stack.push((m, false, ENDS[j]));
                        break;
                    }
                }
            }
            // pure end symbol not in special position: nothing (kept for the
            // ambiguous fallthrough — LT only handles the branches above)
            break;
        }
    }
    // symmetric odd stack (({"}) — flag only the middle symbol
    let mut flagged: Vec<(usize, bool, &'static str)> = Vec::new();
    if stack.len() > 2 && stack.len() % 2 == 1 {
        let mut symmetric = true;
        let n = stack.len();
        for i in 0..n / 2 {
            let s_start = stack[i].2;
            let s_end = stack[n - 1 - i].2;
            if STARTS.iter().position(|s| *s == s_start) != ENDS.iter().position(|s| *s == s_end) {
                symmetric = false;
                break;
            }
        }
        if symmetric {
            flagged.push(stack[stack.len() / 2]);
        } else {
            flagged.extend(stack.iter().cloned());
        }
    } else {
        flagged.extend(stack.iter().cloned());
    }
    let ends_like_sentence = {
        let t = text.trim_end();
        t.ends_with('.') || t.ends_with('?') || t.ends_with('!')
    };
    let mut out: Vec<Suggestion> = Vec::new();
    // dedup: a closing match right after its opener match removes it
    let mut rule_match_stack: Vec<(usize, &'static str, bool)> = Vec::new(); // (out idx, symbol, is_open)
    for (m, is_open, sym) in flagged {
        if !is_open || ends_like_sentence {
            // list-item suppression: a) / \nb)
            let sb = tokens[m].span().start().byte;
            if sym == ")" && sb >= 1 && sb + 1 < text.len() + 1 {
                let ctx1 = &text[sb.saturating_sub(1)..sb + 1];
                let bad1 = ctx1.len() == 2 && ctx1.chars().next().map_or(false, |c| c.is_ascii_alphabetic());
                let bad2 = sb >= 2 && {
                    let ctx2 = &text[sb - 2..sb + 1];
                    ctx2.starts_with('\n')
                        && ctx2.chars().nth(1).map_or(false, |c| c.is_ascii_alphabetic())
                };
                if bad1 || bad2 {
                    continue;
                }
            }
            // createMatch dedup
            let end_idx = ENDS.iter().position(|s| *s == sym);
            let mut removed = false;
            if let Some(ei) = end_idx {
                if let Some(&(idx, top_sym, _)) = rule_match_stack.last() {
                    if top_sym == STARTS[ei] && idx < out.len() {
                        out.remove(idx);
                        rule_match_stack.pop();
                        // pop indices above idx
                        for e in rule_match_stack.iter_mut() {
                            if e.0 > idx {
                                e.0 -= 1;
                            }
                        }
                        removed = true;
                    }
                }
            }
            if !removed {
                let other = ENDS
                    .iter()
                    .position(|s| *s == sym)
                    .map(|i| STARTS[i])
                    .or_else(|| STARTS.iter().position(|s| *s == sym).map(|i| ENDS[i]))
                    .unwrap_or(sym);
                let repl = if is_open {
                    vec![format!("{}{}", sym, other)]
                } else {
                    vec![format!("{}{}", other, sym)]
                };
                let start = tokens[m].span().start();
                let end = tokens[m].span().end();
                rule_match_stack.push((out.len(), sym, is_open));
                out.push(Suggestion::new(
                    cfg.bracket_id.to_string(),
                    "Unpaired bracket.".to_string(),
                    Span::new(start.byte..end.byte, start.char..end.char),
                    repl,
                ));
            }
        }
    }
    out
}

fn generic_unpaired_quotes(sentence: &Sentence, cfg: &UnpairedCfg) -> Vec<Suggestion> {
    const STARTS: [&str; 4] = ["\u{201c}", "\"", "'", "\u{2018}"]; // “ " ‘
    const ENDS: [&str; 4] = ["\u{201d}", "\"", "'", "\u{2019}"]; //   ” " ’

    let tokens: Vec<_> = sentence.iter().collect();
    let text = sentence.text();
    let is_inch_quote = text.contains('\u{201c}') || text.contains('"') && {
        // .*\d".* over the raw sentence text
        let bytes = text.as_bytes();
        let mut prev_digit = false;
        let mut hit = false;
        for &b in bytes {
            if b == b'"' && prev_digit {
                hit = true;
                break;
            }
            prev_digit = b.is_ascii_digit();
        }
        hit
    };
    let tagged = |i: usize| -> bool {
        tokens[i]
            .word()
            .tags()
            .iter()
            .any(|t| matches!(t.pos().as_str(), "POS" | "NNP" | "_apostrophe_contraction_"))
    };
    // English overrides: tagged => NOT (not-apostrophe) => treated as an
    // apostrophe; untagged => a regular quote symbol
    let is_not_beginning_apostrophe = |i: usize| !tagged(i);
    let is_not_ending_apostrophe = |i: usize| !tagged(i);
    let punctuation_like = |w: &str| -> bool {
        // [\p{Punct}…–—&&[^"'_]]
        w.chars().all(|c| {
            c.is_ascii_punctuation()
                && c != '"'
                && c != '\''
                && c != '_'
                || matches!(c, '…' | '–' | '—')
        }) && !w.is_empty()
    };
    let is_not_quote = |i: usize, j: usize| -> bool {
        // (a) standalone: (sentence start or whitespace before) AND
        //     (last token or whitespace before the next one)
        let before_ok = i == 0 || tokens[i].has_space_before();
        let after_ok = i + 1 >= tokens.len() || tokens[i + 1].has_space_before();
        if before_ok && after_ok {
            return true;
        }
        // (b) punct-quote-punct chains for ambiguous symbols: ("'")
        if ENDS[j] == STARTS[j]
            && i + 1 < tokens.len()
            && i > 0
            && !tokens[i].has_space_before()
            && !tokens[i + 1].has_space_before()
            && punctuation_like(tokens[i - 1].word().as_str())
            && tokens[i + 1].word().as_str() != "."
            && punctuation_like(tokens[i + 1].word().as_str())
        {
            return true;
        }
        false
    };

    let punctuation_marks = |w: &str| matches!(w, "?" | "." | "!" | ",");
    let mut out: Vec<Suggestion> = Vec::new();
    // LT isStartSymbolbefore(tokens, i) — verbatim incl. its quirk that a
    // same-string symbol token leads to the else-branch (return false)
    let is_start_symbol_before = |w: &str, i: usize| -> bool {
        let mut j: isize = i as isize - 1;
        while j > 0 {
            let wj = tokens[j as usize].word().as_str();
            if w != wj && STARTS.contains(&wj) {
                let jm = j as usize;
                return tokens[jm - 1].has_space_before();
            } else {
                return false;
            }
        }
        true
    };
    let mut openers: Vec<(usize, &'static str)> = Vec::new();
    let mut last_apos: Option<&'static str> = None;
    let mut was_inch = false;

    let flag = |out: &mut Vec<Suggestion>, i: usize| {
        let start = tokens[i].span().start();
        let end = tokens[i].span().end();
        out.push(Suggestion::new(
            cfg.quote_id.to_string(),
            "Unpaired quote.".to_string(),
            Span::new(start.byte..end.byte, start.char..end.char),
            Vec::new(),
        ));
    };
    // removeAllOpenInnerQuotes: flag+drop openers strictly after `index`
    let remove_inner =
        |openers: &mut Vec<(usize, &'static str)>, out: &mut Vec<Suggestion>, index: isize| {
            let mut i = openers.len() as isize - 1;
            while i > index {
                let ti = openers[i as usize].0;
                flag(out, ti);
                openers.remove(i as usize);
                i -= 1;
            }
        };

    for i in 0..tokens.len() {
        let w = tokens[i].word().as_str();
        let start_j = STARTS.iter().position(|s| *s == w);
        let end_j = ENDS.iter().position(|s| *s == w);
        let has_open_same = |sym: &str| openers.iter().any(|(_, s)| *s == sym);
        let is_opener = start_j.map_or(false, |j| {
            if is_not_quote(i, j) {
                return false;
            }
            if ENDS.contains(&STARTS[j]) {
                // ambiguous symbol (" or ') — opener iff glued to punct / after
                // sentence start / whitespace / start-symbol-before (LT tail)
                (i == 0)
                    || tokens[i].has_space_before()
                    || (i + 1 < tokens.len()
                        && !tokens[i + 1].has_space_before()
                        && ((!punctuation_marks(tokens[i + 1].word().as_str())
                            && punctuation_like(tokens[i - 1].word().as_str()))
                            || tokens[i - 1].word().as_str().ends_with('-')))
                    || is_start_symbol_before(w, i)
            } else {
                // pure start symbol (\u{201c} / \u{2018}) always opens
                true
            }
        });
        let is_closer = end_j.map_or(false, |j| {
            // LT: isNotQuote && isNotOpenSymbol -> not a closer.
            // isNotOpenSymbol is only restrictive for ambiguous symbols.
            let not_open_symbol =
                !(ENDS[j] == STARTS[j] && has_open_same(ENDS[j]));
            !(is_not_quote(i, j) && not_open_symbol)
        });
        if is_opener {
            let j = start_j.unwrap();
            if !is_not_beginning_apostrophe(i) {
                last_apos = Some(STARTS[j]);
                continue;
            }
            if STARTS[j] == "\"" {
                was_inch = false;
            }
            if last_apos == Some(STARTS[j]) {
                last_apos = None;
            }
            if let Some(pos) = openers.iter().position(|(_, s)| *s == STARTS[j]) {
                remove_inner(&mut openers, &mut out, pos as isize - 1);
            }
            openers.push((i, STARTS[j]));
        } else if is_closer {
            let j = end_j.unwrap();
            if !is_not_beginning_apostrophe(i) {
                last_apos = Some(ENDS[j]);
                continue;
            }
            let is_inch_symb = ENDS[j] == "\"";
            let is_inch = is_inch_symb && is_inch_quote;
            let start_symbol = STARTS[j];
            if let Some(pos) = openers.iter().position(|(_, s)| *s == start_symbol) {
                remove_inner(&mut openers, &mut out, pos as isize);
                openers.remove(pos);
                if last_apos == Some(start_symbol) {
                    last_apos = None;
                }
                if is_inch {
                    was_inch = true;
                }
            } else if is_not_ending_apostrophe(i) {
                if !is_inch && (!is_inch_symb || !was_inch) {
                    if last_apos != Some(ENDS[j]) {
                        flag(&mut out, i);
                    } else {
                        last_apos = None;
                    }
                } else {
                    was_inch = false;
                }
            }
        }
    }
    remove_inner(&mut openers, &mut out, -1);
    out
}

pub(crate) fn unpaired_brackets(sentence: &Sentence, lang: Option<&str>) -> Vec<Suggestion> {
    let cfg = match lang.map_or_else(|| None, |l| text_cfg(l)) {
        Some(c) => c,
        None => return Vec::new(),
    };
    let cfg = match cfg.unp {
        Some(u) => u,
        None => return Vec::new(),
    };
    if lang == Some("es") {
        // es uses SpanishUnpairedBracketsRule (GenericUnpairedBracketsRule
        // subclass) for ALL symbols incl. quotes and guillemets
        return generic_unpaired_brackets_es(sentence, &cfg);
    }

    let tokens: Vec<_> = sentence.iter().collect();
    // byte gap between token i and i+1 is empty => glued
    let glued_left = |i: usize| -> bool {
        i > 0 && tokens[i].span().start().byte == tokens[i - 1].span().end().byte
    };
    let glued_right = |i: usize| -> bool {
        i + 1 < tokens.len() && tokens[i + 1].span().start().byte == tokens[i].span().end().byte
    };

    let mut out = Vec::new();
    // bracket stack: (token index, closer)
    let mut stack: Vec<(usize, char)> = Vec::new();
    // straight quote pairing: even occurrence closes, odd opens
    let mut quote_open: Option<usize> = None;

    for (i, token) in tokens.iter().enumerate() {
        let word = token.word().as_str();
        for (pos, c) in word.char_indices() {
            // a char is "glued" when attached to word content in the same
            // token, or to the neighbouring token without whitespace
            let at_start = pos == 0;
            let at_end = pos + c.len_utf8() >= word.len();
            let glue_open = !at_end || glued_right(i);
            let glue_close = !at_start || glued_left(i);
            let glue_either = glue_open || glue_close;
            let mut flag = |id: &'static str, msg: &'static str| {
                let start = token.span().start();
                let end = token.span().end();
                out.push(Suggestion::new(
                    id.to_string(),
                    msg.to_string(),
                    Span::new(start.byte..end.byte, start.char..end.char),
                    Vec::new(),
                ));
            };
            if cfg.brackets {
                match c {
                    '(' if glue_open || !cfg.glued_only => stack.push((i, ')')),
                    '[' if glue_open || !cfg.glued_only => stack.push((i, ']')),
                    '{' if glue_open || !cfg.glued_only => stack.push((i, '}')),
                    // LT's base GenericUnpairedBracketsRule pairs typographic
                    // quotes too (gl “Bo día” must flag the unmatched opener)
                    '\u{201c}' if glue_open || !cfg.glued_only => stack.push((i, '\u{201d}')),
                    '\u{2018}' if glue_open || !cfg.glued_only => stack.push((i, '\u{2019}')),
                    ')' | ']' | '}' | '\u{201d}' | '\u{2019}' if glue_close || !cfg.glued_only => {
                        // pop the matching opener; unmatched closer flags here
                        if let Some(p) = stack.iter().rposition(|(_, cl)| *cl == c) {
                            stack.remove(p);
                        } else {
                            flag(cfg.bracket_id, "Unpaired closing bracket.");
                        }
                    }
                    _ => {}
                }
            }
            if cfg.quotes && !cfg.lt_generic_quotes && c == '"' && (glue_either || !cfg.glued_only) {
                match quote_open.take() {
                    Some(_) => {}
                    None => quote_open = Some(i),
                }
            }
            if cfg.guillemets {
                match c {
                    '«' if glue_open || !cfg.glued_only => stack.push((i, '»')),
                    '»' if glue_close || !cfg.glued_only => {
                        if let Some(p) = stack.iter().rposition(|(_, cl)| *cl == c) {
                            stack.remove(p);
                        } else {
                            flag(cfg.bracket_id, "Unpaired closing guillemet.");
                        }
                    }
                    _ => {}
                }
            }
        }
    }
    // unmatched openers remain on the stack / in quote_open
    let mut bracket_offenders: Vec<usize> = stack.iter().map(|(i, _)| *i).collect();
    bracket_offenders.sort_unstable();
    bracket_offenders.dedup();
    for i in bracket_offenders {
        let start = tokens[i].span().start();
        let end = tokens[i].span().end();
        out.push(Suggestion::new(
            cfg.bracket_id.to_string(),
            "Unpaired opening bracket.".to_string(),
            Span::new(start.byte..end.byte, start.char..end.char),
            Vec::new(),
        ));
    }
    if cfg.lt_generic_quotes {
        // quotes handled by the GenericUnpairedQuotesRule replication;
        // brackets/guillemets stay on the char-level path above
        let mut quotes = generic_unpaired_quotes(sentence, &cfg);
        quotes.extend(out);
        return quotes;
    }
    if let Some(i) = quote_open {
        let start = tokens[i].span().start();
        let end = tokens[i].span().end();
        out.push(Suggestion::new(
            cfg.quote_id.to_string(),
            "Unpaired opening quote.".to_string(),
            Span::new(start.byte..end.byte, start.char..end.char),
            Vec::new(),
        ));
    }
    out
}