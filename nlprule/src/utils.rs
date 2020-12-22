use lazy_static::lazy_static;
use onig::{Captures, Regex, RegexOptions};
use serde::{Deserialize, Deserializer, Serialize};
use std::ops::Deref;

#[derive(Serialize, Deserialize)]
struct RegexFields {
    regex_str: String,
    case_sensitive: bool,
}

#[derive(Serialize, Debug)]
pub struct SerializeRegex {
    regex_str: String,
    case_sensitive: bool,
    #[serde(skip_serializing)]
    regex: Regex,
}

impl<'de> Deserialize<'de> for SerializeRegex {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let fields: RegexFields = Deserialize::deserialize(deserializer)?;
        Ok(SerializeRegex {
            regex: SerializeRegex::compile(&fields.regex_str, fields.case_sensitive),
            regex_str: fields.regex_str,
            case_sensitive: fields.case_sensitive,
        })
    }
}

impl SerializeRegex {
    fn compile(regex_str: &str, case_sensitive: bool) -> Regex {
        Regex::with_options(
            regex_str,
            if case_sensitive {
                RegexOptions::REGEX_OPTION_NONE
            } else {
                RegexOptions::REGEX_OPTION_IGNORECASE
            },
            onig::Syntax::java(),
        )
        .unwrap()
    }

    pub fn new(regex_str: &str, must_fully_match: bool, case_sensitive: bool) -> Self {
        // TODO: more exhaustive backslash check
        let mut fixed = unescape(unescape(unescape(regex_str, "!"), ","), "/");
        let mut case_sensitive = case_sensitive;

        for pattern in &["(?iu)", "(?i)"] {
            if fixed.contains(pattern) {
                case_sensitive = false;
                fixed = fixed.replace(pattern, "");
            }
        }

        let fixed = if must_fully_match {
            format!("^({})$", fixed)
        } else {
            fixed
        };

        SerializeRegex {
            regex: SerializeRegex::compile(&fixed, case_sensitive),
            regex_str: fixed,
            case_sensitive,
        }
    }
}

impl Deref for SerializeRegex {
    type Target = Regex;

    fn deref(&self) -> &Self::Target {
        &self.regex
    }
}

// see https://stackoverflow.com/questions/38406793/why-is-capitalizing-the-first-letter-of-a-string-so-convoluted-in-rust
pub fn apply_to_first<F>(string: &str, func: F) -> String
where
    F: Fn(char) -> String,
{
    let mut c = string.chars();
    match c.next() {
        None => String::new(),
        Some(first) => func(first) + c.as_str(),
    }
}

pub fn is_title_case(string: &str) -> bool {
    let mut char_case = string.chars().map(|x| x.is_uppercase());

    char_case.next().unwrap_or(false) && !char_case.any(|x| x)
}

pub fn is_uppercase(string: &str) -> bool {
    !string.chars().any(|x| x.is_lowercase())
}

// see https://github.com/rust-onig/rust-onig/issues/59#issuecomment-340160520
pub fn dollar_replace(mut replacement: String, caps: &Captures) -> String {
    for i in 1..caps.len() {
        replacement = replacement.replace(&format!("${}", i), caps.at(i).unwrap_or(""));
    }
    replacement
}

// remove duplicate whitespaces
pub fn normalize_whitespace(string: &str) -> String {
    lazy_static! {
        static ref REGEX: Regex = Regex::new(r"(\s)\s+").unwrap();
    }

    REGEX.replace_all(string, |caps: &Captures| caps.at(1).unwrap().to_string())
}

pub fn unescape<S: AsRef<str>>(string: S, c: &str) -> String {
    let placeholder = "###escaped_backslash###";

    string
        .as_ref()
        .replace(r"\\", placeholder)
        .replace(&format!(r"\{}", c), c)
        .replace(placeholder, r"\\")
}

#[inline]
pub fn splitting_chars() -> &'static str {
    r##"«»'’`´‘],.:;!?/\()<=>„“”"+#…*"##
}

#[inline]
pub fn no_space_chars() -> &'static str {
    r##","##
}

pub fn fix_nospace_chars(text: &str) -> String {
    text.char_indices()
        .filter(|(i, c)| {
            if c.is_whitespace() {
                !no_space_chars()
                    .chars()
                    .any(|nospace_c| text[(i + c.len_utf8())..].starts_with(nospace_c))
            } else {
                true
            }
        })
        .map(|x| x.1)
        .collect()
}
