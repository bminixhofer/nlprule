use lazy_static::lazy_static;
use once_cell::sync::OnceCell;
use onig::{Captures, Regex};
use serde::{Deserialize, Serialize};

pub mod parallelism;
pub mod regex;

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct CacheString<S: AsRef<str>> {
    string: S,
    #[serde(skip)]
    lower: OnceCell<String>,
}

impl<S: AsRef<str>> PartialEq for CacheString<S> {
    fn eq(&self, other: &Self) -> bool {
        other.as_str() == self.as_str()
    }
}

impl<S: AsRef<str>> From<S> for CacheString<S> {
    fn from(string: S) -> Self {
        CacheString {
            lower: OnceCell::new(),
            string,
        }
    }
}

impl<S: AsRef<str>> CacheString<S> {
    pub fn to_lowercase(&self) -> &str {
        self.lower
            .get_or_init(|| self.string.as_ref().to_lowercase())
            .as_str()
    }

    pub fn as_str(&self) -> &str {
        self.string.as_ref()
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
