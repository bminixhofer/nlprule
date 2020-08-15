use lazy_static::lazy_static;
use regex::Regex;

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

// remove duplicate whitespaces
pub fn normalize_whitespace(string: &str) -> String {
    lazy_static! {
        static ref REGEX: Regex = Regex::new(r"(\s)\s+").unwrap();
    }

    REGEX.replace_all(string, r"$1").to_string()
}

pub fn fix_regex_replacement(replacement: &str) -> String {
    lazy_static! {
        static ref REGEX: Regex = Regex::new(r"\$(\d)").unwrap();
    }

    REGEX.replace_all(replacement, r"${${1}}").to_string()
}

pub fn fix_regex(regex: &str, must_fully_match: bool) -> String {
    // TODO: more exhaustive backslash check
    let fixed = regex
        .replace(r"\!", "!")
        .replace(r"\,", ",")
        .replace(r"\/", "/");

    if must_fully_match {
        format!("^({})$", fixed)
    } else {
        fixed
    }
}
