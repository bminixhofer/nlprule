use lazy_static::lazy_static;
use onig::{Captures, Regex, RegexOptions};

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

pub fn new_regex(regex: &str, must_fully_match: bool, case_sensitive: bool) -> Regex {
    // TODO: more exhaustive backslash check
    let mut fixed = unescape(unescape(unescape(regex, "!"), ","), "/");
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

    Regex::with_options(
        &fixed,
        if case_sensitive {
            RegexOptions::REGEX_OPTION_NONE
        } else {
            RegexOptions::REGEX_OPTION_IGNORECASE
        },
        onig::Syntax::java(),
    )
    .unwrap()
}
