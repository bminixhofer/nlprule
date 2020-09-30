use lazy_static::lazy_static;
use onig::{Regex, RegexOptions};

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

    REGEX.replace_all(string, r"$1")
}

pub fn fix_regex_replacement(replacement: &str) -> String {
    lazy_static! {
        static ref REGEX: Regex = Regex::new(r"\$(\d)").unwrap();
    }

    REGEX.replace_all(replacement, r"${${1}}")
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
