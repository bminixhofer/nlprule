use crate::Error;
use onig::{Regex, RegexOptions};
use serde::{Deserialize, Serialize};
use std::hash::{Hash, Hasher};
use std::ops::Deref;

fn unescape<S: AsRef<str>>(string: S, c: &str) -> String {
    let placeholder = "###escaped_backslash###";

    string
        .as_ref()
        .replace(r"\\", placeholder)
        .replace(&format!(r"\{}", c), c)
        .replace(placeholder, r"\\")
}

#[derive(Serialize, Deserialize)]
struct RegexFields {
    regex_str: String,
    case_sensitive: bool,
}

impl From<RegexFields> for SerializeRegex {
    fn from(data: RegexFields) -> Self {
        SerializeRegex {
            regex: SerializeRegex::compile(&data.regex_str, data.case_sensitive).unwrap(),
            regex_str: data.regex_str,
            case_sensitive: data.case_sensitive,
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(from = "RegexFields")]
pub struct SerializeRegex {
    regex_str: String,
    case_sensitive: bool,
    #[serde(skip_serializing)]
    regex: Regex,
}

impl Hash for SerializeRegex {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.regex_str.hash(state);
        self.case_sensitive.hash(state);
    }
}

impl SerializeRegex {
    fn compile(regex_str: &str, case_sensitive: bool) -> Result<Regex, onig::Error> {
        Regex::with_options(
            regex_str,
            if case_sensitive {
                RegexOptions::REGEX_OPTION_NONE
            } else {
                RegexOptions::REGEX_OPTION_IGNORECASE
            },
            onig::Syntax::java(),
        )
    }

    pub fn new(
        regex_str: &str,
        must_fully_match: bool,
        case_sensitive: bool,
    ) -> Result<Self, Error> {
        // TODO: more exhaustive backslash check
        let mut fixed = unescape(unescape(unescape(regex_str, "!"), ","), "/");
        let mut case_sensitive = case_sensitive;

        fixed = fixed
            .replace("\\\\s", "###backslash_before_s###")
            .replace("\\$", "###escaped_dollar###")
            // apparently \s in Java regexes only matches an actual space, not e.g non-breaking space
            .replace("\\s", " ")
            .replace("$+", "$")
            .replace("$?", "$")
            .replace("$*", "$")
            .replace("###escaped_dollar###", "\\$")
            .replace("###backslash_before_s###", "\\\\s");

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

        Ok(SerializeRegex {
            regex: SerializeRegex::compile(&fixed, case_sensitive)
                .map_err(|x| Error::Unexpected(format!("{}", x)))?,
            regex_str: fixed,
            case_sensitive,
        })
    }
}

impl Deref for SerializeRegex {
    type Target = Regex;

    fn deref(&self) -> &Self::Target {
        &self.regex
    }
}
