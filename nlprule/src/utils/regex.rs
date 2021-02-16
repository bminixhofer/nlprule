use onig::{Regex, RegexOptions};
use serde::{Deserialize, Serialize};
use std::hash::{Hash, Hasher};
use std::ops::Deref;

#[derive(Serialize, Deserialize, Debug)]
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
    pub(crate) regex_str: String,
    pub(crate) case_sensitive: bool,
    #[serde(skip_serializing)]
    pub(crate) regex: Regex,
}

impl Hash for SerializeRegex {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.regex_str.hash(state);
        self.case_sensitive.hash(state);
    }
}

impl SerializeRegex {
    pub fn compile(regex_str: &str, case_sensitive: bool) -> Result<Regex, onig::Error> {
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
}

impl Deref for SerializeRegex {
    type Target = Regex;

    fn deref(&self) -> &Self::Target {
        &self.regex
    }
}
