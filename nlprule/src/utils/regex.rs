use onig::{Regex, RegexOptions};
use serde::{Deserialize, Serialize, Serializer};
use std::ops::Deref;
use std::{
    convert::TryFrom,
    hash::{Hash, Hasher},
};

impl TryFrom<String> for SerializeRegex {
    type Error = onig::Error;

    fn try_from(string: String) -> Result<Self, onig::Error> {
        Ok(SerializeRegex {
            regex: SerializeRegex::compile(&string)?,
            string,
        })
    }
}

#[derive(Deserialize, Debug)]
#[serde(try_from = "String")]
pub struct SerializeRegex {
    pub(crate) string: String,
    pub(crate) regex: Regex,
}

impl Serialize for SerializeRegex {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.string)
    }
}

impl Hash for SerializeRegex {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.string.hash(state);
    }
}

impl SerializeRegex {
    pub fn compile(regex_str: &str) -> Result<Regex, onig::Error> {
        let mut case_sensitive = true;
        let regex_str = if let Some(stripped) = regex_str.strip_suffix("(?i)") {
            case_sensitive = false;
            stripped
        } else {
            regex_str
        };

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
