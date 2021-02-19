use lazycell::AtomicLazyCell;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::hash::{Hash, Hasher};

pub use regex_impl::{CaptureMatches, Captures, Match, Matches};

#[derive(Debug)]
pub struct Regex {
    regex_str: String,
    regex: AtomicLazyCell<regex_impl::Regex>,
}

impl Serialize for Regex {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.regex_str)
    }
}

impl<'de> Deserialize<'de> for Regex {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let regex_str = String::deserialize(deserializer)?;
        Ok(Regex::new(regex_str))
    }
}

impl Hash for Regex {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.regex_str.hash(state);
    }
}

impl Regex {
    /// Create a new regex from the pattern string.
    ///
    /// Note that the regex compilation happens on first use, which is why this method does not
    /// return a result.
    pub fn new(regex_str: String) -> Self {
        Self {
            regex_str,
            regex: AtomicLazyCell::new(),
        }
    }

    /// Check whether the pattern compiles as a valid regex.
    pub fn try_compile(&self) -> Result<(), Box<dyn std::error::Error + Send + Sync + 'static>> {
        regex_impl::Regex::new(&self.regex_str).map(|_| ())
    }

    fn regex(&self) -> &regex_impl::Regex {
        if let Some(regex) = self.regex.borrow() {
            regex
        } else {
            let regex =
                regex_impl::Regex::new(&self.regex_str).expect("regex string should be pre-tested");
            self.regex.fill(regex).ok();
            self.regex.borrow().unwrap()
        }
    }

    pub fn is_match(&self, text: &str) -> bool {
        self.regex().is_match(text)
    }

    pub fn captures_iter<'r, 't>(&'r self, text: &'t str) -> regex_impl::CaptureMatches<'r, 't> {
        self.regex().captures_iter(text)
    }

    pub fn find_iter<'r, 't>(&'r self, text: &'t str) -> regex_impl::Matches<'r, 't> {
        self.regex().find_iter(text)
    }

    pub fn captures_len(&self) -> usize {
        self.regex().captures_len()
    }

    pub fn captures<'t>(&self, text: &'t str) -> Option<regex_impl::Captures<'t>> {
        self.regex().captures(text)
    }

    pub fn replace_all(&self, text: &str, replacement: &str) -> String {
        self.regex().replace_all(text, replacement)
    }
}

mod regex_impl {
    pub use fancy_regex::{Captures, Match};
    use std::error::Error;
    pub struct Matches<'r, 't>(fancy_regex::Matches<'r, 't>);

    impl<'r, 't> Iterator for Matches<'r, 't> {
        type Item = Match<'t>;

        fn next(&mut self) -> Option<Self::Item> {
            match self.0.next() {
                Some(Ok(mat)) => Some(mat),
                // stop if an error is encountered
                None | Some(Err(_)) => None,
            }
        }
    }

    pub struct CaptureMatches<'r, 't>(fancy_regex::CaptureMatches<'r, 't>);

    impl<'r, 't> Iterator for CaptureMatches<'r, 't> {
        type Item = Captures<'t>;

        fn next(&mut self) -> Option<Self::Item> {
            match self.0.next() {
                Some(Ok(caps)) => Some(caps),
                // stop if an error is encountered
                None | Some(Err(_)) => None,
            }
        }
    }

    #[derive(Debug)]
    pub struct Regex {
        regex: fancy_regex::Regex,
    }

    impl Regex {
        pub fn new(regex_str: &str) -> Result<Self, Box<dyn Error + Send + Sync + 'static>> {
            Ok(Regex {
                regex: fancy_regex::Regex::new(regex_str)?,
            })
        }

        pub fn is_match(&self, text: &str) -> bool {
            // errors are treated as non-matches
            self.regex.is_match(text).unwrap_or(false)
        }

        pub fn captures_iter<'r, 't>(&'r self, text: &'t str) -> CaptureMatches<'r, 't> {
            CaptureMatches(self.regex.captures_iter(text))
        }

        pub fn find_iter<'r, 't>(&'r self, text: &'t str) -> Matches<'r, 't> {
            Matches(self.regex.find_iter(text))
        }

        pub fn captures_len(&self) -> usize {
            self.regex.captures_len()
        }

        pub fn captures<'t>(&self, text: &'t str) -> Option<Captures<'t>> {
            match self.regex.captures(text) {
                Ok(Some(captures)) => Some(captures),
                // errors treated as not matching
                Ok(None) | Err(_) => None,
            }
        }

        pub fn replace_all<'t>(&self, text: &'t str, replacement: &str) -> String {
            let mut index = 0;
            let mut out: Vec<String> = Vec::new();

            for captures in self.captures_iter(text) {
                let mat = captures.get(0).expect("0th capture group exists");

                out.push(text[index..mat.start()].to_string());

                let mut replacement = replacement.to_string();
                for i in 1..captures.len() {
                    replacement = replacement.replace(
                        &format!("${}", i),
                        captures.get(i).map_or("", |x| x.as_str()),
                    );
                }

                out.push(replacement);
                index = mat.end();
            }

            if index != text.len() {
                out.push(text[index..].to_string());
            }

            out.join("")
        }
    }
}

#[cfg(feature = "")]
mod regex_impl {
    use std::error::Error;

    pub struct CaptureMatches<'r, 't>(onig::FindCaptures<'r, 't>, &'t str);

    pub struct Captures<'t>(onig::Captures<'t>, &'t str);

    pub struct Matches<'r, 't>(onig::FindMatches<'r, 't>, &'t str);

    pub struct Match<'t> {
        text: &'t str,
        start: usize,
        end: usize,
    }

    impl<'t> Match<'t> {
        pub fn start(&self) -> usize {
            self.start
        }

        pub fn end(&self) -> usize {
            self.end
        }

        pub fn as_str(&self) -> &'t str {
            self.text
        }
    }

    impl<'t> Captures<'t> {
        pub fn get(&self, index: usize) -> Option<Match<'t>> {
            let (start, end) = self.0.pos(index)?;
            let text = self.0.at(index)?;

            Some(Match { text, start, end })
        }

        pub fn iter(&'t self) -> impl Iterator<Item = Option<Match<'t>>> {
            self.0.iter_pos().map(move |mat| {
                mat.map(|(start, end)| Match {
                    text: &self.1[start..end],
                    start,
                    end,
                })
            })
        }
    }

    impl<'r, 't> Iterator for CaptureMatches<'r, 't> {
        type Item = Captures<'t>;

        fn next(&mut self) -> Option<Self::Item> {
            self.0.next().map(|x| Captures(x, self.1))
        }
    }

    impl<'r, 't> Iterator for Matches<'r, 't> {
        type Item = Match<'t>;

        fn next(&mut self) -> Option<Self::Item> {
            self.0.next().map(|(start, end)| Match {
                text: &self.1[start..end],
                start,
                end,
            })
        }
    }

    #[derive(Debug)]
    pub struct Regex {
        regex: onig::Regex,
    }

    impl Regex {
        pub fn new(regex_str: &str) -> Result<Self, Box<dyn Error + Send + Sync + 'static>> {
            let mut case_sensitive = true;
            let regex_str = if let Some(stripped) = regex_str.strip_suffix("(?i)") {
                case_sensitive = false;
                stripped
            } else {
                regex_str
            };

            let regex = onig::Regex::with_options(
                regex_str,
                onig::RegexOptions::REGEX_OPTION_CAPTURE_GROUP
                    | if case_sensitive {
                        onig::RegexOptions::REGEX_OPTION_NONE
                    } else {
                        onig::RegexOptions::REGEX_OPTION_IGNORECASE
                    },
                onig::Syntax::default(),
            )?;

            Ok(Regex { regex })
        }

        pub fn is_match(&self, text: &str) -> bool {
            self.regex.is_match(text)
        }

        pub fn captures_iter<'r, 't>(&'r self, text: &'t str) -> CaptureMatches<'r, 't> {
            CaptureMatches(self.regex.captures_iter(text), text)
        }

        pub fn find_iter<'r, 't>(&'r self, text: &'t str) -> Matches<'r, 't> {
            Matches(self.regex.find_iter(text), text)
        }

        pub fn captures_len(&self) -> usize {
            self.regex.captures_len()
        }

        pub fn captures<'t>(&self, text: &'t str) -> Option<Captures<'t>> {
            self.regex.captures(text).map(|x| Captures(x, text))
        }

        pub fn replace_all(&self, text: &str, replacement: &str) -> String {
            self.regex.replace_all(&text, |caps: &onig::Captures| {
                let mut replacement = replacement.to_owned();

                for i in 1..caps.len() {
                    replacement = replacement.replace(&format!("${}", i), caps.at(i).unwrap_or(""));
                }

                replacement
            })
        }
    }
}
