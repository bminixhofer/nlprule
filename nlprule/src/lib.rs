//! Rule-based grammatical error correction through parsing LanguageTool rules.
//! # Overview
//!
//! nlprule has the following core abstractions:
//! - A [Tokenizer][tokenizer::Tokenizer] to split a text into tokens and analyze it by chunking, lemmatizing and part-of-speech tagging. Can also be used independently of the grammatical rules.
//! - A [Rules][rules::Rules] structure containing a set of grammatical error correction rules.
//!
//! # Examples
//!
//! Correct a text:
//!
//! ```no_run
//! use nlprule::{Tokenizer, Rules};
//!
//! let tokenizer = Tokenizer::new("path/to/en_tokenizer.bin")?;
//! let rules = Rules::new("path/to/en_rules.bin")?;
//!
//! assert_eq!(
//!     rules.correct("She was not been here since Monday.", &tokenizer),
//!     String::from("She was not here since Monday.")
//! );
//! # Ok::<(), nlprule::Error>(())
//! ```
//!
//! Get suggestions and correct a text:
//!
//! ```no_run
//! use nlprule::{Tokenizer, Rules, types::Suggestion, rules::apply_suggestions};
//!
//! let tokenizer = Tokenizer::new("path/to/en_tokenizer.bin")?;
//! let rules = Rules::new("path/to/en_rules.bin")?;
//!
//! let text = "She was not been here since Monday.";
//!
//! let suggestions = rules.suggest(text, &tokenizer);
//! assert_eq!(*suggestions[0].span().char(), 4usize..16);
//! assert_eq!(suggestions[0].replacements(), vec!["was not", "has not been"]);
//! assert_eq!(suggestions[0].source(), "GRAMMAR/WAS_BEEN/1");
//! assert_eq!(suggestions[0].message(), "Did you mean was not or has not been?");
//!
//! let corrected = apply_suggestions(text, &suggestions);
//!
//! assert_eq!(corrected, "She was not here since Monday.");
//! # Ok::<(), nlprule::Error>(())
//! ```
//!
//! Tokenize & analyze a text:
//!
//! ```no_run
//! use nlprule::Tokenizer;
//!
//! let tokenizer = Tokenizer::new("path/to/en_tokenizer.bin")?;
//!
//! let text = "A brief example is shown.";
//!
//! // returns an iterator over sentences
//! let sentence = tokenizer.pipe(text).next().expect("`text` contains one sentence.");
//!
//! println!("{:#?}", sentence);
//! assert_eq!(sentence.tokens()[1].word().text().as_str(), "brief");
//! assert_eq!(sentence.tokens()[1].word().tags()[0].pos().as_str(), "JJ");
//! assert_eq!(sentence.tokens()[1].chunks(), vec!["I-NP-singular"]);
//! // some other information like char / byte span, lemmas etc. is also set!
//! # Ok::<(), nlprule::Error>(())
//! ```
//! ---
//! Binaries are distributed with [Github releases](https://github.com/bminixhofer/nlprule/releases).

// #![warn(missing_docs)]
use std::io;

use thiserror::Error;

#[cfg(feature = "compile")]
pub mod compile;
mod filter;
pub mod properties;
pub mod rule;
pub mod rules;
pub mod tokenizer;
pub mod types;
pub(crate) mod utils;

pub use rules::Rules;
pub use tokenizer::Tokenizer;

#[derive(Error, Debug)]
#[allow(missing_docs)]
pub enum Error {
    #[error(transparent)]
    Io(#[from] io::Error),
    /// (De)serialization error. Can have occured during deserialization or during serialization.
    #[error(transparent)]
    Serialization(#[from] bincode::Error),
    #[error(transparent)]
    IdError(#[from] rule::id::Error),
    #[error("unset token property: {0}")]
    Unset(&'static str),
}

/// Gets the canonical filename for the tokenizer binary for a language code in ISO 639-1 (two-letter) format.
pub fn tokenizer_filename(lang_code: &str) -> String {
    format!("{}_tokenizer.bin", lang_code)
}

/// Gets the canonical filename for the rules binary for a language code in ISO 639-1 (two-letter) format.
pub fn rules_filename(lang_code: &str) -> String {
    format!("{}_rules.bin", lang_code)
}

/// Gets the canonical filename for the tokenizer binary for a language code in ISO 639-1 (two-letter) format.
#[macro_export]
macro_rules! tokenizer_filename {
    ($lang_code:literal) => {
        concat!($lang_code, "_tokenizer.bin")
    };
}

/// Gets the canonical filename for the rules binary for a language code in ISO 639-1 (two-letter) format.
#[macro_export]
macro_rules! rules_filename {
    ($lang_code:literal) => {
        concat!($lang_code, "_rules.bin")
    };
}
