//! Rule-based grammatical error correction through parsing LanguageTool rules.
//! # Overview
//!
//! NLPRule has the following core abstractions:
//! - A [Tokenizer][tokenizer::Tokenizer] to split a text into tokens and analyze it by chunking, lemmatizing and part-of-speech tagging. Can also be used independently of the grammatical rules.
//! - A [Rules][rules::Rules] structure containing a set of grammatical error correction rules.
//!
//! # Example: correct a text
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
//! # Ok::<(), bincode::Error>(())
//! ```
//!
//! # Example: get suggestions and correct a text
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
//! assert_eq!(
//!     suggestions,
//!     vec![Suggestion {
//!         start: 4, // these are character indices!
//!         end: 16,
//!         replacements: vec!["was not".into(), "has not been".into()],
//!         source: "WAS_BEEN.1".into(),
//!         message: "Did you mean was not or has not been?".into()
//!     }]
//! );
//!
//! let corrected = apply_suggestions(text, &suggestions);
//!
//! assert_eq!(corrected, "She was not here since Monday.");
//! # Ok::<(), bincode::Error>(())
//! ```
//!
//! Binaries are distributed with [Github releases](https://github.com/bminixhofer/nlprule/releases).
//!
//! # The 't lifetime
//! By convention the lifetime `'t` in this crate is the lifetime of the input text.
//! Almost all structures with a lifetime are bound to this lifetime.

use std::io;

use thiserror::Error;

#[cfg(feature = "compile")]
pub mod compile;
mod filter;
pub mod rule;
pub mod rules;
pub mod tokenizer;
pub mod types;
pub(crate) mod utils;

pub use rules::Rules;
pub use tokenizer::Tokenizer;

#[derive(Error, Debug)]
pub enum Error {
    #[error("i/o error: {0}")]
    Io(#[from] io::Error),
    #[error("deserialization error: {0}")]
    Deserialization(#[from] bincode::Error),
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
