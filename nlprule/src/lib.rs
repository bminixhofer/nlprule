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
#![cfg_attr(docsrs, feature(doc_cfg))] // see https://stackoverflow.com/a/61417700

pub use nlprule_core::*;

/// Creates a tokenizer from an ISO 639-1 (two-letter) language code by inlining the binary at compile time.
///
/// # Example
/// ```
/// # use nlprule::{Tokenizer, tokenizer};
/// let tokenizer: Tokenizer = tokenizer!("en");
/// ```
// NB: making this a trait `FromLangCode` would be nice but is blocked by https://github.com/rust-lang/rust/issues/53749
#[macro_export]
#[cfg_attr(docsrs, doc(cfg(feature = "binaries")))]
#[cfg(feature = "binaries")]
macro_rules! tokenizer {
    ($lang_code:literal) => {{
        let bytes = include_bytes!(concat!(env!("OUT_DIR"), "/", $lang_code, "_tokenizer.bin"));

        $crate::Tokenizer::from_reader(std::io::Cursor::new(bytes))
            .expect("tokenizer bytes must decode to valid tokenizer")
    }};
}

/// Creates a rules set from an ISO 639-1 (two-letter) language code by inlining the binary at compile time.
///
/// # Example
/// ```
/// # use nlprule::{Rules, rules};
/// let rules: Rules = rules!("en");
/// ```
#[macro_export]
#[cfg_attr(docsrs, doc(cfg(feature = "binaries")))]
#[cfg(feature = "binaries")]
macro_rules! rules {
    ($lang_code:literal) => {{
        let bytes = include_bytes!(concat!(env!("OUT_DIR"), "/", $lang_code, "_rules.bin"));

        $crate::Rules::from_reader(std::io::Cursor::new(bytes))
            .expect("rules bytes must decode to valid rules")
    }};
}

#[cfg(test)]
mod tests {
    use super::{rules, tokenizer};

    #[test]
    fn tokenizer_from_lang_code_works() {
        let _tokenizer = tokenizer!("en");
    }

    #[test]
    fn rules_from_lang_code_works() {
        let _rules = rules!("en");
    }
}
