//! The NLPRule core.
//! # Overview
//!
//! NLPRule has the following core abstractions:
//! - A [Tokenizer][tokenizer::Tokenizer] to split a text into tokens and analyze it.
//! - A [Rules][rules::Rules] structure containing a set of grammatical error correction rules.
//!
//! # Example: get suggestions and correct a text
//!
//! ```no_run
//! use nlprule::types::*;
//! use nlprule::{tokenizer::{Tokenizer, finalize}, rules::{Rules, correct}};
//!
//! let tokenizer = Tokenizer::new("path/to/en_tokenizer.bin")?;
//! let rules = Rules::new("path/to/en_rules.bin")?;
//!
//! let text = "She was not been here since Monday.";
//!
//! let tokens = tokenizer.disambiguate(tokenizer.tokenize(text));
//! let suggestions = rules.suggest(&finalize(tokens), &tokenizer);
//! assert_eq!(
//!     suggestions,
//!     vec![Suggestion {
//!         start: 4, // these are character indices!
//!         end: 16,
//!         text: vec!["was not".into(), "has not been".into()],
//!         source: "WAS_BEEN.1".into(),
//!         message: "Did you mean was not or has not been?".into()
//!     }]
//! );
//!
//! let corrected = correct(text, &suggestions);
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
use thiserror::Error;

#[cfg(feature = "compile")]
pub mod compile;
mod filter;
pub mod rule;
pub mod rules;
pub mod tokenizer;
pub mod types;
pub(crate) mod utils;

#[derive(Error, Debug)]
pub enum Error {
    #[error("unexpected condition: {0}")]
    Unexpected(String),
    #[error("feature not implemented: {0}")]
    Unimplemented(String),
}
