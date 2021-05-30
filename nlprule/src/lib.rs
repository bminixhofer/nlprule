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
//! use nlprule::lang::en;
//!
//! let correcter = en::correcter();
//!
//! assert_eq!(
//!     correcter.correct("She was not been here since Monday.").collect::<Vec<String>>().join(""),
//!     String::from("She was not here since Monday.")
//! );
//! # Ok::<(), nlprule::Error>(())
//! ```
//!
//! Get suggestions and correct a text:
//!
//! ```no_run
//! use nlprule::lang::en;
//!
//! let correcter = en::correcter();
//!
//! let text = "She was not been here since Monday.";
//!
//! let suggestions = correcter.suggest(text).next().expect("`text` contains one sentence.");
//! assert_eq!(*suggestions[0].span().char(), 4usize..16);
//! assert_eq!(suggestions[0].replacements(), vec!["was not", "has not been"]);
//! assert_eq!(suggestions[0].source(), "GRAMMAR/WAS_BEEN/1");
//! assert_eq!(suggestions[0].message(), "Did you mean was not or has not been?");
//!
//! # Ok::<(), nlprule::Error>(())
//! ```
//!
//! Tokenize & analyze a text:
//!
//! ```no_run
//! use nlprule::lang::en;
//! use nlprule::properties::Tokenize;
//!
//! let analyzer = en::analyzer();
//!
//! let text = "A brief example is shown.";
//!
//! // returns an iterator over sentences
//! let sentence = analyzer.tokenize(text).next().expect("`text` contains one sentence.");
//!
//! println!("{:#?}", sentence);
//! assert_eq!(sentence.tokens()[1].as_str(), "brief");
//! assert_eq!(sentence.tokens()[1].tags()?.iter().next().unwrap().pos().as_str(), "JJ");
//! assert_eq!(sentence.tokens()[1].chunks()?, &["I-NP-singular"]);
//! // some other information like char / byte span, lemmas etc. is also set!
//! # Ok::<(), nlprule::Error>(())
//! ```

// #![warn(missing_docs)]
use std::io;

use thiserror::Error;

#[cfg(feature = "compile")]
pub mod compile;
pub mod components;
mod filter;
#[macro_use]
pub mod lang;
pub mod properties;
pub mod rule;
pub mod types;
pub(crate) mod utils;

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
    #[error(transparent)]
    Property(#[from] properties::Error),
    #[error("Test failed. See logs for details.")]
    TestFailed,
}
