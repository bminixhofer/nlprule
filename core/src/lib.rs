//! The NLPRule core.

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
    #[error("unexpected condition: {0}")]
    Unexpected(String),
    #[error("feature not implemented: {0}")]
    Unimplemented(String),
}
