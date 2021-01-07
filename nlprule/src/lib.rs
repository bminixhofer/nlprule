//! The NLPRule core.
//! # Overview
//!
//! NLPRule has the following abstractions:
//! - A [Tokenizer][tokenizer::Tokenizer].

use thiserror::Error;

pub mod composition;
pub mod filter;
#[cfg(feature = "compile")]
pub(crate) mod from_structure;
pub mod rule;
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
