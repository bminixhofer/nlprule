use std::{
    hash::{Hash, Hasher},
    io::BufReader,
    num::ParseIntError,
    path::Path,
};

pub mod utils;

use crate::components::tagger::Tagger;

use crate::types::*;
use fs_err::File;
use log::info;
use serde::{de::DeserializeOwned, Deserialize, Serialize};
use thiserror::Error;

#[derive(Error, Debug)]
#[allow(missing_docs)]
pub enum Error {
    #[error(transparent)]
    Io(#[from] std::io::Error),
    #[error(transparent)]
    Serialization(#[from] bincode::Error),
    #[error(transparent)]
    NlpruleError(#[from] crate::Error),
    #[error(transparent)]
    Json(#[from] serde_json::Error),
    #[error(transparent)]
    Srx(#[from] srx::Error),
    #[error(transparent)]
    RegexSyntax(#[from] regex_syntax::ast::Error),
    #[error("regex compilation error: {0}")]
    Regex(Box<dyn std::error::Error + Send + Sync + 'static>),
    #[error("unexpected condition: {0}")]
    Unexpected(String),
    #[error("feature not implemented: {0}")]
    Unimplemented(String),
    #[error(transparent)]
    ParseError(#[from] ParseIntError),
    #[error("`BuildInfo` is required to build this component, but is unset.")]
    BuildInfoUnset,
    #[error("unknown error: {0}")]
    Other(#[from] Box<dyn std::error::Error + Send + Sync + 'static>),
}

pub trait BuildComponent: Sized {
    type Paths: DeserializeOwned;

    fn build(paths: Self::Paths, build_info: Option<&mut BuildInfo>) -> Result<Self, Error>;
}

#[derive(Serialize, Deserialize, Debug)]
pub struct RegexCache {
    cache: DefaultHashMap<u64, Option<DefaultHashSet<WordIdInt>>>,
    // this is compared with the hash of the word store of the tagger
    word_hash: u64,
}

impl RegexCache {
    pub fn new(word_hash: u64) -> Self {
        RegexCache {
            cache: DefaultHashMap::default(),
            word_hash,
        }
    }

    pub fn word_hash(&self) -> &u64 {
        &self.word_hash
    }

    pub(crate) fn get(&self, key: &u64) -> Option<&Option<DefaultHashSet<WordIdInt>>> {
        self.cache.get(key)
    }

    pub(crate) fn insert(&mut self, key: u64, value: Option<DefaultHashSet<WordIdInt>>) {
        self.cache.insert(key, value);
    }
}

pub struct BuildInfo<'a> {
    tagger: &'a Tagger,
    regex_cache: RegexCache,
}

impl<'a> BuildInfo<'a> {
    pub fn new<P: AsRef<Path>>(tagger: &'a Tagger, regex_cache_path: P) -> Result<Self, Error> {
        let mut hasher = DefaultHasher::default();
        let mut word_store = tagger.word_store().iter().collect::<Vec<_>>();
        word_store.sort_by(|a, b| a.1.cmp(b.1));
        word_store.hash(&mut hasher);
        let word_store_hash = hasher.finish();

        let regex_cache = if let Ok(file) = File::open(regex_cache_path.as_ref()) {
            let cache: RegexCache = bincode::deserialize_from(BufReader::new(file))?;
            if *cache.word_hash() == word_store_hash {
                info!(
                    "Regex cache at {} is valid.",
                    regex_cache_path.as_ref().display()
                );
                cache
            } else {
                info!("Regex cache was provided but is not valid. Rebuilding.");
                RegexCache::new(word_store_hash)
            }
        } else {
            info!(
                "No regex cache provided. Building and writing to {}.",
                regex_cache_path.as_ref().display()
            );
            RegexCache::new(word_store_hash)
        };

        Ok(BuildInfo {
            tagger,
            regex_cache,
        })
    }

    pub fn tagger(&self) -> &'a Tagger {
        self.tagger
    }

    pub fn mut_regex_cache(&mut self) -> &mut RegexCache {
        &mut self.regex_cache
    }
}
