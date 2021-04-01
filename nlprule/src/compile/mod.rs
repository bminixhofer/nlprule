//! Creates the nlprule binaries from a *build directory*. Usage information in /build/README.md.

use fs::File;
use fs_err as fs;

use std::{
    hash::{Hash, Hasher},
    io::{self, BufReader, BufWriter},
    num::ParseIntError,
    path::{Path, PathBuf},
    str::FromStr,
    sync::Arc,
};

use crate::{
    rules::Rules,
    tokenizer::{chunk::Chunker, multiword::MultiwordTagger, tag::Tagger, Tokenizer},
    types::DefaultHasher,
};
use log::info;

use self::parse_structure::{BuildInfo, RegexCache};
use thiserror::Error;

mod impls;
mod parse_structure;
mod structure;
mod utils;

struct BuildFilePaths {
    lang_code_path: PathBuf,
    tag_paths: Vec<PathBuf>,
    tag_remove_paths: Vec<PathBuf>,
    chunker_path: PathBuf,
    disambiguation_path: PathBuf,
    grammar_path: PathBuf,
    multiword_tag_path: PathBuf,
    common_words_path: PathBuf,
    regex_cache_path: PathBuf,
    srx_path: PathBuf,
}

impl BuildFilePaths {
    fn new<P: AsRef<Path>>(build_dir: P) -> Self {
        let p = build_dir.as_ref();
        BuildFilePaths {
            lang_code_path: p.join("lang_code.txt"),
            tag_paths: vec![p.join("tags/output.dump"), p.join("tags/added.txt")],
            tag_remove_paths: vec![p.join("tags/removed.txt")],
            chunker_path: p.join("chunker.json"),
            disambiguation_path: p.join("disambiguation.xml"),
            grammar_path: p.join("grammar.xml"),
            multiword_tag_path: p.join("tags/multiwords.txt"),
            common_words_path: p.join("common.txt"),
            regex_cache_path: p.join("regex_cache.bin"),
            srx_path: p.join("segment.srx"),
        }
    }
}

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
    #[error("language options do not exist for '{lang_code}'")]
    LanguageOptionsDoNotExist { lang_code: String },
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
    #[error("unknown error: {0}")]
    Other(#[from] Box<dyn std::error::Error + Send + Sync + 'static>),
}

/// Compiles the binaries from a build directory.
pub fn compile(
    build_dir: impl AsRef<Path>,
    rules_dest: impl io::Write,
    tokenizer_dest: impl io::Write,
) -> Result<(), Error> {
    let paths = BuildFilePaths::new(&build_dir);

    let lang_code = fs::read_to_string(paths.lang_code_path)?;

    info!(
        "Reading common words from {}.",
        paths.common_words_path.display()
    );
    let common_words = fs::read_to_string(paths.common_words_path)?
        .lines()
        .map(|x| x.to_string())
        .collect();

    let tokenizer_lang_options = utils::tokenizer_lang_options(&lang_code).ok_or_else(|| {
        Error::LanguageOptionsDoNotExist {
            lang_code: lang_code.clone(),
        }
    })?;

    let rules_lang_options =
        utils::rules_lang_options(&lang_code).ok_or_else(|| Error::LanguageOptionsDoNotExist {
            lang_code: lang_code.clone(),
        })?;

    let tagger_lang_options =
        utils::tagger_lang_options(&lang_code).ok_or_else(|| Error::LanguageOptionsDoNotExist {
            lang_code: lang_code.clone(),
        })?;

    info!("Creating tagger.");
    let tagger = Tagger::from_dumps(
        &paths.tag_paths,
        &paths.tag_remove_paths,
        &common_words,
        tagger_lang_options,
    )?;

    let mut hasher = DefaultHasher::default();
    let mut word_store = tagger.word_store().iter().collect::<Vec<_>>();
    word_store.sort_by(|a, b| a.1.cmp(b.1));
    word_store.hash(&mut hasher);
    let word_store_hash = hasher.finish();

    let regex_cache = if let Ok(file) = File::open(&paths.regex_cache_path) {
        let cache: RegexCache = bincode::deserialize_from(BufReader::new(file))?;
        if *cache.word_hash() == word_store_hash {
            info!(
                "Regex cache at {} is valid.",
                paths.regex_cache_path.display()
            );
            cache
        } else {
            info!("Regex cache was provided but is not valid. Rebuilding.");
            RegexCache::new(word_store_hash)
        }
    } else {
        info!(
            "No regex cache provided. Building and writing to {}.",
            paths.regex_cache_path.display()
        );
        RegexCache::new(word_store_hash)
    };

    let mut build_info = BuildInfo::new(Arc::new(tagger), regex_cache);
    let chunker = if paths.chunker_path.exists() {
        info!("{} exists. Building chunker.", paths.chunker_path.display());
        let reader = BufReader::new(File::open(paths.chunker_path)?);
        let chunker = Chunker::from_json(reader)?;
        Some(chunker)
    } else {
        None
    };
    let multiword_tagger = if paths.multiword_tag_path.exists() {
        info!(
            "{} exists. Building multiword tagger.",
            paths.multiword_tag_path.display()
        );
        Some(MultiwordTagger::from_dump(
            paths.multiword_tag_path,
            &build_info,
        )?)
    } else {
        None
    };

    info!("Creating tokenizer.");
    let tokenizer = Tokenizer::from_xml(
        &paths.disambiguation_path,
        &mut build_info,
        chunker,
        multiword_tagger,
        srx::SRX::from_str(&fs::read_to_string(&paths.srx_path)?)?.language_rules(lang_code),
        tokenizer_lang_options,
    )?;
    tokenizer.to_writer(tokenizer_dest)?;

    info!("Creating grammar rules.");
    let rules = Rules::from_xml(&paths.grammar_path, &mut build_info, rules_lang_options);
    rules.to_writer(rules_dest)?;

    // we need to write the regex cache after building the rules, otherwise it isn't fully populated
    let f = BufWriter::new(File::create(&paths.regex_cache_path)?);
    bincode::serialize_into(f, build_info.mut_regex_cache())?;

    Ok(())
}
