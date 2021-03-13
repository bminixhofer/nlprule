//! Creates the nlprule binaries from a *build directory*. Usage information in /build/README.md.

use fs::File;
use fs_err as fs;

use std::{
    hash::{Hash, Hasher},
    io::{self, BufRead, BufReader, BufWriter},
    num::ParseIntError,
    path::{Path, PathBuf},
    str::FromStr,
    sync::Arc,
};

use crate::{
    rules::Rules,
    spellcheck::Spell,
    tokenizer::{chunk::Chunker, multiword::MultiwordTagger, tag::Tagger, Tokenizer},
    types::*,
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
    regex_cache_path: PathBuf,
    srx_path: PathBuf,
    common_words_path: PathBuf,
    spell_dir_path: PathBuf,
    spell_map_path: PathBuf,
    spell_extra_path: PathBuf,
}

impl BuildFilePaths {
    // this has to be kept in sync with the paths the builder in build/make_build_dir.py stores the resources at
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
            regex_cache_path: p.join("regex_cache.bin"),
            srx_path: p.join("segment.srx"),
            common_words_path: p.join("common.txt"),
            spell_dir_path: p.join("spell"),
            spell_map_path: p.join("spell/map.txt"),
            spell_extra_path: p.join("spell/spelling.txt"),
        }
    }
}

#[derive(Error, Debug)]
#[allow(missing_docs)]
pub enum Error {
    #[error("input/output error")]
    Io(#[from] std::io::Error),
    #[error("serialization error")]
    Serialization(#[from] bincode::Error),
    #[error("JSON deserialization error")]
    JSON(#[from] serde_json::Error),
    #[error("error loading SRX")]
    SRX(#[from] srx::Error),
    #[error("language options do not exist for '{lang_code}'")]
    LanguageOptionsDoNotExist { lang_code: String },
    #[error("regex syntax error: {0}")]
    RegexSyntax(#[from] regex_syntax::ast::Error),
    #[error("regex compilation error: {0}")]
    Regex(Box<dyn std::error::Error + Send + Sync + 'static>),
    #[error("unexpected condition: {0}")]
    Unexpected(String),
    #[error("feature not implemented: {0}")]
    Unimplemented(String),
    #[error("error parsing to integer: {0}")]
    ParseError(#[from] ParseIntError),
    #[error("nlprule error: {0}")]
    NLPRuleError(#[from] crate::Error),
    #[error("unknown error")]
    Other(#[from] Box<dyn std::error::Error + Send + Sync + 'static>),
}

/// Compiles the binaries from a build directory.
pub fn compile(
    build_dir: impl AsRef<Path>,
    mut rules_dest: impl io::Write,
    mut tokenizer_dest: impl io::Write,
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

    let spellchecker_lang_options =
        utils::spellchecker_lang_options(&lang_code).ok_or_else(|| {
            Error::LanguageOptionsDoNotExist {
                lang_code: lang_code.clone(),
            }
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

    info!("Creating tokenizer.");

    let mut tokenizer = Tokenizer::from_xml(
        &paths.disambiguation_path,
        &mut build_info,
        chunker,
        None,
        srx::SRX::from_str(&fs::read_to_string(&paths.srx_path)?)?.language_rules(lang_code),
        tokenizer_lang_options,
    )?;

    let mut extra_phrases = DefaultHashSet::new();
    let mut extra_spell_words = DefaultHashSet::new();

    // comments must already be stripped from this file such that each line contains one word or phrase
    let reader = BufReader::new(File::open(&paths.spell_extra_path)?);
    for line in reader.lines() {
        let line = line?;
        let content = line.trim();

        match tokenizer.get_token_strs(content).len() {
            0 => {
                return Err(Error::Unexpected(format!(
                    "empty lines in {} are not allowed.",
                    paths.spell_extra_path.display()
                )))
            }
            // if the content is exactly one token, we just add it to the spellchecker regularly
            1 => extra_spell_words.insert(content.to_owned()),
            // if the content is a phrase (i.e multiple tokens) we add it to the multiword tagger, since words found by the multiword tagger are considered correct
            _ => extra_phrases.insert(content.to_owned()),
        };
    }

    let multiword_tagger = if paths.multiword_tag_path.exists() {
        info!(
            "{} exists. Building multiword tagger.",
            paths.multiword_tag_path.display()
        );
        Some(MultiwordTagger::from_dump(
            paths.multiword_tag_path,
            extra_phrases.into_iter(),
            &build_info,
        )?)
    } else {
        None
    };
    tokenizer.multiword_tagger = multiword_tagger;
    tokenizer.to_writer(&mut tokenizer_dest)?;

    info!("Creating spellchecker.");

    let spellchecker = Spell::from_dumps(
        paths.spell_dir_path,
        paths.spell_map_path,
        &extra_spell_words,
        spellchecker_lang_options,
        &tokenizer,
    )?;

    info!("Creating grammar rules.");

    let rules = Rules::from_xml(
        &paths.grammar_path,
        &mut build_info,
        spellchecker,
        Arc::new(tokenizer),
        rules_lang_options,
    );
    rules.to_writer(&mut rules_dest)?;

    // we need to write the regex cache after building the rules, otherwise it isn't fully populated
    let f = BufWriter::new(File::create(&paths.regex_cache_path)?);
    bincode::serialize_into(f, build_info.mut_regex_cache())?;

    Ok(())
}
