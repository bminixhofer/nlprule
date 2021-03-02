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
    spell_dir_path: PathBuf,
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
            spell_dir_path: p.join("spell"),
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

fn parse_spell_dumps<P: AsRef<Path>>(
    spell_dir_path: P,
    variants: &[String],
) -> Result<DefaultHashMap<String, (u8, u8)>, Error> {
    let mut words = DefaultHashMap::new();

    for (i, variant) in variants.iter().enumerate() {
        let spell_path = spell_dir_path.as_ref().join(variant).with_extension("dump");
        info!("Reading spelling dictionary from {}.", spell_path.display());

        let reader = BufReader::new(File::open(spell_path)?);
        for line in reader.lines() {
            match line?
                .trim()
                .split_whitespace()
                .collect::<Vec<_>>()
                .as_slice()
            {
                [freq, word] => {
                    // frequency is denoted as letters from A to Z in LanguageTool where A is the least frequent.
                    // we start from 1 because 0 is reserved for words we do not know the frequency of
                    let freq = 1 + freq.chars().next().expect("freq must have one char - would not have been yielded by split_whitespace otherwise.") as usize - 'A' as usize;
                    assert!(freq < u8::MAX as usize);
                    assert!(i < 8);
                    words.insert(word.to_string(), (1u8 << i, freq as u8));
                }
                _ => continue,
            }
        }
    }

    Ok(words)
}

/// Compiles the binaries from a build directory.
pub fn compile(
    build_dir: impl AsRef<Path>,
    mut rules_dest: impl io::Write,
    mut tokenizer_dest: impl io::Write,
) -> Result<(), Error> {
    let paths = BuildFilePaths::new(&build_dir);

    let lang_code = fs::read_to_string(paths.lang_code_path)?;

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

    let words = parse_spell_dumps(&paths.spell_dir_path, &tagger_lang_options.variants)?;

    info!("Creating tagger.");
    let tagger = Tagger::from_dumps(
        &paths.tag_paths,
        &paths.tag_remove_paths,
        words,
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
    tokenizer.to_writer(&mut tokenizer_dest)?;

    info!("Creating grammar rules.");
    let rules = Rules::from_xml(
        &paths.grammar_path,
        &mut build_info,
        Arc::new(tokenizer),
        rules_lang_options,
    );
    rules.to_writer(&mut rules_dest)?;

    // we need to write the regex cache after building the rules, otherwise it isn't fully populated
    let f = BufWriter::new(File::create(&paths.regex_cache_path)?);
    bincode::serialize_into(f, build_info.mut_regex_cache())?;

    Ok(())
}
