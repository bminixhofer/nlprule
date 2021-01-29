use std::{
    collections::HashSet,
    fs::{read_to_string, File},
    hash::{Hash, Hasher},
    io::{BufReader, BufWriter},
    sync::Arc,
};

use clap::Clap;

use crate::{
    rules::{Rules, RulesOptions},
    tokenizer::{
        chunk::Chunker, multiword::MultiwordTagger, tag::Tagger, Tokenizer, TokenizerOptions,
    },
    types::DefaultHasher,
};

use self::parse_structure::{BuildInfo, RegexCache};

mod impls;
mod parse_structure;
mod structure;

#[derive(Clap)]
#[clap(
    version = env!("CARGO_PKG_VERSION"),
    author = "Benjamin Minixhofer <bminixhofer@gmail.com>"
)]
pub struct BuildOptions {
    #[clap(long)]
    pub tag_paths: Vec<String>,
    #[clap(long)]
    pub tag_remove_paths: Vec<String>,
    #[clap(long)]
    pub disambiguation_path: String,
    #[clap(long)]
    pub grammar_path: String,
    #[clap(long)]
    pub tokenizer_config_path: String,
    #[clap(long)]
    pub rules_config_path: String,
    #[clap(long)]
    pub chunker_path: Option<String>,
    #[clap(long)]
    pub multiword_tag_path: Option<String>,
    #[clap(long)]
    pub common_words_path: Option<String>,
    #[clap(long)]
    pub regex_cache_path: String,
    #[clap(long)]
    pub out_tokenizer_path: String,
    #[clap(long)]
    pub out_rules_path: String,
}

pub fn compile(opts: &BuildOptions) {
    let common_words = opts
        .common_words_path
        .as_ref()
        .map_or_else(HashSet::new, |path| {
            read_to_string(path)
                .unwrap()
                .lines()
                .map(|x| x.to_string())
                .collect()
        });

    let tokenizer_options: TokenizerOptions =
        serde_json::from_str(&read_to_string(&opts.tokenizer_config_path).unwrap()).unwrap();
    let rules_options: RulesOptions =
        serde_json::from_str(&read_to_string(&opts.rules_config_path).unwrap()).unwrap();

    let tagger = Tagger::from_dumps(
        &opts.tag_paths,
        &opts.tag_remove_paths,
        &tokenizer_options.extra_tags,
        &common_words,
    )
    .unwrap();

    let mut hasher = DefaultHasher::default();
    let mut word_store = tagger.word_store().iter().collect::<Vec<_>>();
    word_store.sort_by(|a, b| a.1.cmp(b.1));
    word_store.hash(&mut hasher);
    let word_store_hash = hasher.finish();

    let regex_cache = if let Ok(file) = File::open(&opts.regex_cache_path) {
        let cache: RegexCache = bincode::deserialize_from(BufReader::new(file)).unwrap();
        if *cache.word_hash() == word_store_hash {
            cache
        } else {
            RegexCache::new(word_store_hash)
        }
    } else {
        RegexCache::new(word_store_hash)
    };

    let mut build_info = BuildInfo::new(Arc::new(tagger), regex_cache);
    let chunker = if let Some(path) = &opts.chunker_path {
        let reader = BufReader::new(File::open(path).unwrap());
        let chunker = Chunker::from_json(reader);
        Some(chunker)
    } else {
        None
    };
    let multiword_tagger = if let Some(path) = &opts.multiword_tag_path {
        Some(MultiwordTagger::from_dump(path, &build_info))
    } else {
        None
    };

    let tokenizer = Tokenizer::from_xml(
        &opts.disambiguation_path,
        &mut build_info,
        chunker,
        multiword_tagger,
        tokenizer_options,
    )
    .unwrap();

    let f = BufWriter::new(File::create(&opts.out_tokenizer_path).unwrap());
    bincode::serialize_into(f, &tokenizer).unwrap();

    let rules = Rules::from_xml(&opts.grammar_path, &mut build_info, rules_options);

    let f = BufWriter::new(File::create(&opts.regex_cache_path).unwrap());
    bincode::serialize_into(f, build_info.mut_regex_cache()).unwrap();

    let f = BufWriter::new(File::create(&opts.out_rules_path).unwrap());
    bincode::serialize_into(f, &rules).unwrap();
}
