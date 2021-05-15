use clap::Clap;
use fs::{File, OpenOptions};
use fs_err as fs;

use log::info;
use nlprule::compile::{BuildComponent, BuildInfo, Error};
use nlprule::components::{
    chunker::Chunker,
    multiword_tagger::MultiwordTagger,
    rules::{Disambiguator, Rules},
    tagger::Tagger,
    tokenizer::Tokenizer,
    Component,
};
use serde::{Deserialize, Serialize};
use std::path::Path;
use std::path::PathBuf;

#[derive(clap::Clap)]
#[clap(
    version = env!("CARGO_PKG_VERSION"),
    author = "Benjamin Minixhofer <bminixhofer@gmail.com>"
)]
pub struct BuildOptions {
    #[clap(long, parse(from_os_str))]
    pub build_dir: PathBuf,
    #[clap(long, parse(from_os_str))]
    pub out_dir: PathBuf,
}

#[derive(Serialize, Deserialize)]
struct BuildFilePaths {
    lang_code: PathBuf,
    tag_dict: Vec<PathBuf>,
    tag_remove_dict: Vec<PathBuf>,
    chunker: PathBuf,
    disambiguator_xml: PathBuf,
    rules_xml: PathBuf,
    multiword_tags: PathBuf,
    common_words: PathBuf,
    regex_cache: PathBuf,
    srx: PathBuf,
    tagger_options: PathBuf,
    rules_options: PathBuf,
    tokenizer_options: PathBuf,
    disambiguator_options: PathBuf,
}

impl BuildFilePaths {
    fn new<P: AsRef<Path>>(build_dir: P) -> Self {
        let p = build_dir.as_ref();
        BuildFilePaths {
            lang_code: p.join("lang_code.txt"),
            tag_dict: vec![p.join("tags/output.dump"), p.join("tags/added.txt")],
            tag_remove_dict: vec![p.join("tags/removed.txt")],
            chunker: p.join("chunker.json"),
            disambiguator_xml: p.join("disambiguation.xml"),
            rules_xml: p.join("grammar.xml"),
            multiword_tags: p.join("tags/multiwords.txt"),
            common_words: p.join("common.txt"),
            regex_cache: p.join("regex_cache.bin"),
            srx: p.join("segment.srx"),
            tagger_options: p.join("tagger_options.json"),
            rules_options: p.join("rules_options.json"),
            tokenizer_options: p.join("tokenizer_options.json"),
            disambiguator_options: p.join("disambiguator_options.json"),
        }
    }
}

fn main() -> Result<(), Error> {
    env_logger::init();
    let opts = BuildOptions::parse();
    let paths = BuildFilePaths::new(opts.build_dir);

    fs::create_dir_all(&opts.out_dir)?;

    let paths_value = serde_json::to_value(&paths)?;

    let tagger = Tagger::build(serde_json::from_value(paths_value.clone())?, None)?;
    let mut build_info = BuildInfo::new(&tagger, &paths.regex_cache)?;

    macro_rules! build {
        ($component:ty) => {
            info!("Creating component \"{}\".", <$component>::name());
            let instance = <$component>::build(
                serde_json::from_value(paths_value.clone())?,
                Some(&mut build_info),
            )?;
            instance.to_writer(
                &OpenOptions::new()
                    .write(true)
                    .create(true)
                    .open(opts.out_dir.join(format!("{}.bin", <$component>::name())))?,
            )?;
        };
    }

    build!(Tokenizer);
    build!(Disambiguator);
    build!(MultiwordTagger);
    build!(Chunker);
    build!(Rules);

    // write the regex cache at the end, otherwise it isn't fully populated
    bincode::serialize_into(
        &File::create(&paths.regex_cache)?,
        build_info.mut_regex_cache(),
    )?;

    Ok(())
}
