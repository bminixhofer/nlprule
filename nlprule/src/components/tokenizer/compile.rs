use fs_err as fs;
use fs_err::File;
use std::{io::BufReader, path::PathBuf, str::FromStr};

use crate::compile::{BuildComponent, BuildInfo, Error};

use super::*;

#[derive(Deserialize)]
pub struct Paths {
    tokenizer_options: PathBuf,
    srx: PathBuf,
    lang_code: PathBuf,
}

impl BuildComponent for Tokenizer {
    type Paths = Paths;

    fn build(paths: Paths, build_info: Option<&mut BuildInfo>) -> Result<Self, Error> {
        let build_info = build_info.ok_or(Error::BuildInfoUnset)?;

        let options: TokenizerLangOptions =
            serde_json::from_reader(BufReader::new(File::open(&paths.tokenizer_options)?))?;
        let lang_code = fs::read_to_string(paths.lang_code)?;

        let sentencizer =
            srx::SRX::from_str(&fs::read_to_string(&paths.srx)?)?.language_rules(lang_code);

        let mut whitelist = DefaultHashSet::new();

        for (word, _) in build_info.tagger().word_store() {
            if word.contains(|c| options.extra_split_chars.contains(&c)) {
                whitelist.insert(word.to_owned());
            }
        }

        Ok(Tokenizer {
            tagger: build_info.tagger().clone(),
            sentencizer,
            lang_options: options,
            whitelist,
            properties: Default::default(),
        })
    }
}
