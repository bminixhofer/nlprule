use std::{
    io::{BufRead, BufReader},
    path::PathBuf,
};

use fs_err::File;

use crate::compile::{BuildComponent, BuildInfo, Error};

use super::*;

#[derive(Deserialize)]
pub struct Paths {
    multiword_tags: PathBuf,
}

impl BuildComponent for MultiwordTagger {
    type Paths = Paths;

    fn build(paths: Paths, info: Option<&mut BuildInfo>) -> Result<Self, Error> {
        let tagger = info.ok_or(Error::BuildInfoUnset)?.tagger();

        let reader = BufReader::new(File::open(paths.multiword_tags)?);
        let mut multiwords = Vec::new();

        for line in reader.lines() {
            let line = line?;

            // strip comments
            let line = &line[..line.find('#').unwrap_or_else(|| line.len())].trim();
            if line.is_empty() {
                continue;
            }
            let tab_split: Vec<_> = line.split('\t').collect();

            let word: String = tab_split[0]
                .split_whitespace()
                .collect::<Vec<_>>()
                .join(" ");
            let pos = tagger.id_tag(tab_split[1]).into_static();
            multiwords.push((word, pos));
        }

        Ok((MultiwordTaggerFields { multiwords }).into())
    }
}
