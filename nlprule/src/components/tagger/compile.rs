use fs_err as fs;
use fs_err::File;

use crate::compile::{BuildComponent, BuildInfo, Error};
use crate::components::tagger::TaggerLangOptions;

use super::*;
use serde::Deserialize;
use std::{
    collections::HashSet,
    io::{BufRead, BufReader},
    path::{Path, PathBuf},
};

fn get_lines<S1: AsRef<Path>, S2: AsRef<Path>>(
    paths: &[S1],
    remove_paths: &[S2],
) -> std::io::Result<Vec<(String, String, String)>> {
    let mut output = Vec::new();
    let mut disallowed: Vec<String> = Vec::new();

    for path in remove_paths {
        let file = File::open(path.as_ref())?;
        let reader = std::io::BufReader::new(file);

        for line in reader.lines() {
            let line = line?;
            if line.starts_with('#') {
                continue;
            }

            disallowed.push(line.to_string());
        }
    }

    for path in paths {
        let file = File::open(path.as_ref())?;
        let reader = std::io::BufReader::new(file);

        for line in reader.lines() {
            let line = line?;
            if line.starts_with('#') {
                continue;
            }

            if disallowed.contains(&line) {
                continue;
            }

            let parts: Vec<_> = line.split('\t').collect();

            let word = parts[0].to_string();
            let inflection = parts[1].to_string();
            let tag = parts[2].to_string();

            output.push((word, inflection, tag))
        }
    }

    Ok(output)
}

#[derive(Deserialize)]
pub struct Paths {
    tag_dict: Vec<PathBuf>,
    tag_remove_dict: Vec<PathBuf>,
    common_words: PathBuf,
    tagger_options: PathBuf,
}

impl BuildComponent for Tagger {
    type Paths = Paths;

    /// TODO: move and update
    /// Creates a tagger from raw files.
    ///
    /// # Arguments
    /// * `paths`: Paths to files where each line contains the word, lemma and tag, respectively,
    /// separated by tabs, to be added to the tagger.
    /// * `remove_paths`: Paths to files where each line contains the word, lemma and tag, respectively,
    /// separated by tabs, to be removed from the tagger if present in the files from `paths`.
    fn build(paths: Paths, _build_info: Option<&mut BuildInfo>) -> Result<Self, Error> {
        let options: TaggerLangOptions =
            serde_json::from_reader(BufReader::new(File::open(&paths.tagger_options)?))?;
        let common_words: HashSet<String> = fs::read_to_string(paths.common_words)?
            .lines()
            .map(ToOwned::to_owned)
            .collect();

        let mut tag_store = HashSet::new();
        let mut word_store = HashSet::new();

        // add language specific special tags
        tag_store.extend(options.extra_tags.iter().map(|x| x.as_str()));

        let lines = get_lines(&paths.tag_dict, &paths.tag_remove_dict)?;

        let punct = "!\"#$%&\\'()*+,-./:;<=>?@[\\]^_`{|}~";
        for i in 0..punct.len() {
            word_store.insert(&punct[i..(i + 1)]);
        }

        word_store.extend(common_words.iter().map(|x| x.as_str()));

        for (word, inflection, tag) in lines.iter() {
            word_store.insert(word);
            word_store.insert(inflection);
            tag_store.insert(tag);
        }

        // the empty string must not be part of any wordlist
        assert!(!word_store.contains(""));

        // word store ids should be consistent across runs
        let mut word_store: Vec<_> = word_store.into_iter().collect();
        word_store.sort_unstable();

        // add special empty string to wordlist, must be the first element to have id 0
        word_store.insert(0, "");

        // tag store ids should be consistent across runs
        let mut tag_store: Vec<_> = tag_store.into_iter().collect();
        tag_store.sort_unstable();

        // add special part of speech tags, they must have ids starting from zero
        for (i, special_pos) in SpecialPos::iter().enumerate() {
            tag_store.insert(i, special_pos);
        }

        let word_store: BiMap<_, _> = word_store
            .iter()
            .enumerate()
            .map(|(i, x)| (x.to_string(), WordIdInt::from_value_unchecked(i as u32)))
            .collect();
        let tag_store: BiMap<_, _> = tag_store
            .iter()
            .enumerate()
            .map(|(i, x)| (x.to_string(), PosIdInt::from_value_unchecked(i as u16)))
            .collect();

        let mut tags: Vec<Option<Vec<(WordIdInt, PosIdInt)>>> = vec![None; word_store.len()];

        for (word, inflection, tag) in lines.iter() {
            let word_id = word_store.get_by_left(word).unwrap();
            let lemma_id = word_store.get_by_left(inflection).unwrap();
            let pos_id = tag_store.get_by_left(tag).unwrap();

            match &mut tags[word_id.value() as usize] {
                Some(vec) => {
                    vec.push((*lemma_id, *pos_id));
                }
                None => {
                    tags[word_id.value() as usize] = Some(vec![(*lemma_id, *pos_id)]);
                }
            }
        }

        Ok(Tagger {
            tags: WordIdMap(tags),
            word_store,
            tag_store,
            lang_options: options,
        })
    }
}
