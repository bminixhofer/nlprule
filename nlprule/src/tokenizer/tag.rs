//! A dictionary-based tagger. The raw format is tuples of the form `(word, lemma, part-of-speech)`
//! where each word typically has multiple entries with different part-of-speech tags.

use crate::types::*;
use bimap::BiMap;
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::BufRead;

/// The lexical tagger.
#[derive(Serialize, Deserialize, Default)]
pub struct Tagger {
    tags: HashMap<u32, IndexMap<u32, Vec<u16>>>,
    tag_store: BiMap<String, u16>,
    word_store: BiMap<String, u32>,
    groups: HashMap<u32, Vec<u32>>,
}

impl Tagger {
    fn get_lines<S1: AsRef<str>, S2: AsRef<str>>(
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

                let parts: Vec<_> = line.split('\t').collect();

                if disallowed.contains(&line) {
                    continue;
                }

                let word = parts[0].to_string();
                let inflection = parts[1].to_string();
                let tag = parts[2].to_string();

                output.push((word, inflection, tag))
            }
        }

        Ok(output)
    }

    /// Creates a tagger from raw files.
    ///
    /// # Arguments
    /// * `paths`: Paths to files where each line contains the word, lemma and tag, respectively,
    /// separated by tabs, to be added to the tagger.
    /// * `remove_paths`: Paths to files where each line contains the word, lemma and tag, respectively,
    /// separated by tabs, to be removed from the tagger if present in the files from `paths`.
    pub fn from_dumps<S1: AsRef<str>, S2: AsRef<str>>(
        paths: &[S1],
        remove_paths: &[S2],
    ) -> std::io::Result<Self> {
        let mut tags = HashMap::new();
        let mut groups = HashMap::new();

        let mut tag_store = HashSet::new();
        let mut word_store = HashSet::new();

        // special tags
        tag_store.insert("");
        tag_store.insert("SENT_START");
        tag_store.insert("SENT_END");
        tag_store.insert("UNKNOWN");
        tag_store.insert("PCT");
        tag_store.insert("ORD");
        tag_store.insert("SYM");
        tag_store.insert("RB_SENT");

        tag_store.insert("PKT");
        tag_store.insert("PRO:IND:DAT:SIN:NEU");

        let lines = Tagger::get_lines(paths, remove_paths)?;

        for (word, inflection, tag) in lines.iter() {
            word_store.insert(word);
            word_store.insert(inflection);
            tag_store.insert(tag);
        }

        let word_store: BiMap<_, _> = word_store
            .iter()
            .enumerate()
            .map(|(i, x)| (x.to_string(), i as u32))
            .collect();
        let tag_store: BiMap<_, _> = tag_store
            .iter()
            .enumerate()
            .map(|(i, x)| (x.to_string(), i as u16))
            .collect();

        for (word, inflection, tag) in lines.iter() {
            let word_id = word_store.get_by_left(word).unwrap();
            let inflection_id = word_store.get_by_left(inflection).unwrap();
            let tag_id = tag_store.get_by_left(tag).unwrap();

            let group = groups.entry(*inflection_id).or_insert_with(Vec::new);
            if !group.contains(word_id) {
                group.push(*word_id);
            }

            tags.entry(*word_id)
                .or_insert_with(IndexMap::new)
                .entry(*inflection_id)
                .or_insert_with(Vec::new)
                .push(*tag_id);
        }

        Ok(Tagger {
            tags,
            groups,
            word_store,
            tag_store,
        })
    }

    #[allow(clippy::clippy::ptr_arg)]
    fn get_raw(&self, word: &String) -> Vec<WordData> {
        if let Some(map) = self
            .word_store
            .get_by_left(word)
            .and_then(|x| self.tags.get(x))
        {
            let mut output = Vec::new();

            for (key, value) in map.iter() {
                for tag_id in value {
                    output.push(WordData::new(
                        self.word_store.get_by_right(key).unwrap(),
                        *tag_id,
                    ))
                }
            }

            output
        } else {
            Vec::new()
        }
    }

    fn get_strict_tags(
        &self,
        word: &str,
        add_lower: bool,
        add_lower_if_empty: bool,
    ) -> Vec<WordData> {
        let mut tags = self.get_raw(&word.to_string());
        let lower = word.to_lowercase();

        if (add_lower || (add_lower_if_empty && tags.is_empty()))
            && (word != lower
                && (crate::utils::is_title_case(word) || crate::utils::is_uppercase(word)))
        {
            tags.extend(self.get_raw(&lower));
        }

        tags
    }

    pub fn id_to_tag(&self, id: u16) -> &str {
        self.tag_store.get_by_right(&id).unwrap()
    }

    pub fn tag_to_id(&self, tag: &str) -> u16 {
        *self.tag_store.get_by_left(&tag.to_string()).unwrap()
    }

    pub fn tag_store(&self) -> &BiMap<String, u16> {
        &self.tag_store
    }

    /// Get the tags and lemmas (as [WordData][crate::types::WordData]) for the given word.
    ///
    /// # Arguments
    /// * `word`: The word to lookup data for.
    /// * `add_lower`: Whether to add data for the lowercase variant of the word.
    /// * `use_compound_split_heuristic`: Whether to use a heuristic to split compound words.
    /// If true, will attempt to find tags for words which are longer than some cutoff and unknown by looking up tags
    /// for substrings from left to right until tags are found or a minimum length reached.
    pub fn get_tags(
        &self,
        word: &str,
        add_lower: bool,
        use_compound_split_heuristic: bool,
    ) -> Vec<WordData> {
        let mut tags = self.get_strict_tags(word, add_lower, true);

        // compound splitting heuristic, seems to work reasonably well
        if use_compound_split_heuristic && tags.is_empty() {
            let n_chars = word.chars().count() as isize;

            if n_chars >= 7 {
                let indices = word
                    .char_indices()
                    .take(std::cmp::max(n_chars - 4, 0) as usize)
                    .skip(1)
                    .map(|x| x.0);

                for i in indices {
                    let next = if word.chars().next().unwrap().is_uppercase() {
                        crate::utils::apply_to_first(&word[i..], |c| c.to_uppercase().collect())
                    } else {
                        word[i..].to_string()
                    };

                    let next_tags = self.get_strict_tags(&next, add_lower, false);

                    if !next_tags.is_empty() {
                        tags = next_tags
                            .into_iter()
                            .map(|mut x| {
                                x.lemma =
                                    format!("{}{}", &word[..i], x.lemma.to_lowercase()).into();
                                x
                            })
                            .collect();
                        break;
                    }
                }
            }
        }

        tags
    }

    /// Get the words with the same lemma as the given lemma.
    #[allow(clippy::clippy::ptr_arg)]
    pub fn get_group_members(&self, lemma: &String) -> Vec<&str> {
        self.word_store
            .get_by_left(lemma)
            .and_then(|x| self.groups.get(x))
            .map(|vec| {
                vec.iter()
                    .map(|x| self.word_store.get_by_right(x).unwrap().as_str())
                    .collect()
            })
            .unwrap_or_else(Vec::new)
    }
}
