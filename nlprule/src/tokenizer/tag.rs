//! A dictionary-based tagger. The raw format is tuples of the form `(word, lemma, part-of-speech)`
//! where each word typically has multiple entries with different part-of-speech tags.

use crate::types::*;
use bimap::BiMap;
use fst::{IntoStreamer, Map, Streamer};
use indexmap::IndexMap;
use log::error;
use serde::{Deserialize, Serialize};
use std::io::BufRead;
use std::{borrow::Cow, fs::File, iter::once};
use std::{collections::HashSet, path::Path};

#[derive(Serialize, Deserialize)]
struct TaggerFields {
    tag_fst: Vec<u8>,
    word_store_fst: Vec<u8>,
    tag_store: BiMap<String, PosIdInt>,
}

impl From<Tagger> for TaggerFields {
    fn from(tagger: Tagger) -> Self {
        let mut tag_fst_items = Vec::new();

        for (word_id, map) in tagger.tags.iter() {
            let mut i = 0u8;
            let word = tagger.str_for_word_id(word_id);

            for (inflect_id, pos_ids) in map.iter() {
                for pos_id in pos_ids {
                    assert!(i < 255);
                    i += 1;

                    let key: Vec<u8> = word.as_bytes().iter().chain(once(&i)).copied().collect();
                    let pos_bytes = pos_id.0.to_be_bytes();
                    let inflect_bytes = inflect_id.0.to_be_bytes();

                    let value = u64::from_be_bytes([
                        inflect_bytes[0],
                        inflect_bytes[1],
                        inflect_bytes[2],
                        inflect_bytes[3],
                        0,
                        0,
                        pos_bytes[0],
                        pos_bytes[1],
                    ]);
                    tag_fst_items.push((key, value));
                }
            }
        }

        tag_fst_items.sort_by(|(a, _), (b, _)| a.cmp(b));

        let mut word_store_items: Vec<_> = tagger
            .word_store
            .iter()
            .map(|(key, value)| (key.clone(), value.0 as u64))
            .collect();
        word_store_items.sort_by(|(a, _), (b, _)| a.cmp(b));

        let tag_fst = Map::from_iter(tag_fst_items)
            .unwrap()
            .into_fst()
            .as_bytes()
            .to_vec();
        let word_store_fst = Map::from_iter(word_store_items)
            .unwrap()
            .into_fst()
            .as_bytes()
            .to_vec();

        TaggerFields {
            tag_fst,
            word_store_fst,
            tag_store: tagger.tag_store,
        }
    }
}

impl From<TaggerFields> for Tagger {
    fn from(data: TaggerFields) -> Self {
        let word_store_fst = Map::new(data.word_store_fst).unwrap();
        let word_store: BiMap<String, WordIdInt> = word_store_fst
            .into_stream()
            .into_str_vec()
            .unwrap()
            .into_iter()
            .map(|(key, value)| (key, WordIdInt(value as u32)))
            .collect();

        let mut tags = DefaultHashMap::new();
        let mut groups = DefaultHashMap::new();

        let tag_fst = Map::new(data.tag_fst).unwrap();
        let mut stream = tag_fst.into_stream();

        while let Some((key, value)) = stream.next() {
            let word = std::str::from_utf8(&key[..key.len() - 1]).unwrap();
            let word_id = *word_store.get_by_left(word).unwrap();

            let value_bytes = value.to_be_bytes();
            let inflection_id = WordIdInt(u32::from_be_bytes([
                value_bytes[0],
                value_bytes[1],
                value_bytes[2],
                value_bytes[3],
            ]));
            let pos_id = PosIdInt(u16::from_be_bytes([value_bytes[6], value_bytes[7]]));

            let group = groups.entry(inflection_id).or_insert_with(Vec::new);
            if !group.contains(&word_id) {
                group.push(word_id);
            }

            tags.entry(word_id)
                .or_insert_with(IndexMap::new)
                .entry(inflection_id)
                .or_insert_with(Vec::new)
                .push(pos_id);
        }

        Tagger {
            tags,
            tag_store: data.tag_store,
            word_store,
            groups,
        }
    }
}

/// The lexical tagger.
#[derive(Default, Serialize, Deserialize, Clone)]
#[serde(from = "TaggerFields", into = "TaggerFields")]
pub struct Tagger {
    tags: DefaultHashMap<WordIdInt, IndexMap<WordIdInt, Vec<PosIdInt>>>,
    tag_store: BiMap<String, PosIdInt>,
    word_store: BiMap<String, WordIdInt>,
    groups: DefaultHashMap<WordIdInt, Vec<WordIdInt>>,
}

impl Tagger {
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

    /// Creates a tagger from raw files.
    ///
    /// # Arguments
    /// * `paths`: Paths to files where each line contains the word, lemma and tag, respectively,
    /// separated by tabs, to be added to the tagger.
    /// * `remove_paths`: Paths to files where each line contains the word, lemma and tag, respectively,
    /// separated by tabs, to be removed from the tagger if present in the files from `paths`.
    pub fn from_dumps<S1: AsRef<Path>, S2: AsRef<Path>, S3: AsRef<str>>(
        paths: &[S1],
        remove_paths: &[S2],
        extra_tags: &[S3],
        common_words: &HashSet<String>,
    ) -> std::io::Result<Self> {
        let mut tags = DefaultHashMap::default();
        let mut groups = DefaultHashMap::default();

        let mut tag_store = HashSet::new();
        let mut word_store = HashSet::new();

        // hardcoded special tags
        tag_store.insert("");
        tag_store.insert("SENT_START");
        tag_store.insert("SENT_END");
        tag_store.insert("UNKNOWN");

        // add language specific special tags
        tag_store.extend(extra_tags.iter().map(|x| x.as_ref()));

        let lines = Tagger::get_lines(paths, remove_paths)?;

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

        // word store ids should be consistent across runs
        let mut word_store: Vec<_> = word_store.iter().collect();
        word_store.sort();

        //  tag store ids should be consistent across runs
        let mut tag_store: Vec<_> = tag_store.iter().collect();
        tag_store.sort();

        let word_store: BiMap<_, _> = word_store
            .iter()
            .enumerate()
            .map(|(i, x)| (x.to_string(), WordIdInt(i as u32)))
            .collect();
        let tag_store: BiMap<_, _> = tag_store
            .iter()
            .enumerate()
            .map(|(i, x)| (x.to_string(), PosIdInt(i as u16)))
            .collect();

        for (word, inflection, tag) in lines.iter() {
            let word_id = word_store.get_by_left(word).unwrap();
            let inflection_id = word_store.get_by_left(inflection).unwrap();
            let pos_id = tag_store.get_by_left(tag).unwrap();

            let group = groups.entry(*inflection_id).or_insert_with(Vec::new);
            if !group.contains(word_id) {
                group.push(*word_id);
            }

            tags.entry(*word_id)
                .or_insert_with(IndexMap::new)
                .entry(*inflection_id)
                .or_insert_with(Vec::new)
                .push(*pos_id);
        }

        Ok(Tagger {
            tags,
            groups,
            word_store,
            tag_store,
        })
    }

    fn get_raw(&self, word: &str) -> Vec<WordData> {
        if let Some(map) = self
            .word_store
            .get_by_left(word)
            .and_then(|x| self.tags.get(x))
        {
            let mut output = Vec::new();

            for (key, value) in map.iter() {
                for pos_id in value {
                    output.push(WordData::new(
                        self.id_word(self.str_for_word_id(key).into()),
                        self.id_tag(self.str_for_pos_id(pos_id)),
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
        let mut tags = self.get_raw(&word);
        let lower = word.to_lowercase();

        if (add_lower || (add_lower_if_empty && tags.is_empty()))
            && (word != lower
                && (crate::utils::is_title_case(word) || crate::utils::is_uppercase(word)))
        {
            tags.extend(self.get_raw(&lower));
        }

        tags
    }

    #[allow(dead_code)] // used by compile module
    pub(crate) fn tag_store(&self) -> &BiMap<String, PosIdInt> {
        &self.tag_store
    }

    #[allow(dead_code)] // used by compile module
    pub(crate) fn word_store(&self) -> &BiMap<String, WordIdInt> {
        &self.word_store
    }

    fn str_for_word_id(&self, id: &WordIdInt) -> &str {
        self.word_store
            .get_by_right(id)
            .expect("only valid word ids are created")
    }

    fn str_for_pos_id(&self, id: &PosIdInt) -> &str {
        self.tag_store
            .get_by_right(id)
            .expect("only valid pos ids are created")
    }

    pub fn id_tag<'a>(&self, tag: &'a str) -> PosId<'a> {
        PosId(
            tag,
            *self.tag_store.get_by_left(tag).unwrap_or_else(|| {
                error!(
                    "'{}' not found in tag store, please add it to the `extra_tags`. Using UNKNOWN instead.",
                    tag
                );
                self.tag_store.get_by_left("UNKNOWN").expect("UNKNOWN tag must exist in tag store")
            }),
        )
    }

    pub fn id_word<'t>(&'t self, text: Cow<'t, str>) -> WordId<'t> {
        let id = self.word_store.get_by_left(text.as_ref()).copied();
        WordId(text, id)
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
                // the word always has at least one char if the above condition is satisfied
                // but semantically this is false if no char exists
                let starts_with_uppercase = word.chars().next().map_or(false, |x| x.is_uppercase());

                for i in indices {
                    let next = if starts_with_uppercase {
                        crate::utils::apply_to_first(&word[i..], |c| c.to_uppercase().collect())
                    } else {
                        word[i..].to_string()
                    };

                    let next_tags = self.get_strict_tags(&next, add_lower, false);

                    if !next_tags.is_empty() {
                        tags = next_tags
                            .into_iter()
                            .map(|mut x| {
                                x.lemma = self.id_word(
                                    format!("{}{}", &word[..i], x.lemma.as_ref().to_lowercase())
                                        .into(),
                                );
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
    pub fn get_group_members(&self, lemma: &str) -> Vec<&str> {
        self.word_store
            .get_by_left(lemma)
            .and_then(|x| self.groups.get(x))
            .map(|vec| vec.iter().map(|x| self.str_for_word_id(x)).collect())
            .unwrap_or_else(Vec::new)
    }
}
