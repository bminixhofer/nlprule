//! A dictionary-based tagger. The raw format is tuples of the form `(word, lemma, part-of-speech)`
//! where each word typically has multiple entries with different part-of-speech tags.

use crate::types::*;
use bimap::BiMap;
use fst::{IntoStreamer, Map, Streamer};
use indexmap::IndexMap;
use log::error;
use serde::{Deserialize, Serialize};
use std::{borrow::Cow, iter::once};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct TaggerLangOptions {
    /// Whether to use a heuristic to split potential compound words.
    pub use_compound_split_heuristic: bool,
    /// Whether to always add tags for a lowercase version of the word when assigning part-of-speech tags.
    pub always_add_lower_tags: bool,
    /// Used part-of-speech tags which are not in the tagger dictionary.
    pub extra_tags: Vec<String>,
}

impl Default for TaggerLangOptions {
    fn default() -> Self {
        TaggerLangOptions {
            use_compound_split_heuristic: false,
            always_add_lower_tags: false,
            extra_tags: Vec::new(),
        }
    }
}

#[derive(Serialize, Deserialize)]
struct TaggerFields {
    tag_fst: Vec<u8>,
    word_store_fst: Vec<u8>,
    tag_store: BiMap<String, PosIdInt>,
    lang_options: TaggerLangOptions,
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
                    let inflect_bytes = inflect_id.raw_value().to_be_bytes();

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
            .map(|(key, value)| (key.clone(), value.raw_value() as u64))
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
            lang_options: tagger.lang_options,
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
            .map(|(key, value)| (key, WordIdInt::from_raw_value(value as u32)))
            .collect();

        let mut tags = DefaultHashMap::new();
        let mut groups = DefaultHashMap::new();

        let tag_fst = Map::new(data.tag_fst).unwrap();
        let mut stream = tag_fst.into_stream();

        while let Some((key, value)) = stream.next() {
            let word = std::str::from_utf8(&key[..key.len() - 1]).unwrap();
            let word_id = *word_store.get_by_left(word).unwrap();

            let value_bytes = value.to_be_bytes();
            let inflection_id = WordIdInt::from_raw_value(u32::from_be_bytes([
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
            lang_options: data.lang_options,
        }
    }
}

/// The lexical tagger.
#[derive(Default, Serialize, Deserialize, Clone)]
#[serde(from = "TaggerFields", into = "TaggerFields")]
pub struct Tagger {
    pub(crate) tags: DefaultHashMap<WordIdInt, IndexMap<WordIdInt, Vec<PosIdInt>>>,
    pub(crate) tag_store: BiMap<String, PosIdInt>,
    pub(crate) word_store: BiMap<String, WordIdInt>,
    pub(crate) groups: DefaultHashMap<WordIdInt, Vec<WordIdInt>>,
    pub(crate) lang_options: TaggerLangOptions,
}

impl Tagger {
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

    /// Tags the given string representation of a part-of-speech tag.
    /// Part-of-speech tags are treated as a closed set so each valid part-of-speech tag will get a numerical id.
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

    /// Tags the given text.
    /// Unknown words will not get a numerical id.
    pub fn id_word<'t>(&'t self, text: Cow<'t, str>) -> WordId<'t> {
        let id = self.word_store.get_by_left(text.as_ref()).copied();
        WordId(text, id)
    }

    /// Get the tags and lemmas (as [WordData][crate::types::WordData]) for the given word.
    ///
    /// # Arguments
    /// * `word`: The word to lookup data for.
    /// * `add_lower`: Whether to add data for the lowercase variant of the word.
    ///     If `None`, will be set according to the language options.
    /// * `use_compound_split_heuristic`: Whether to use a heuristic to split compound words.
    ///     If `None`, will be set according to the language options.
    /// If true, will attempt to find tags for words which are longer than some cutoff and unknown by looking up tags
    /// for substrings from left to right until tags are found or a minimum length reached.
    pub fn get_tags_with_options(
        &self,
        word: &str,
        add_lower: Option<bool>,
        use_compound_split_heuristic: Option<bool>,
    ) -> Vec<WordData> {
        let add_lower = add_lower.unwrap_or(self.lang_options.always_add_lower_tags);
        let use_compound_split_heuristic =
            use_compound_split_heuristic.unwrap_or(self.lang_options.use_compound_split_heuristic);

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

    /// Get the tags and lemmas (as [WordData][crate::types::WordData]) for the given word
    /// using the default options of the tagger.
    ///
    /// # Arguments
    /// * `word`: The word to lookup data for.
    pub fn get_tags(&self, word: &str) -> Vec<WordData> {
        self.get_tags_with_options(word, None, None)
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
