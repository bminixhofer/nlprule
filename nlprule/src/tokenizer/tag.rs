//! A dictionary-based tagger. The raw format is tuples of the form `(word, lemma, part-of-speech)`
//! where each word typically has multiple entries with different part-of-speech tags.

use crate::types::*;
use bimap::BiMap;
use fst::{IntoStreamer, Map, Streamer};
use indexmap::IndexMap;
use log::error;
use serde::{Deserialize, Serialize};
use std::{borrow::Cow, fmt, iter::once};

#[derive(Debug, Copy, Clone, Serialize, Deserialize, Hash, Eq, PartialEq, Ord, PartialOrd)]
#[serde(transparent)]
pub(crate) struct WordIdInt(u32);

impl WordIdInt {
    pub(crate) fn from_value_unchecked(value: u32) -> Self {
        WordIdInt(value)
    }
}

#[derive(Debug, Copy, Clone, Serialize, Deserialize, Hash, Eq, PartialEq, Ord, PartialOrd)]
#[serde(transparent)]
pub(crate) struct PosIdInt(u16);

impl PosIdInt {
    pub(crate) fn from_value_unchecked(value: u16) -> Self {
        PosIdInt(value)
    }

    pub fn value(&self) -> u16 {
        self.0
    }
}

/// A potentially identified word. If it is identified as a known word, many optimizations can be applied.
#[derive(Clone, PartialEq)]
pub struct WordId<'t>(pub(crate) Cow<'t, str>, pub(crate) Option<WordIdInt>);

impl<'t> fmt::Debug for WordId<'t> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let id_str = if let Some(id) = self.1 {
            id.0.to_string()
        } else {
            "none".into()
        };

        write!(f, "{:?}<id={}>", self.0, id_str)
    }
}

impl<'t> WordId<'t> {
    pub(crate) fn to_owned_id(&self) -> owned::WordId {
        owned::WordId(self.0.to_string(), self.1)
    }

    pub(crate) fn id(&self) -> &Option<WordIdInt> {
        &self.1
    }

    pub(crate) fn empty() -> Self {
        WordId("".into(), Some(WordIdInt(0)))
    }

    /// Gets the word as string.
    pub fn as_str(&'t self) -> &'t str {
        self.0.as_ref()
    }
}

#[derive(Copy, Clone, PartialEq)]
pub(crate) enum SpecialPos {
    None = 0,
    Unknown = 1,
    SentStart = 2,
    SentEnd = 3,
}

impl SpecialPos {
    pub fn as_str(&self) -> &'static str {
        match &self {
            SpecialPos::None => "",
            SpecialPos::Unknown => "UNKNOWN",
            SpecialPos::SentStart => "SENT_START",
            SpecialPos::SentEnd => "SENT_END",
        }
    }

    pub fn iter() -> impl Iterator<Item = &'static str> + 'static {
        [
            SpecialPos::None,
            SpecialPos::Unknown,
            SpecialPos::SentStart,
            SpecialPos::SentEnd,
        ]
        .iter()
        .map(|pos| pos.as_str())
    }
}

#[derive(Clone, Copy, PartialEq)]
enum InnerPosId<'t> {
    Normal(&'t str, PosIdInt),
    Special(SpecialPos),
}

/// An identified part-of-speech tag. POS tags are treated as a closed set so every POS tag is identified.
#[derive(Clone, Copy, PartialEq)]
pub struct PosId<'t> {
    inner: InnerPosId<'t>,
}

impl<'t> fmt::Debug for PosId<'t> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}<id={}>", self.as_str(), self.id().0)
    }
}

impl<'t> PosId<'t> {
    pub(crate) fn regular(text: &'t str, id: PosIdInt) -> Self {
        PosId {
            inner: InnerPosId::Normal(text, id),
        }
    }

    pub(crate) fn special(special: SpecialPos) -> Self {
        PosId {
            inner: InnerPosId::Special(special),
        }
    }

    /// Converts this ID to an owned ID.
    pub fn to_owned_id(&self) -> owned::PosId {
        owned::PosId(self.as_str().to_string(), self.id())
    }

    pub(crate) fn id(&self) -> PosIdInt {
        match &self.inner {
            InnerPosId::Normal(_, id) => *id,
            InnerPosId::Special(special) => PosIdInt(*special as u16),
        }
    }

    /// Gets the part-of-speech as string.
    pub fn as_str(&self) -> &'t str {
        match &self.inner {
            InnerPosId::Normal(text, _) => *text,
            InnerPosId::Special(special) => special.as_str(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct TaggerLangOptions {
    /// Whether to use a heuristic to split potential compound words.
    pub use_compound_split_heuristic: bool,
    /// Whether to always add tags for a lowercase version of the word when assigning part-of-speech tags.
    pub always_add_lower_tags: bool,
    /// Used part-of-speech tags which are not in the tagger dictionary.
    pub extra_tags: Vec<String>,
    /// Whether to retain the last tag if disambiguation leads to an empty tag.
    /// Language-specific in LT so it has to be an option.
    pub retain_last: bool,
}

impl Default for TaggerLangOptions {
    fn default() -> Self {
        TaggerLangOptions {
            use_compound_split_heuristic: false,
            always_add_lower_tags: false,
            extra_tags: Vec::new(),
            retain_last: false,
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

    pub(crate) fn lang_options(&self) -> &TaggerLangOptions {
        &self.lang_options
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
        PosId::regular(
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
    pub fn id_word<'a>(&self, text: Cow<'a, str>) -> WordId<'a> {
        let id = self.word_store.get_by_left(text.as_ref()).copied();
        WordId(text, id)
    }

    /// Get the tags and lemmas (as [WordData][crate::types::WordData]) for the given word.
    ///
    /// # Arguments
    /// * `word`: The word to lookup data for.
    /// * `add_lower`: Whether to add data for the lowercase variant of the word.
    ///     If `None`, will be set according to the language options.
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
                            .map(|x| {
                                let lemma = self.id_word(
                                    format!("{}{}", &word[..i], x.lemma().as_str().to_lowercase())
                                        .into(),
                                );

                                WordData::new(lemma, *x.pos())
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
