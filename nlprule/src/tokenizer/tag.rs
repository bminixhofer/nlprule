//! A dictionary-based tagger.

use crate::{properties::*, types::*, utils::parallelism::MaybeParallelRefIterator};
use bimap::BiMap;
use fst::{IntoStreamer, Map, Streamer};
use lazy_static::lazy_static;
use log::error;
use serde::{Deserialize, Serialize};
use std::{
    borrow::Cow,
    cell::UnsafeCell,
    fmt,
    iter::{once, FusedIterator},
};

#[derive(Debug, Copy, Clone, Serialize, Deserialize, Hash, Eq, PartialEq, Ord, PartialOrd)]
#[serde(transparent)]
pub(crate) struct WordIdInt(u32);

#[allow(dead_code)] // used in compile module
impl WordIdInt {
    pub(crate) fn from_value_unchecked(value: u32) -> Self {
        WordIdInt(value)
    }

    pub fn value(&self) -> u32 {
        self.0
    }
}

#[derive(Debug, Copy, Clone, Serialize, Deserialize, Hash, Eq, PartialEq, Ord, PartialOrd)]
#[serde(transparent)]
pub(crate) struct PosIdInt(u16);

impl PosIdInt {
    #[allow(dead_code)] // used in compile module
    pub(crate) fn from_value_unchecked(value: u16) -> Self {
        PosIdInt(value)
    }

    pub fn value(&self) -> u16 {
        self.0
    }
}

/// A potentially identified word. If it is identified as a known word, many optimizations can be applied.
#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
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

impl<'t> Default for WordId<'t> {
    fn default() -> Self {
        WordId::empty()
    }
}

impl<'t> WordId<'t> {
    pub(crate) fn id(&self) -> &Option<WordIdInt> {
        &self.1
    }

    pub(crate) fn empty() -> Self {
        WordId("".into(), Some(WordIdInt(0)))
    }

    /// Gets the word as string.
    pub fn as_str(&self) -> &str {
        self.0.as_ref()
    }

    pub fn as_ref_str(&self) -> &'t str {
        match &self.0 {
            Cow::Borrowed(x) => *x,
            Cow::Owned(_) => panic!("can not get `&'t str` reference from owned Cow!"),
        }
    }

    /// Converts this struct to a struct with `'static` lifetime by cloning borrowed data.
    pub fn into_static(self) -> WordId<'static> {
        WordId(self.0.into_owned().into(), self.1)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
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

    #[allow(dead_code)] // used in compile module
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

#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
enum InnerPosId<'t> {
    Normal(Cow<'t, str>, PosIdInt),
    Special(SpecialPos),
}

/// An identified part-of-speech tag. POS tags are treated as a closed set so every POS tag is identified.
#[derive(Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
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
            inner: InnerPosId::Normal(text.into(), id),
        }
    }

    pub(crate) fn special(special: SpecialPos) -> Self {
        PosId {
            inner: InnerPosId::Special(special),
        }
    }

    pub(crate) fn id(&self) -> PosIdInt {
        match &self.inner {
            InnerPosId::Normal(_, id) => *id,
            InnerPosId::Special(special) => PosIdInt(*special as u16),
        }
    }

    pub(crate) fn is_special(&self) -> bool {
        matches!(self.inner, InnerPosId::Special { .. })
    }

    /// Converts this struct to a struct with `'static` lifetime by cloning borrowed data.
    pub fn into_static(self) -> PosId<'static> {
        let inner = match self.inner {
            InnerPosId::Normal(string, id) => InnerPosId::Normal(string.into_owned().into(), id),
            InnerPosId::Special(special) => InnerPosId::Special(special),
        };

        PosId { inner }
    }

    /// Gets the part-of-speech as string.
    pub fn as_str(&self) -> &str {
        match &self.inner {
            InnerPosId::Normal(text, _) => text.as_ref(),
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
    anchors: Vec<String>,
}

impl From<Tagger> for TaggerFields {
    fn from(tagger: Tagger) -> Self {
        let mut tag_fst_items = Vec::new();

        for (word_id, map) in tagger.tags.iter() {
            let word = tagger.str_for_word_id(&word_id);

            for (i, (inflect_id, pos_id)) in map.iter().enumerate() {
                assert!(i < 255);

                let key: Vec<u8> = word
                    .as_bytes()
                    .iter()
                    .chain(once(&(i as u8)))
                    .copied()
                    .collect();
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

        tag_fst_items.sort_by(|(a, _), (b, _)| a.cmp(b));

        let mut word_store_items: Vec<_> = tagger
            .word_store
            .iter()
            .map(|(key, value)| (key.clone(), value.0 as u64))
            .collect();
        word_store_items.sort_by(|(a, _), (b, _)| a.cmp(b));

        let anchors = word_store_items
            .iter()
            .map(|(key, _)| key.clone())
            // The distance between anchors, there are usually more than 300k words, so a step of 1000 allows dividing
            // the work between threads nicely. Changing this by a factor of 10 only makes a couple of percent difference in speed.
            .step_by(1_000)
            .collect();

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
            anchors,
        }
    }
}

/// An (unsafe) shared mutable slice. Can be used if the same index in the slice will never
/// be used in different threads. See https://stackoverflow.com/a/65182786.
#[derive(Copy, Clone)]
struct UnsafeSlice<'a, T> {
    slice: &'a [UnsafeCell<T>],
}
unsafe impl<'a, T: Send + Sync> Send for UnsafeSlice<'a, T> {}
unsafe impl<'a, T: Send + Sync> Sync for UnsafeSlice<'a, T> {}

impl<'a, T> UnsafeSlice<'a, T> {
    pub fn new(slice: &'a mut [T]) -> Self {
        let ptr = slice as *mut [T] as *const [UnsafeCell<T>];
        Self {
            slice: unsafe { &*ptr },
        }
    }

    /// SAFETY: It is UB if two threads write to the same index without
    /// synchronization.
    pub unsafe fn write(&self, i: usize, value: T) {
        let ptr = self.slice[i].get();
        *ptr = value;
    }
}

impl From<TaggerFields> for Tagger {
    fn from(data: TaggerFields) -> Self {
        let word_store_fst = Map::new(data.word_store_fst).unwrap();
        let mut word_store: BiMap<String, WordIdInt> = BiMap::with_capacity(word_store_fst.len());
        let mut stream = word_store_fst.into_stream();

        while let Some((key, value)) = stream.next() {
            word_store.insert(
                std::str::from_utf8(key)
                    .expect("word store keys are valid utf-8.")
                    .to_owned(),
                WordIdInt(value as u32),
            );
        }

        let tag_fst = Map::new(data.tag_fst).unwrap();
        let anchors = data.anchors;

        let mut tags: Vec<Option<Vec<(WordIdInt, PosIdInt)>>> = vec![None; word_store.len()];
        let tag_slice = UnsafeSlice::new(tags.as_mut_slice());

        // Anchors are strings set at more or less equal distance in the FST.
        // Here, we iterate from one anchor to the next, on each thread.
        // Because keys in the FST are sorted, we know that each thread will
        // deal with a disjoint set of words. Thus, the accessed indices never overlap
        // and we can use the `UnsafeSlice` to avoid synchronization.
        anchors.maybe_par_iter().enumerate().for_each(|(i, _)| {
            let mut stream_builder = tag_fst.range().ge(anchors[i].as_bytes());

            if i + 1 < anchors.len() {
                stream_builder = stream_builder.lt(anchors[i + 1].as_bytes());
            }

            let mut stream = stream_builder.into_stream();

            let mut current_word_id: Option<WordIdInt> = None;
            let mut current_vec: Option<Vec<(WordIdInt, PosIdInt)>> = None;

            while let Some((key, value)) = stream.next() {
                let word = std::str::from_utf8(&key[..key.len() - 1]).unwrap();
                let word_id = *word_store.get_by_left(word).unwrap();

                let value_bytes = value.to_be_bytes();
                let lemma_id = WordIdInt(u32::from_be_bytes([
                    value_bytes[0],
                    value_bytes[1],
                    value_bytes[2],
                    value_bytes[3],
                ]));
                let pos_id = PosIdInt(u16::from_be_bytes([value_bytes[6], value_bytes[7]]));

                if current_word_id == Some(word_id) {
                    current_vec.as_mut().unwrap().push((lemma_id, pos_id));
                } else {
                    if let Some(id) = current_word_id {
                        let word_id_idx = id.0 as usize;
                        // SAFETY: see above. The word index is unique to this chunk.
                        unsafe { tag_slice.write(word_id_idx, current_vec.take()) };
                    }

                    current_word_id = Some(word_id);
                    current_vec = Some(vec![(lemma_id, pos_id)]);
                }
            }

            if let Some(vec) = current_vec {
                let word_id_idx = current_word_id.unwrap().0 as usize;

                // SAFETY: see above. The word index is unique to this chunk.
                unsafe { tag_slice.write(word_id_idx, Some(vec)) };
            }
        });

        Tagger {
            tags: WordIdMap(tags),
            tag_store: data.tag_store,
            word_store,
            lang_options: data.lang_options,
        }
    }
}

#[derive(Default, Serialize, Deserialize, Clone)]
pub(crate) struct WordIdMap<T>(pub Vec<Option<T>>);

impl<T: Clone + Default> WordIdMap<T> {
    pub fn get(&self, id: &WordIdInt) -> Option<&T> {
        self.0
            .get(id.0 as usize)
            .map(|value| value.as_ref())
            .flatten()
    }

    pub fn iter(&self) -> impl Iterator<Item = (WordIdInt, &T)> {
        self.0
            .iter()
            .enumerate()
            .filter_map(|(index, maybe_value)| {
                if let Some(value) = maybe_value {
                    Some((WordIdInt(index as u32), value))
                } else {
                    None
                }
            })
    }
}

pub(crate) enum AddLower {
    Always,
    IfEmpty,
    Never,
}

struct RawTagIter<'a> {
    tagger: &'a Tagger,
    slice_iter: std::slice::Iter<'a, (WordIdInt, PosIdInt)>,
}

impl<'a> Iterator for RawTagIter<'a> {
    type Item = WordData<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.slice_iter.next().map(move |(lemma_id, pos_id)| {
            WordData::new(
                self.tagger
                    .id_word(self.tagger.str_for_word_id(lemma_id).into()),
                self.tagger.id_tag(self.tagger.str_for_pos_id(pos_id)),
            )
        })
    }
}

impl<'a> FusedIterator for RawTagIter<'a> {}
impl<'a> ExactSizeIterator for RawTagIter<'a> {
    fn len(&self) -> usize {
        self.slice_iter.len()
    }
}

struct StrictTagIter<'a> {
    tag_iter: RawTagIter<'a>,
    lower_tag_iter: Option<RawTagIter<'a>>,
}

impl<'a> Iterator for StrictTagIter<'a> {
    type Item = WordData<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.tag_iter.next().or_else(|| {
            self.lower_tag_iter
                .as_mut()
                .and_then(|lower_iter| lower_iter.next())
        })
    }
}

impl<'a> FusedIterator for StrictTagIter<'a> {}
impl<'a> ExactSizeIterator for StrictTagIter<'a> {
    fn len(&self) -> usize {
        self.tag_iter.len()
            + self
                .lower_tag_iter
                .as_ref()
                .map_or(0, |lower_iter| lower_iter.len())
    }
}

/// An iterator over [WordData].
pub struct TagIter<'a> {
    tagger: &'a Tagger,
    tag_iter: StrictTagIter<'a>,
    prefix: Option<&'a str>,
}

impl<'a> Iterator for TagIter<'a> {
    type Item = WordData<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.tag_iter.next().map(|data| {
            let lemma = if let Some(prefix) = &self.prefix {
                self.tagger
                    .id_word(format!("{}{}", prefix, data.lemma().as_str().to_lowercase()).into())
            } else {
                data.lemma().clone()
            };

            WordData::new(lemma, data.pos().clone())
        })
    }
}

impl<'a> FusedIterator for TagIter<'a> {}
impl<'a> ExactSizeIterator for TagIter<'a> {
    fn len(&self) -> usize {
        self.tag_iter.len()
    }
}

/// The lexical tagger.
/// Created from a dictionary that looks like this:
///
/// ```raw
/// actualize   actualize   VB
/// actualize   actualize   VBP
/// actualized  actualize   VBD
/// actualized  actualize   VBN
/// actualizes  actualize   VBZ
/// actualizing actualize   VBG
/// actually    actually    RB
/// ```
///
/// i.e. one word (left) associated with one or more pairs of lemma (middle) and POS (part-of-speech) tag (right).
/// From this structure, the tagger must be able to look up:
/// 1. lemma and pos by word: all lemmas and POS tags associated with a given word.
///
/// (1) is called extensively (at least once for every word) so it has to be as fast as possible.
///
/// ## Implementation
///
/// The tagger stores two bidirectional maps:
/// - A POS bimap: A bimap assigning each POS tag a 16-bit ID. POS tags are a closed set, so there is an entry
/// for every tag in the bimap. This allows e.g. storing a set of IDs of matching tags for a regex instead of actually
/// evaluating it in the matcher logic.
/// - A word bimap: A bimap assigning each known word a 32-bit ID. Words are not a closed set, so if an entry in this map
/// does not exist it only means that the word is not known to nlprule. Still, the map can be used for optimizations similar
/// to the POS bimap. The word bimap also stores lemmas since there is often a large overlap between known words
/// and known lemmas.
///
/// These two maps can be used to relatively cheaply in terms of memory allow (1) while retaining fast lookup.
///
/// There is a `tags` map which associates a Word ID with *multiple* pairs of
/// `(lemma_id, pos_id)` where the ID for the lemma is a regular 32-bit Word ID.
#[derive(Default, Serialize, Deserialize, Clone)]
#[serde(from = "TaggerFields", into = "TaggerFields")]
pub struct Tagger {
    pub(crate) tags: WordIdMap<Vec<(WordIdInt, PosIdInt)>>,
    pub(crate) tag_store: BiMap<String, PosIdInt>,
    pub(crate) word_store: BiMap<String, WordIdInt>,
    pub(crate) lang_options: TaggerLangOptions,
}

impl Transform for Tagger {
    fn properties(&self) -> PropertiesMut {
        lazy_static! {
            static ref PROPERTIES: PropertiesMut = Properties::default().write(&[Property::Tags]);
        }
        *PROPERTIES
    }

    fn transform<'t>(
        &'t self,
        mut sentence: Sentence<'t>,
    ) -> Result<Sentence<'t>, crate::properties::Error> {
        let props = self.property_guard(&mut sentence)?;

        for token in sentence.iter_mut() {
            let mut tag_vec: Vec<_> = self
                .get_tags_with_options(
                    token.as_str(),
                    if token.is_sentence_start() {
                        Some(true)
                    } else {
                        None
                    },
                    None,
                )
                .collect();

            tag_vec.push(
                WordData::new(
                    self.id_word(token.as_str().into()),
                    PosId::special(SpecialPos::None),
                )
                .freeze(),
            );

            if token.is_sentence_end() {
                tag_vec.push(
                    WordData::new(WordId::empty(), PosId::special(SpecialPos::SentEnd)).freeze(),
                );
            }

            *props.tags_mut(token)? = Tags::new(self.id_word(token.as_str().into()), tag_vec);
        }

        Ok(sentence)
    }
}

impl Tagger {
    /// Directly looks up the given word in the `tags` map and returns
    /// corresponding [WordData].
    fn get_raw(&self, word: &str) -> RawTagIter {
        let slice = if let Some(pairs) = self
            .word_store
            .get_by_left(word)
            .and_then(|x| self.tags.get(x))
        {
            pairs.as_slice()
        } else {
            &[]
        };

        RawTagIter {
            tagger: &self,
            slice_iter: slice.iter(),
        }
    }

    #[allow(dead_code)] // used in compile module
    pub(crate) fn lang_options(&self) -> &TaggerLangOptions {
        &self.lang_options
    }

    /// Same as [get_tags] but optionally:
    /// - Adds tags for the lower variant of the word (if `add_lower` is `AddLower::Always`).
    /// - Adds tags for the lower variant of the word if no [WordData] is found otherwise.
    /// (if `add_lower` is `AddLower::IfEmpty`).
    fn get_strict_tags(&self, word: &str, add_lower: AddLower) -> StrictTagIter {
        let tag_iter = self.get_raw(&word);
        let lower_tag_iter = if (matches!(add_lower, AddLower::Always)
            || (matches!(add_lower, AddLower::IfEmpty) && tag_iter.len() == 0))
        {
            let lower = word.to_lowercase();

            if (word != lower)
                && (crate::utils::is_title_case(word) || crate::utils::is_uppercase(word))
            {
                Some(self.get_raw(&lower))
            } else {
                None
            }
        } else {
            None
        };

        StrictTagIter {
            tag_iter,
            lower_tag_iter,
        }
    }

    #[allow(dead_code)] // used by compile module
    pub(crate) fn tag_store(&self) -> &BiMap<String, PosIdInt> {
        &self.tag_store
    }

    #[allow(dead_code)] // used by compile module
    pub(crate) fn word_store(&self) -> &BiMap<String, WordIdInt> {
        &self.word_store
    }

    /// Gets the string associated with a word ID.
    fn str_for_word_id(&self, id: &WordIdInt) -> &str {
        self.word_store
            .get_by_right(id)
            .expect("only valid word ids are created")
    }

    /// Gets the string associated with a POS ID.
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

    /// Get the tags and lemmas (as [WordData]) for the given word.
    ///
    /// # Arguments
    /// * `word`: The word to lookup data for.
    /// * `add_lower`: Whether to add data for the lowercase variant of the word.
    ///     If `None`, will be set according to the language options.
    /// * `use_compound_split_heuristic`: Whether to use a heuristic to split compound words.
    ///     If `None`, will be set according to the language options.
    /// If true, will attempt to find tags for words which are longer than some cutoff and unknown by looking up tags
    /// for substrings from left to right until tags are found or a minimum length reached.
    pub fn get_tags_with_options<'a>(
        &'a self,
        word: &'a str,
        add_lower: Option<bool>,
        use_compound_split_heuristic: Option<bool>,
    ) -> TagIter<'a> {
        let add_lower = add_lower.unwrap_or(self.lang_options.always_add_lower_tags);
        let use_compound_split_heuristic =
            use_compound_split_heuristic.unwrap_or(self.lang_options.use_compound_split_heuristic);

        let mut tags = TagIter {
            tagger: &self,
            tag_iter: self.get_strict_tags(
                word,
                if add_lower {
                    AddLower::Always
                } else {
                    AddLower::IfEmpty
                },
            ),
            prefix: None,
        };

        // compound splitting heuristic, seems to work reasonably well
        if use_compound_split_heuristic && tags.len() == 0 {
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

                    let next_tags = self.get_strict_tags(
                        &next,
                        if add_lower {
                            AddLower::Always
                        } else {
                            AddLower::Never
                        },
                    );

                    if next_tags.len() != 0 {
                        tags = TagIter {
                            tagger: &self,
                            tag_iter: next_tags,
                            prefix: Some(&word[..i]),
                        };
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
    pub fn get_tags<'a>(&'a self, word: &'a str) -> TagIter<'a> {
        self.get_tags_with_options(word, None, None)
    }
}
