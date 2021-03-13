use fst::{IntoStreamer, Map, MapBuilder, Streamer};
use serde::{Deserialize, Serialize};
use std::{
    cmp::Ordering,
    collections::{BinaryHeap, HashSet},
    ops::{Deref, DerefMut},
};
use unicode_categories::UnicodeCategories;

use crate::{
    types::*,
    utils::{apply_to_first, is_title_case},
    Error,
};

mod levenshtein;
mod spell_int {
    #[derive(Debug, Clone, Default, Copy)]
    pub(crate) struct SpellInt(pub(super) u64);

    type FreqType = u8;

    const fn freq_size() -> usize {
        std::mem::size_of::<FreqType>() * 8
    }

    impl SpellInt {
        pub fn as_u64(&self) -> u64 {
            self.0
        }

        pub fn update_freq(&mut self, freq: usize) {
            assert!(freq < FreqType::MAX as usize);

            // erase previous frequency
            self.0 &= u64::MAX - FreqType::MAX as u64;
            // set new frequency
            self.0 |= freq as u64;
        }

        pub fn add_variant(&mut self, index: usize) {
            assert!(index < 64 - freq_size());
            self.0 |= 1 << (freq_size() + index);
        }

        pub fn contains_variant(&self, index: usize) -> bool {
            (self.0 >> (freq_size() + index)) & 1 == 1
        }

        pub fn freq(&self) -> usize {
            (self.0 & FreqType::MAX as u64) as usize
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn can_encode_freq() {
            let mut int = SpellInt::default();
            int.update_freq(100);
            int.add_variant(1);
            int.add_variant(10);

            assert!(int.freq() == 100);
        }

        #[test]
        fn can_encode_variants() {
            let mut int = SpellInt::default();
            int.update_freq(100);
            int.add_variant(1);
            int.add_variant(10);
            int.update_freq(10);

            assert!(int.contains_variant(1));
            assert!(int.contains_variant(10));
            assert!(!int.contains_variant(2));
            assert!(int.freq() == 10);
        }
    }
}

pub(crate) use spell_int::SpellInt;

#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
pub struct Candidate {
    score: f32,
    distance: usize,
    freq: usize,
    term: String,
}
impl Eq for Candidate {}
impl PartialOrd for Candidate {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        // higher score => lower order such that sorting puts highest scores first
        other.score.partial_cmp(&self.score)
    }
}
impl Ord for Candidate {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).expect("scores are never NaN")
    }
}

impl Candidate {
    pub fn score(&self) -> f32 {
        self.score
    }

    pub fn freq(&self) -> usize {
        self.freq
    }

    pub fn distance(&self) -> usize {
        self.distance
    }

    pub fn term(&self) -> &str {
        self.term.as_str()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(default)]
/// TODO
pub struct SpellOptions {
    pub variant: Option<Variant>,
    pub max_distance: usize,
    pub prefix_length: usize,
    pub freq_weight: f32,
    pub top_n: usize,
    pub whitelist: HashSet<String>,
}

pub struct SpellOptionsGuard<'a> {
    spell: &'a mut Spell,
}

impl<'a> Deref for SpellOptionsGuard<'a> {
    type Target = SpellOptions;

    fn deref(&self) -> &Self::Target {
        &self.spell.options
    }
}

impl<'a> DerefMut for SpellOptionsGuard<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.spell.options
    }
}

impl<'a> Drop for SpellOptionsGuard<'a> {
    fn drop(&mut self) {
        self.spell.ingest_options()
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq)]
pub(crate) struct SpellLangOptions {
    /// Variants of the language (e. g. "en_US", "en_GB") to consider for spellchecking.
    pub variants: Vec<Variant>,
    pub split_hyphens: bool,
}

impl Default for SpellOptions {
    fn default() -> Self {
        SpellOptions {
            variant: None,
            max_distance: 2,
            prefix_length: 2,
            freq_weight: 2.,
            top_n: 10,
            whitelist: HashSet::new(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
#[serde(transparent)]
pub struct Variant(String);

impl Variant {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Spell {
    fst: Vec<u8>,
    max_freq: usize,
    map: DefaultHashMap<String, String>,
    lang_options: SpellLangOptions,
    options: SpellOptions,
    // fields below are computed depending on the selected variant
    used_variant: Option<Variant>,
    used_fst: Vec<u8>,
    used_set: DefaultHashSet<String>,
}

impl Spell {
    pub(crate) fn new(
        fst: Vec<u8>,
        max_freq: usize,
        map: DefaultHashMap<String, String>,
        lang_options: SpellLangOptions,
    ) -> Self {
        let mut spell = Spell {
            fst,
            max_freq,
            map,
            lang_options,
            options: SpellOptions::default(),
            ..Default::default()
        };
        spell.ingest_options();
        spell
    }

    pub fn options(&self) -> &SpellOptions {
        &self.options
    }

    pub fn options_mut(&mut self) -> SpellOptionsGuard {
        SpellOptionsGuard { spell: self }
    }

    pub fn variants(&self) -> &[Variant] {
        self.lang_options.variants.as_slice()
    }

    pub fn variant(&self, variant: &str) -> Result<Variant, Error> {
        self.lang_options
            .variants
            .iter()
            .find(|x| x.as_str() == variant)
            .cloned()
            .ok_or_else(|| Error::UnknownVariant(variant.to_owned()))
    }

    pub(crate) fn ingest_options(&mut self) {
        if self.used_variant == self.options.variant {
            return;
        }

        let variant = if let Some(variant) = self.options.variant.as_ref() {
            variant
        } else {
            self.used_variant = None;
            self.used_fst = Vec::new();
            self.used_set = DefaultHashSet::new();
            return;
        };

        let mut used_fst_builder = MapBuilder::memory();
        let mut set = DefaultHashSet::new();

        let fst = Map::new(&self.fst).expect("serialized fst must be valid.");
        let mut stream = fst.into_stream();

        let variant_index = self
            .variants()
            .iter()
            .position(|x| x == variant)
            .expect("only valid variants are created.");

        while let Some((k, v)) = stream.next() {
            if SpellInt(v).contains_variant(variant_index) {
                set.insert(String::from_utf8(k.to_vec()).expect("fst keys must be valid utf-8."));
                used_fst_builder
                    .insert(k, v)
                    .expect("fst stream returns values in lexicographic order.");
            }
        }

        self.used_variant = Some(variant.clone());
        self.used_fst = used_fst_builder
            .into_inner()
            .expect("subset of valid fst must be valid.");
        self.used_set = set;
    }

    fn check_flat(&self, word: &str) -> bool {
        self.used_variant.is_none()
            || word.is_empty()
            || word.chars().all(|x| x.is_punctuation() || x.is_numeric())
            || self.used_set.contains(word)
            || (is_title_case(word)
                && self.check(&apply_to_first(word, |x| x.to_lowercase().collect())))
    }

    pub fn check(&self, word: &str) -> bool {
        self.check_flat(word)
            || (self.lang_options.split_hyphens
                && word
                    .split(&['-', '\u{2010}', '\u{2011}'][..])
                    .all(|x| self.check_flat(x)))
    }

    pub fn search(&self, word: &str) -> Vec<Candidate> {
        if let Some(candidate) = self.map.get(word) {
            return vec![Candidate {
                score: 0., // numerical values here do not matter since there is always exactly one candidate - ranking is irrelevant
                freq: 0,
                distance: 0,
                term: candidate.to_owned(),
            }];
        }

        let used_fst = Map::new(self.used_fst.as_slice()).expect("used fst must be valid.");
        let query = levenshtein::Levenshtein::new(word, self.options.max_distance, 2);

        let mut out = BinaryHeap::with_capacity(self.options.top_n);

        let mut stream = used_fst.search_with_state(query).into_stream();
        while let Some((k, v, s)) = stream.next() {
            let state = s.expect("matching levenshtein state is always `Some`.");
            assert!(state.dist() > 0);

            let id = SpellInt(v);

            let term = String::from_utf8(k.to_vec()).expect("fst keys must be valid utf-8.");
            out.push(Candidate {
                distance: state.dist(),
                freq: id.freq(),
                term,
                score: (self.options.max_distance - state.dist()) as f32
                    + id.freq() as f32 / self.max_freq as f32 * self.options.freq_weight,
            });
            if out.len() > self.options.top_n {
                out.pop();
            }
        }

        out.into_sorted_vec()
    }

    pub fn suggest(&self, tokens: &[Token]) -> Vec<Suggestion> {
        let mut suggestions = Vec::new();

        for token in tokens {
            let text = token.word.text.as_ref();

            if token.ignore_spelling || self.check(text) {
                continue;
            }

            let candidates = self.search(text);
            suggestions.push(Suggestion {
                source: "SPELLCHECK/SINGLE".into(),
                message: "Possibly misspelled word.".into(),
                start: token.char_span.0,
                end: token.char_span.1,
                replacements: candidates.into_iter().map(|x| x.term).collect(),
            });
        }

        suggestions
    }
}
