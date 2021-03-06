use std::{
    collections::HashSet,
    ops::{Deref, DerefMut},
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc, RwLock, RwLockReadGuard,
    },
};

use fst::{IntoStreamer, Map, MapBuilder, Streamer};
use serde::{Deserialize, Serialize};

use crate::types::*;

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
            self.0 = self.0 & (u64::MAX - FreqType::MAX as u64);
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

#[derive(Debug, Clone, Default, PartialEq, PartialOrd)]
struct Candidate {
    pub score: f32,
    pub term: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(default)]
/// TODO
pub struct SpellcheckerOptions {
    pub variant: Option<String>,
    pub max_distance: usize,
    pub prefix: usize,
    pub frequency_weight: f32,
    pub n_suggestions: usize,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq)]
pub(crate) struct SpellcheckerLangOptions {
    /// Variants of the language (e. g. "en_US", "en_GB") to consider for spellchecking.
    pub variants: Vec<String>,
}

impl Default for SpellcheckerOptions {
    fn default() -> Self {
        SpellcheckerOptions {
            variant: None,
            max_distance: 2,
            prefix: 2,
            frequency_weight: 2.,
            n_suggestions: 10,
        }
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub(crate) struct Spellchecker {
    pub(crate) fst: Vec<u8>,
    pub(crate) max_freq: usize,
    pub(crate) lang_options: SpellcheckerLangOptions,
    pub(crate) used_variant: Arc<AtomicUsize>,
    pub(crate) used: Arc<RwLock<(Vec<u8>, HashSet<String>)>>,
}

impl Spellchecker {
    fn update_used_fst(
        &self,
        options: &SpellcheckerOptions,
    ) -> Option<RwLockReadGuard<'_, (Vec<u8>, HashSet<String>)>> {
        let variant = if let Some(variant) = options.variant.as_ref() {
            variant.as_str()
        } else {
            return None;
        };
        let variant_index = self
            .lang_options
            .variants
            .iter()
            .position(|x| x == variant)?;

        if self.used_variant.swap(variant_index, Ordering::Relaxed) != variant_index {
            let mut used_fst_builder = MapBuilder::memory();
            let mut set = DefaultHashSet::new();

            let fst = Map::new(&self.fst).expect("serialized fst must be valid.");
            let mut stream = fst.into_stream();

            while let Some((k, v)) = stream.next() {
                if SpellInt(v).contains_variant(variant_index) {
                    set.insert(
                        String::from_utf8(k.to_vec()).expect("fst keys must be valid utf-8."),
                    );
                    used_fst_builder
                        .insert(k, v)
                        .expect("fst stream returns values in lexicographic order.");
                }
            }

            let mut guard = self.used.write().expect("lock must not be poisoned.");
            let (used_fst, used_set) = guard.deref_mut();

            *used_fst = used_fst_builder
                .into_inner()
                .expect("subset of valid fst must be valid.");
            *used_set = set;
        }

        Some(self.used.read().expect("lock must not be poisoned"))
    }

    fn lookup(&self, token: &Token, options: &SpellcheckerOptions) -> Option<Vec<Candidate>> {
        let guard = self.update_used_fst(options)?;
        let (used_fst, used_set) = guard.deref();
        let used_fst = Map::new(used_fst).expect("used fst must be valid.");

        let text = token.word.text.as_ref();
        // no text => nothing to correct, only the case for special tokens (e.g. SENT_START)
        if text.is_empty() || used_set.contains(text) {
            return None;
        }

        let query = levenshtein::Levenshtein::new(text, options.max_distance, 2);

        let mut out = Vec::new();

        let mut stream = used_fst.search_with_state(query).into_stream();
        while let Some((k, v, s)) = stream.next() {
            let state = s.expect("matching levenshtein state is always `Some`.");
            if state.dist() == 0 {
                return None;
            }

            let id = SpellInt(v);

            let string = String::from_utf8(k.to_vec()).expect("fst keys must be valid utf-8.");
            out.push(Candidate {
                score: (options.max_distance - state.dist()) as f32
                    + id.freq() as f32 / self.max_freq as f32 * options.frequency_weight,
                term: string,
            })
        }

        // we want higher scores first
        out.sort_by(|a, b| b.partial_cmp(a).expect("candidate scores are never NaN."));
        Some(out)
    }

    pub fn suggest(&self, tokens: &[Token], options: &SpellcheckerOptions) -> Vec<Suggestion> {
        let mut suggestions = Vec::new();

        for token in tokens {
            if let Some(candidates) = self.lookup(token, options) {
                // TODO: disallow empty / properly treat empty
                suggestions.push(Suggestion {
                    source: "SPELLCHECK/SINGLE".into(),
                    message: "Possibly misspelled word.".into(),
                    start: token.char_span.0,
                    end: token.char_span.1,
                    replacements: candidates
                        .into_iter()
                        .map(|x| x.term.to_owned())
                        .take(options.n_suggestions)
                        .collect(),
                })
            }
        }

        suggestions
    }
}
