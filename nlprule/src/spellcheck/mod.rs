use std::{
    ops::Deref,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc, RwLock, RwLockReadGuard,
    },
};

use fst::{IntoStreamer, Map, MapBuilder, Streamer};
use serde::{Deserialize, Serialize};

use crate::types::*;

mod levenshtein;

#[derive(Debug, Clone, Default, Copy)]
pub(crate) struct SpellInt(u64);

impl SpellInt {
    pub fn as_u64(&self) -> u64 {
        self.0
    }

    pub fn update_freq(&mut self, freq: usize) {
        assert!(freq < u32::MAX as usize);

        // erase previous frequency
        self.0 = self.0 & (u64::MAX - u32::MAX as u64);
        // set new frequency
        self.0 |= freq as u64;
    }

    pub fn add_variant(&mut self, index: usize) {
        assert!(index < 32);
        self.0 |= 1 << (32 + index);
    }

    pub fn contains_variant(&self, index: usize) -> bool {
        (self.0 >> (32 + index)) & 1 == 1
    }

    pub fn freq(&self) -> usize {
        (self.0 & u32::MAX as u64) as usize
    }
}

#[derive(Debug, Clone, Default, PartialEq, PartialOrd)]
struct Candidate {
    pub score: f32,
    pub term: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
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
    pub(crate) used_fst: Arc<RwLock<Vec<u8>>>,
}

impl Spellchecker {
    fn update_used_fst(
        &self,
        options: &SpellcheckerOptions,
    ) -> Option<RwLockReadGuard<'_, Vec<u8>>> {
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

            let fst = Map::new(&self.fst).expect("serialized fst must be valid.");
            let mut stream = fst.into_stream();

            while let Some((k, v)) = stream.next() {
                if SpellInt(v).contains_variant(variant_index) {
                    used_fst_builder
                        .insert(k, v)
                        .expect("fst stream returns values in lexicographic order.");
                }
            }

            let mut guard = self.used_fst.write();
            let used_fst = guard.as_deref_mut().expect("lock must not be poisoned.");

            *used_fst = used_fst_builder
                .into_inner()
                .expect("subset of valid fst must be valid.");
        }

        Some(self.used_fst.read().expect("lock must not be poisoned"))
    }

    fn lookup(&self, token: &Token, options: &SpellcheckerOptions) -> Option<Vec<Candidate>> {
        let guard = self.update_used_fst(options)?;
        let used_fst = Map::new(guard.deref()).expect("used fst must be valid.");

        let text = token.word.text.as_ref();
        // no text => nothing to correct, only the case for special tokens (e.g. SENT_START)
        if text.is_empty() {
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
