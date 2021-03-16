//! Structures and implementations related to spellchecking.
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
    use std::cmp;

    use serde::{Deserialize, Serialize};

    /// Encodes information about a valid word in a `u64` for storage as value in an FST.
    /// Currently:
    /// - the bottom 8 bits encode the frequency
    /// - the other 56 bits act as flags for the variants e.g. bit 10 and 12 are set if the word exists in the the second and fourth variant.
    #[derive(Debug, Clone, Default, Copy, Serialize, Deserialize)]
    pub(crate) struct SpellInt(pub(super) u64);

    type FreqType = u8;

    const fn freq_size() -> usize {
        std::mem::size_of::<FreqType>() * 8
    }

    #[allow(dead_code)] // some methods are only needed for compilation - kept here for clarity
    impl SpellInt {
        pub fn as_u64(&self) -> u64 {
            self.0
        }

        pub fn update_freq(&mut self, freq: usize) {
            assert!(freq < FreqType::MAX as usize);

            let prev_freq = self.freq();
            // erase previous frequency
            self.0 &= u64::MAX - FreqType::MAX as u64;
            // set new frequency, strictly speaking we would have to store a frequency for each variant
            // but that would need significantly more space, so we just store the highest frequency
            self.0 |= cmp::max(prev_freq, freq) as u64;
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
            assert!(int.freq() == 100);
        }
    }
}

pub(crate) use spell_int::SpellInt;

#[derive(Debug, Clone, Default, PartialEq, Serialize, Deserialize)]
struct Candidate {
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

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(default)]
/// Options to configure the spellchecker.
pub struct SpellOptions {
    /// The language variant to use. Setting this to `None` disables spellchecking.
    pub variant: Option<Variant>,
    /// The maximum edit distance to consider for corrections. Currently Optimal String Alignment distance is used.
    pub max_distance: usize,
    /// A fixed prefix length for which to consider only edits with a distance of 1. This speeds up the search by pruning the tree early.
    pub prefix_length: usize,
    /// How high to weigh the frequency of a word compared to the edit distance when ranking correction candidates.
    /// Setting this to `x` makes the frequency make a difference of at most `x` edit distance.
    pub freq_weight: f32,
    /// The maximum number of correction candidates to return.
    pub top_n: usize,
    /// A set of words to ignore. Can also contain phrases delimited by a space.
    pub whitelist: HashSet<String>,
}

/// A guard around the [SpellOptions]. Makes sure the spellchecker is updated once this is dropped.
/// Implements `Deref` and `DerefMut` to the [SpellOptions].
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
    /// Variants of the language (e.g. "en_US", "en_GB") to consider for spellchecking.
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

/// A valid language variant. Obtained by [Spell::variant].
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
#[serde(transparent)]
pub struct Variant(String);

impl Variant {
    /// Gets the language code of this variant.
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

/// Spellchecker logic for one variant. Does the actual work.
#[derive(Debug, Clone)]
pub(crate) struct VariantChecker {
    variant: Variant,
    fst: Vec<u8>,
    max_freq: usize,
    multiwords: DefaultHashMap<String, Vec<Vec<String>>>,
    set: DefaultHashSet<String>,
    map: DefaultHashMap<String, String>,
    lang_options: SpellLangOptions,
    options: SpellOptions,
}

impl VariantChecker {
    /// Checks the validity of one word.
    /// NB: The ordering of this chain of `||` operators is somewhat nontrivial. Could potentially be improved by benchmarking.
    /// If this is true, the token is always correct. The converse is not true because e.g. multiwords are checked separately.
    fn check_word(&self, word: &str, recurse: bool) -> bool {
        word.is_empty()
            || self.set.contains(word)
            || word
                .chars()
                .all(|x| x.is_symbol() || x.is_punctuation() || x.is_numeric())
            || (recurse
                // for title case words, it is enough if the lowercase variant is known.
                // it is possible that `is_title_case` is still true for word where `.to_lowercase()` was called so we need a `recurse` parameter.
                && is_title_case(word)
                && self.check_word(&apply_to_first(word, |x| x.to_lowercase().collect()), false))
    }

    /// Populates `correct_mask` according to the correctness of the given zeroth token.
    /// - `correct_mask[0]` is `true` if the zeroth token is correct, `false` if it is not correct.
    /// - Indices `1..n` of `correct_mask` are `true` if the `n`th token is also definitely correct.
    ///     If they are `false`, they need to be checked separately.  
    fn check(&self, tokens: &[Token], correct_mask: &mut [bool]) {
        let word = tokens[0].word.text.as_ref();
        let mut word_is_correct = self.check_word(word, true);

        if !word_is_correct && self.lang_options.split_hyphens {
            // there exist multiple valid hyphens, see https://jkorpela.fi/dashes.html
            let hyphens = &['-', '\u{2010}', '\u{2011}'][..];

            if word.contains(hyphens) && word.split(hyphens).all(|x| self.check_word(x, true)) {
                word_is_correct = true;
            }
        }

        correct_mask[0] = word_is_correct;

        if let Some(continuations) = self.multiwords.get(word) {
            if let Some(matching_cont) = continuations.iter().find(|cont| {
                // important: an empty continuation matches! so single words can also validly be part of `multiwords`
                (tokens.len() - 1) >= cont.len()
                    && cont
                        .iter()
                        .enumerate()
                        .all(|(i, x)| tokens[i + 1].word.text.as_ref() == x)
            }) {
                correct_mask[..1 + matching_cont.len()]
                    .iter_mut()
                    .for_each(|x| *x = true);
            }
        }
    }

    fn search(&self, word: &str) -> Vec<String> {
        if let Some(candidate) = self.map.get(word) {
            return vec![candidate.to_owned()];
        }

        let used_fst = Map::new(self.fst.as_slice()).expect("used fst must be valid.");
        let query = levenshtein::Levenshtein::new(word, self.options.max_distance, 2);

        let mut out = BinaryHeap::with_capacity(self.options.top_n);

        let mut stream = used_fst.search_with_state(query).into_stream();
        while let Some((k, v, s)) = stream.next() {
            let state = s.expect("matching levenshtein state is always `Some`.");

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

        // `into_iter_sorted` is unstable - see https://github.com/rust-lang/rust/issues/59278
        out.into_sorted_vec().into_iter().map(|x| x.term).collect()
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
/// A spellchecker implementing the algorithm described in [Error-tolerant Finite State Recognition](https://www.aclweb.org/anthology/1995.iwpt-1.24/) with some extensions.
pub struct Spell {
    /// An FST mapping valid words (always single tokens!) to a [SpellInt].
    pub(crate) fst: Vec<u8>,
    /// Known *multiwords* i. e. phrases. Can also validly contain single words if they should not be part of the FST (e.g. words in the whitelist).
    pub(crate) multiwords: DefaultHashMap<String, Vec<(Vec<String>, SpellInt)>>,
    ///  The maximum occured word frequency. Used to normalize.
    pub(crate) max_freq: usize,
    /// A map of `wrong->right`. `wrong` must always be exactly one token.
    pub(crate) map: DefaultHashMap<String, String>,
    pub(crate) lang_options: SpellLangOptions,
    pub(crate) options: SpellOptions,
    /// The structure containing the actual spellchecking logic. Computed based on the selected variant.
    #[serde(skip)]
    pub(crate) variant_checker: Option<VariantChecker>,
}

impl Spell {
    /// Gets the options.
    pub fn options(&self) -> &SpellOptions {
        &self.options
    }

    /// Mutably gets the options.
    pub fn options_mut(&mut self) -> SpellOptionsGuard {
        SpellOptionsGuard { spell: self }
    }

    /// Returns all known variants.
    pub fn variants(&self) -> &[Variant] {
        self.lang_options.variants.as_slice()
    }

    /// Returns the variant for a language code e.g. `"en_GB"`.
    /// # Errors
    /// - If no variant exists for the language code.
    pub fn variant(&self, variant: &str) -> Result<Variant, Error> {
        self.lang_options
            .variants
            .iter()
            .find(|x| x.as_str() == variant)
            .cloned()
            .ok_or_else(|| {
                Error::UnknownVariant(
                    variant.to_owned(),
                    self.lang_options
                        .variants
                        .iter()
                        .map(|x| x.as_str().to_owned())
                        .collect(),
                )
            })
    }

    pub(crate) fn ingest_options(&mut self) {
        let variant = if let Some(variant) = self.options.variant.as_ref() {
            variant.clone()
        } else {
            self.variant_checker = None;
            return;
        };

        let variant_index = self
            .variants()
            .iter()
            .position(|x| *x == variant)
            .expect("only valid variants are created.");

        let mut checker = match self.variant_checker.take() {
            // if the variant checker exists and uses the correct variant, we don't need to rebuild
            Some(checker) if checker.variant == variant => checker,
            _ => {
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

                let fst = used_fst_builder
                    .into_inner()
                    .expect("subset of valid fst must be valid.");

                VariantChecker {
                    variant,
                    fst,
                    multiwords: DefaultHashMap::new(),
                    set,
                    map: self.map.clone(),
                    max_freq: self.max_freq,
                    options: self.options.clone(),
                    lang_options: self.lang_options.clone(),
                }
            }
        };

        // `multiwords` depend on the whitelist. For convenience we always rebuild this.
        // the whitelist could be separated into a new structure for a speedup.
        // We can revisit this if performance becomes an issue, it should still be quite fast as implemented now.

        // selects only the multiwords which exist for the selected variant
        let mut multiwords: DefaultHashMap<_, _> = self
            .multiwords
            .iter()
            .map(|(key, value)| {
                let value = value
                    .iter()
                    .filter_map(|(continuations, int)| {
                        if int.contains_variant(variant_index) {
                            Some(continuations)
                        } else {
                            None
                        }
                    })
                    .cloned()
                    .collect();
                (key.to_owned(), value)
            })
            .collect();

        // adds words from the user-set whitelist
        // careful: words in the `whitelist` are set by the user, so this must never fail!
        for phrase in self
            .options
            .whitelist
            .iter()
            .map(|x| x.as_str())
            // for some important words we have to manually make sure they are ignored :)
            .chain(vec!["nlprule", "Minixhofer"])
        {
            let mut parts = phrase.trim().split_whitespace();

            let first = if let Some(first) = parts.next() {
                first
            } else {
                // silently ignore empty words
                continue;
            };

            multiwords
                .entry(first.to_owned())
                .or_insert_with(Vec::new)
                .push(parts.map(|x| x.to_owned()).collect());
        }

        checker.multiwords = multiwords;
        self.variant_checker = Some(checker);
    }

    /// Runs the spellchecking algorithm on all tokens and returns suggestions.
    pub fn suggest(&self, tokens: &[Token]) -> Vec<Suggestion> {
        let variant_checker = if let Some(checker) = self.variant_checker.as_ref() {
            checker
        } else {
            return Vec::new();
        };

        let mut suggestions = Vec::new();
        let mut correct_mask = vec![false; tokens.len()];

        for (i, token) in tokens.iter().enumerate() {
            let text = token.word.text.as_ref();

            if !correct_mask[i] {
                variant_checker.check(&tokens[i..], &mut correct_mask[i..]);
            }
            if correct_mask[i] || token.ignore_spelling {
                continue;
            }

            suggestions.push(Suggestion {
                source: "SPELLCHECK/SINGLE".into(),
                message: "Possibly misspelled word.".into(),
                start: token.char_span.0,
                end: token.char_span.1,
                replacements: variant_checker.search(text),
            });
        }

        suggestions
    }
}
