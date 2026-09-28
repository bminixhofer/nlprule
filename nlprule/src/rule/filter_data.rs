//! Serializable data used by the Java-class runtime filters
//! ([crate::rule::filter_java]): speller dictionaries, multitoken
//! suggestion tables, confusion pairs and compound part lists.
//!
//! All of this is built at compile time from LanguageTool's resources and
//! stored once per language (on [crate::rules::Rules]), mirroring how LT
//! keeps these as static instances in the filter classes.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// A speller vocabulary: an FST over lowercased word forms mapping to the
/// index of one original-cased form. Approximates LT's `MorfologikSpeller`.
#[derive(Serialize, Deserialize, Default, Clone)]
pub struct SpellerDict {
    /// `fst::Map` data: lowercased form -> `(index << 5) | frequency`,
    /// where frequency is morfologik's 0..=25 rank (higher = more frequent).
    /// For case-sensitive dictionaries (pl: `fsa.dict.encoder=none`, no
    /// conversion) the keys keep their original case instead.
    pub map_bytes: Vec<u8>,
    /// Original-cased forms (deduplicated).
    pub forms: Vec<String>,
    /// LT dictionaries without case conversion (pl_PL) keep proper-noun
    /// casing: "Bledem" is a key but "bledem" must still be misspelled.
    /// Not serialized (bincode has no field versioning): the loader sets it
    /// from the language code via [Rules::with_lang].
    #[serde(skip)]
    pub case_sensitive: bool,
}

impl SpellerDict {
    pub fn is_empty(&self) -> bool {
        self.forms.is_empty()
    }

    fn map(&self) -> fst::Map<&[u8]> {
        fst::Map::new(&self.map_bytes[..]).expect("speller FST must be valid")
    }

    /// Whether `word` is in the speller vocabulary (case-insensitively).
    pub fn is_known(&self, word: &str) -> bool {
        if word.is_empty() {
            return false;
        }
        if self.case_sensitive {
            // exact-case lookup first; capitalized common words at sentence
            // start still resolve via the lowercase fallback
            if self.map().get(word).is_some() {
                return true;
            }
            let lower = word.to_lowercase();
            lower != word && self.map().get(&lower).is_some()
        } else {
            let lower = word.to_lowercase();
            self.map().get(&lower).is_some()
        }
    }

    fn unpack(value: u64) -> (usize, u8) {
        (((value >> 5) as usize), (value & 0x1F) as u8)
    }

    /// Finds words within `max_distance` Damerau-Levenshtein (OSA) edits of
    /// `word`, ordered by (distance, form). Mirrors `MorfologikSpeller.findSimilarWords`.
    ///
    /// Candidates come from the FST's Levenshtein automaton (a superset of the
    /// Damerau neighborhood; each candidate is verified with the true
    /// restricted-Damerau distance). Ordered like morfologik's `Speller`:
    /// by edit distance, then by descending dictionary frequency; ties keep
    /// the (alphabetical) automaton order.
    pub fn find_similar(&self, word: &str, max_distance: usize) -> Vec<String> {
        self.find_similar_opt(word, max_distance, false)
    }

    /// Like [SpellerDict::find_similar] with morfologik's `ignore-diacritics`
    /// option: comparisons run on diacritics-stripped forms.
    pub fn find_similar_ignoring_diacritics(
        &self,
        word: &str,
        max_distance: usize,
    ) -> Vec<String> {
        self.find_similar_opt(word, max_distance, true)
    }

    fn find_similar_opt(
        &self,
        word: &str,
        max_distance: usize,
        ignore_diacritics: bool,
    ) -> Vec<String> {
        use fst::automaton::Levenshtein;
        use fst::{Automaton, IntoStreamer, Streamer};

        let lower = if ignore_diacritics {
            remove_diacritics(&word.to_lowercase())
        } else {
            word.to_lowercase()
        };
        let mut out: Vec<(usize, u8, String)> = Vec::new();
        let map = self.map();

        let dist = |a: &str, b: &str| -> usize {
            if ignore_diacritics {
                osa_distance(&remove_diacritics(a), &remove_diacritics(b))
            } else {
                osa_distance(a, b)
            }
        };

        // the fst automaton is byte-based and matches the char-level
        // neighborhood for ASCII queries; long words at distance 3 can
        // exceed the automaton's state cap -> fall back to the full scan
        let automaton = if lower.is_ascii() && !ignore_diacritics {
            Levenshtein::new(&lower, max_distance as u32).ok()
        } else {
            None
        };

        if let Some(automaton) = automaton {
            debug_assert!(!ignore_diacritics || lower.chars().all(|c| c.is_ascii()));
            let mut stream = map.search(automaton).into_stream();
            while let Some((key, value)) = stream.next() {
                let key = String::from_utf8_lossy(key).into_owned();
                let d = dist(&lower, &key);
                if d > 0 && d <= max_distance {
                    let (idx, freq) = Self::unpack(value);
                    if let Some(form) = self.forms.get(idx) {
                        out.push((d, freq, form.clone()));
                    }
                }
            }
        } else {
            // non-ASCII queries (or diacritics-insensitive comparisons):
            // full scan with a cheap length pre-filter
            let lower_len = lower.chars().count();
            let mut stream = map.into_stream();
            while let Some((key, value)) = stream.next() {
                let key = String::from_utf8_lossy(key).into_owned();
                if key.chars().count().abs_diff(lower_len) > max_distance {
                    continue;
                }
                let d = dist(&lower, &key);
                if d > 0 && d <= max_distance {
                    let (idx, freq) = Self::unpack(value);
                    if let Some(form) = self.forms.get(idx) {
                        out.push((d, freq, form.clone()));
                    }
                }
            }
        }

        // stable sort: distance asc, frequency desc
        out.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| b.1.cmp(&a.1)));
        out.dedup_by(|a, b| a.2 == b.2);
        out.into_iter().map(|(_, _, form)| form).collect()
    }
}

/// Restricted Damerau-Levenshtein (optimal string alignment) distance.
pub fn osa_distance(a: &str, b: &str) -> usize {
    let a: Vec<char> = a.chars().collect();
    let b: Vec<char> = b.chars().collect();
    let (n, m) = (a.len(), b.len());

    if n == 0 {
        return m;
    }
    if m == 0 {
        return n;
    }

    let mut prev_prev = vec![0usize; m + 1];
    let mut prev: Vec<usize> = (0..=m).collect();
    let mut curr = vec![0usize; m + 1];

    for i in 1..=n {
        curr[0] = i;
        for j in 1..=m {
            let cost = if a[i - 1] == b[j - 1] { 0 } else { 1 };
            let mut d = (curr[j - 1] + 1).min(prev[j] + 1).min(prev[j - 1] + cost);
            if i > 1 && j > 1 && a[i - 1] == b[j - 2] && a[i - 2] == b[j - 1] {
                d = d.min(prev_prev[j - 2] + 1);
            }
            curr[j] = d;
        }
        prev_prev.copy_from_slice(&prev);
        prev.copy_from_slice(&curr);
    }

    prev[m]
}

/// Plain Levenshtein distance (as used by `MultitokenSpeller`).
pub fn levenshtein(a: &str, b: &str) -> usize {
    let a: Vec<char> = a.chars().collect();
    let b: Vec<char> = b.chars().collect();
    let (n, m) = (a.len(), b.len());

    if n == 0 {
        return m;
    }
    if m == 0 {
        return n;
    }

    let mut prev: Vec<usize> = (0..=m).collect();
    let mut curr = vec![0usize; m + 1];

    for i in 1..=n {
        curr[0] = i;
        for j in 1..=m {
            let cost = if a[i - 1] == b[j - 1] { 0 } else { 1 };
            curr[j] = (curr[j - 1] + 1).min(prev[j] + 1).min(prev[j - 1] + cost);
        }
        std::mem::swap(&mut prev, &mut curr);
    }

    prev[m]
}

/// Mirrors LT's `MultitokenSpeller`: a naive multiword speller that provides
/// suggestions for multitoken expressions from word lists.
#[derive(Serialize, Deserialize, Default)]
pub struct MultitokenSuggester {
    /// first char of the normalized key -> (normalized key -> original lines)
    pub by_char: HashMap<char, HashMap<String, Vec<String>>>,
    /// normalized key without spaces -> original lines
    pub no_spaces: HashMap<String, Vec<String>>,
}

impl MultitokenSuggester {
    pub fn is_empty(&self) -> bool {
        self.no_spaces.is_empty() && self.by_char.is_empty()
    }

    /// Normalization used for keys: lowercase, remove diacritics, `-` -> space.
    pub fn normalize_key(word: &str) -> String {
        remove_diacritics(&word.to_lowercase()).replace('-', " ")
    }
}

/// Removes common Latin diacritics (mirrors `StringTools.removeDiacritics`
/// closely enough for key normalization).
pub fn remove_diacritics(s: &str) -> String {
    s.chars()
        .flat_map(|c| match c {
            'á' | 'à' | 'â' | 'ä' | 'ã' | 'ā' => "a".chars().collect::<Vec<_>>(),
            'é' | 'è' | 'ê' | 'ë' | 'ē' => "e".chars().collect(),
            'í' | 'ì' | 'î' | 'ï' | 'ī' => "i".chars().collect(),
            'ó' | 'ò' | 'ô' | 'ö' | 'õ' | 'ō' => "o".chars().collect(),
            'ú' | 'ù' | 'û' | 'ü' | 'ū' => "u".chars().collect(),
            'ý' | 'ÿ' => "y".chars().collect(),
            'ñ' => "n".chars().collect(),
            'ç' => "c".chars().collect(),
            'š' => "s".chars().collect(),
            'ž' => "z".chars().collect(),
            'Á' | 'À' | 'Â' | 'Ä' | 'Ã' => "a".chars().collect(),
            'É' | 'È' | 'Ê' | 'Ë' => "e".chars().collect(),
            'Í' | 'Ì' | 'Î' | 'Ï' => "i".chars().collect(),
            'Ó' | 'Ò' | 'Ô' | 'Ö' | 'Õ' => "o".chars().collect(),
            'Ú' | 'Ù' | 'Û' | 'Ü' => "u".chars().collect(),
            'Ý' => "y".chars().collect(),
            'Ñ' => "n".chars().collect(),
            'Ç' => "c".chars().collect(),
            'Š' => "s".chars().collect(),
            'Ž' => "z".chars().collect(),
            _ => c.to_string().chars().collect(),
        })
        .collect()
}

/// Whether the string contains any character with a diacritic.
pub fn has_diacritics(s: &str) -> bool {
    let lower = s.to_lowercase();
    lower != remove_diacritics(&lower)
}

/// All auxiliary data the Java filters need, built per language at compile time.
#[derive(Serialize, Deserialize, Default)]
pub struct FilterData {
    pub speller: Option<SpellerDict>,
    pub multitoken: Option<MultitokenSuggester>,
    /// `wrong-form` (lowercased) -> `[(correct form, postag)]`
    pub confusion_pairs: Option<HashMap<String, Vec<(String, String)>>>,
    /// German `addedCompound.txt`: part1 (lowercase) -> [part2 (lowercase)]
    pub added_compound: Option<HashMap<String, Vec<String>>>,
    /// Whether the MultitokenSpellerFilter should additionally check the
    /// plain speller (LT does this for en/de/pt/nl).
    #[serde(default)]
    pub multitoken_speller_check: bool,
    /// LT `AbstractSimpleReplaceRule2` table (`filters/replace.txt`):
    /// phrase-level `wrong=correct` pairs, matched case-sensitively with
    /// sub-rule ids derived from the wrong phrase (e.g.
    /// `NL_SIMPLE_REPLACE_DUR`).
    #[serde(default)]
    pub simple_replace: Option<SimpleReplaceTable>,
}

/// Wrong phrases indexed by their first word, with the id prefix the
/// language's SimpleReplaceRule uses (`NL_SIMPLE_REPLACE` for nl).
fn default_true() -> bool { true }

#[derive(Serialize, Deserialize, Debug, Default, Clone)]
pub struct SimpleReplaceTable {
    pub prefix: String,
    /// Whether sub-rule ids get the wrong-phrase suffix (nl's
    /// AbstractSimpleReplaceRule2 does, en's older one does not)
    #[serde(default = "default_true")]
    pub sub_ids: bool,
    /// first word of the wrong phrase -> [(wrong phrase, correct phrases)]
    pub by_first_word: HashMap<String, Vec<(String, Vec<String>)>>,
}
