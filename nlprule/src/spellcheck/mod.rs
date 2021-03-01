use std::{
    cmp,
    hash::{Hash, Hasher},
};

use appendlist::AppendList;
use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};
use triple_accel::levenshtein;

use crate::{tokenizer::tag::Tagger, types::*};

fn hash<H: Hash>(string: H) -> u64 {
    let mut hasher = DefaultHasher::new();
    string.hash(&mut hasher);
    hasher.finish()
}

fn distance(a: usize, b: usize) -> usize {
    if a > b {
        a - b
    } else {
        b - a
    }
}

#[derive(Debug, Clone, Default, PartialEq, PartialOrd, Ord, Eq)]
pub struct Candidate<'a> {
    pub distance: usize,
    pub freq: u8,
    pub term: &'a str,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SpellcheckOptions {
    max_dictionary_distance: usize,
    prefix_length: usize,
}

impl Default for SpellcheckOptions {
    fn default() -> Self {
        SpellcheckOptions {
            max_dictionary_distance: 2,
            prefix_length: 7,
        }
    }
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Spellchecker {
    deletes: DefaultHashMap<u64, Vec<WordIdInt>>,
    max_length: usize,
    options: SpellcheckOptions,
}

lazy_static! {
    static ref EMPTY_HASH: u64 = {
        let empty: &[u8] = &[];
        hash(empty)
    };
}

impl Spellchecker {
    fn deletes_prefix(key: &[u8], options: &SpellcheckOptions) -> DefaultHashSet<u64> {
        let mut out = DefaultHashSet::new();

        if key.len() <= options.max_dictionary_distance {
            out.insert(*EMPTY_HASH);
        }

        Self::deletes_recurse(&key[..options.prefix_length], 0, &mut out, options);
        out.insert(hash(key));

        out
    }

    fn deletes_recurse(
        bytes: &[u8],
        distance: usize,
        out: &mut DefaultHashSet<u64>,
        options: &SpellcheckOptions,
    ) {
        if bytes.len() > 1 {
            for i in 0..bytes.len() {
                let mut delete = bytes.to_vec();
                delete.remove(i);

                if distance + 1 < options.max_dictionary_distance {
                    Self::deletes_recurse(&delete, distance + 1, out, options);
                }

                out.insert(hash(delete));
            }
        }
    }

    pub fn new(tagger: &Tagger, options: SpellcheckOptions) -> Self {
        let mut deletes = DefaultHashMap::new();
        let mut max_length = 0;

        for (word, id) in tagger.word_store() {
            for delete in Self::deletes_prefix(word.as_bytes(), &options) {
                deletes.entry(delete).or_insert_with(Vec::new).push(*id);
            }
            max_length = cmp::max(word.len(), max_length);
        }

        Spellchecker {
            deletes,
            max_length,
            options,
        }
    }

    pub fn lookup<'t>(&self, token: &'t Token, max_distance: usize) -> Option<Vec<Candidate<'t>>> {
        if token.word.text.id().is_some() {
            return None;
        }

        let word = token.word.text.0.as_ref();
        let input_length = word.len();

        if input_length - self.options.max_dictionary_distance > self.max_length {
            return Some(Vec::new());
        }

        let mut candidates = Vec::new();

        // deletes we've considered already
        let mut known_deletes: DefaultHashSet<u64> = DefaultHashSet::new();
        // suggestions we've considered already
        let mut known_suggestions: DefaultHashSet<WordIdInt> = DefaultHashSet::new();

        let mut candidate_index = 0;
        let deletes: AppendList<Vec<u8>> = AppendList::new();

        let input_prefix_length = cmp::min(input_length, self.options.prefix_length);
        deletes.push(word.as_bytes()[..input_prefix_length].to_vec());

        while candidate_index < deletes.len() {
            let candidate = deletes[candidate_index].as_slice();
            let candidate_length = candidate.len();

            candidate_index += 1;

            let length_diff = input_prefix_length - candidate_length;

            if let Some(suggestions) = self.deletes.get(&hash(candidate)) {
                for suggestion_id in suggestions {
                    let suggestion = token.tagger.str_for_word_id(suggestion_id);
                    let suggestion_length = suggestion.len();

                    if distance(suggestion_length, input_length) > max_distance
                // suggestion must be for a different delete string, in same bin only because of hash collision
                    || suggestion_length < candidate_length
                // in the same bin only because of hash collision, a valid suggestion is always longer than the delete
                    || suggestion_length == candidate_length
                // we already added the suggestion
                    || known_suggestions.contains(suggestion_id)
                    {
                        continue;
                    }

                    // SymSpell.cs covers some additional cases here where it is not necessary to compute the edit distance
                    // would have to be benchmarked if they are worth it considering `triple_accel` is presumably faster than
                    // the C# implementation of edit distance

                    let distance = levenshtein(suggestion.as_bytes(), word.as_bytes()) as usize;
                    let freq = suggestion_id.freq();

                    candidates.push(Candidate {
                        term: suggestion,
                        distance,
                        freq,
                    });
                    known_suggestions.insert(*suggestion_id);
                }
            }

            if length_diff < max_distance && candidate_length <= self.options.prefix_length {
                for i in 0..candidate.len() {
                    let mut delete = candidate.to_owned();
                    delete.remove(i);

                    let delete_hash = hash(&delete);

                    if !known_deletes.contains(&delete_hash) {
                        deletes.push(delete);
                        known_deletes.insert(delete_hash);
                    }
                }
            }
        }

        candidates.sort();
        Some(candidates)
    }
}
