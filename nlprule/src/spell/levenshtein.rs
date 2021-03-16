use fnv::FnvHasher;
use fst::Automaton;
use std::{
    cmp::{self, min},
    hash::{Hash, Hasher},
};

#[derive(Clone, Debug)]
pub struct LevenshteinState {
    dist: usize,
    n: usize,
    // to compute the next row of the matrix, we also need the row two rows up for transposes
    prev_row: Option<Vec<usize>>,
    prev_byte: u8,
    row: Vec<usize>,
    hash: u64,
}

impl LevenshteinState {
    pub fn dist(&self) -> usize {
        self.dist
    }
}

#[derive(Debug, Clone)]
pub struct Levenshtein<'a> {
    query: &'a [u8],
    distance: usize,
    prefix: usize,
}

impl<'a> Levenshtein<'a> {
    pub fn new(query: &'a str, distance: usize, prefix: usize) -> Self {
        Levenshtein {
            query: query.as_bytes(),
            distance,
            prefix,
        }
    }
}

impl<'a> Automaton for Levenshtein<'a> {
    type State = Option<LevenshteinState>;

    fn start(&self) -> Self::State {
        Some(LevenshteinState {
            dist: self.query.len(),
            n: 0,
            prev_row: None,
            prev_byte: 0,
            row: (0..=self.query.len()).collect(),
            hash: FnvHasher::default().finish(),
        })
    }

    fn is_match(&self, state: &Self::State) -> bool {
        state
            .as_ref()
            .map_or(false, |state| state.dist <= self.distance)
    }

    fn can_match(&self, state: &Self::State) -> bool {
        state.is_some()
    }

    fn accept(&self, state: &Self::State, byte: u8) -> Self::State {
        state.as_ref().and_then(|state| {
            let mut next_hasher = FnvHasher::with_key(state.hash);
            byte.hash(&mut next_hasher);
            let next_hash = next_hasher.finish();

            let row = &state.row;
            let mut next_row = state.row.to_vec();

            next_row[0] = state.n + 1;

            for i in 1..next_row.len() {
                let mut cost = if byte == self.query[i - 1] {
                    row[i - 1]
                } else {
                    min(
                        next_row[i - 1] + 1, // deletes
                        min(
                            row[i - 1] + 1, // inserts
                            row[i] + 1,     // substitutes
                        ),
                    )
                };

                if i > 1 {
                    // transposes
                    if let Some(prev_row) = state.prev_row.as_ref() {
                        if byte == self.query[i - 2] && state.prev_byte == self.query[i - 1] {
                            cost = min(cost, prev_row[i - 2] + 1);
                        }
                    }
                }

                next_row[i] = cost;
            }

            let distance = if state.n >= self.prefix {
                self.distance
            } else {
                1
            };

            let lower_bound = state.n.saturating_sub(distance);
            let upper_bound = cmp::min(state.n + distance, self.query.len());

            let cutoff = if lower_bound > upper_bound {
                0
            } else {
                *next_row[lower_bound..=upper_bound]
                    .iter()
                    .min()
                    .unwrap_or(&0)
            };

            if cutoff > distance {
                return None;
            }

            Some(LevenshteinState {
                dist: next_row[self.query.len()],
                n: state.n + 1,
                prev_row: Some(row.clone()),
                prev_byte: byte,
                row: next_row,
                hash: next_hash,
            })
        })
    }
}
