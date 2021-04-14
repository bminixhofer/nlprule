use std::ops::Range;

use crate::types::*;
use itertools::Itertools;
use serde::{Deserialize, Serialize};

use super::engine::composition::PosMatcher;

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct PosFilter {
    pub matcher: PosMatcher,
}

impl PosFilter {
    fn is_word_data_match(&self, data: &WordData) -> bool {
        self.matcher.is_match(data.pos())
    }

    fn keep(&self, data: &mut Word) {
        data.retain(|x| self.is_word_data_match(x))
    }

    fn remove(&self, data: &mut Word) {
        data.retain(|x| !self.is_word_data_match(x))
    }

    pub fn and(filters: &[&Self], data: &Word) -> bool {
        data.tags()
            .iter()
            .any(|x| filters.iter().all(|filter| filter.is_word_data_match(x)))
    }

    pub fn apply(filters: &[Vec<&Self>], data: &mut Word) {
        data.retain(|x| {
            filters
                .iter()
                .any(|filter| filter.iter().all(|f| f.is_word_data_match(x)))
        })
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum Disambiguation {
    Remove(Vec<either::Either<WordData<'static>, PosFilter>>),
    Add(Vec<WordData<'static>>),
    Replace(Vec<WordData<'static>>),
    Filter(
        Vec<Option<either::Either<WordData<'static>, PosFilter>>>,
        bool,
    ),
    Unify(Vec<Vec<PosFilter>>, Vec<Option<PosFilter>>, Vec<bool>),
    Nop,
}

impl Disambiguation {
    pub fn apply<'t>(&'t self, groups: Vec<Vec<&mut IncompleteToken<'t>>>) {
        match self {
            Disambiguation::Remove(data_or_filters) => {
                for (group, data_or_filter) in groups.into_iter().zip(data_or_filters) {
                    for token in group.into_iter() {
                        match data_or_filter {
                            either::Left(data) => {
                                token.word_mut().retain(|x| {
                                    !(x.pos() == data.pos()
                                        && (data.lemma().as_str().is_empty()
                                            || x.lemma() == data.lemma()))
                                });
                            }
                            either::Right(filter) => {
                                filter.remove(token.word_mut());
                            }
                        }
                    }
                }
            }
            Disambiguation::Filter(filters, retain_last) => {
                for (group, maybe_filter) in groups.into_iter().zip(filters) {
                    if let Some(data_or_filter) = maybe_filter {
                        match data_or_filter {
                            either::Left(limit) => {
                                for token in group.into_iter() {
                                    let last = token.word().tags().get(0).map_or_else(
                                        || token.word().text().clone(),
                                        |x| x.lemma().clone(),
                                    );

                                    token.word_mut().retain(|x| x.pos() == limit.pos());

                                    if token.word().tags().is_empty() {
                                        if *retain_last {
                                            token
                                                .word_mut()
                                                .push(WordData::new(last, limit.pos().clone()));
                                        } else {
                                            let lemma = token.word().text().clone();

                                            token
                                                .word_mut()
                                                .push(WordData::new(lemma, limit.pos().clone()));
                                        }
                                    }
                                }
                            }
                            either::Right(filter) => {
                                for token in group.into_iter() {
                                    filter.keep(token.word_mut())
                                }
                            }
                        }
                    }
                }
            }
            Disambiguation::Add(datas) => {
                for (group, data) in groups.into_iter().zip(datas) {
                    for token in group.into_iter() {
                        let data = WordData::new(
                            if data.lemma().as_str().is_empty() {
                                token.word().text().clone()
                            } else {
                                data.lemma().clone()
                            },
                            data.pos().clone(),
                        );

                        token.word_mut().push(data);
                        token.word_mut().retain(|x| !x.pos().as_str().is_empty());
                    }
                }
            }
            Disambiguation::Replace(datas) => {
                for (group, data) in groups.into_iter().zip(datas) {
                    for token in group.into_iter() {
                        let data = WordData::new(
                            if data.lemma().as_str().is_empty() {
                                token.word().text().clone()
                            } else {
                                data.lemma().clone()
                            },
                            data.pos().clone(),
                        );

                        token.word_mut().clear();
                        token.word_mut().push(data);
                    }
                }
            }
            Disambiguation::Unify(filters, disambigs, mask) => {
                let filters: Vec<_> = filters.iter().multi_cartesian_product().collect();

                let mut filter_mask: Vec<_> = filters.iter().map(|_| true).collect();

                for (group, use_mask_val) in groups.iter().zip(mask) {
                    for token in group.iter() {
                        if *use_mask_val {
                            let finalized: Token = (*token).clone().into_token();

                            for (mask_val, filter) in filter_mask.iter_mut().zip(filters.iter()) {
                                *mask_val = *mask_val && PosFilter::and(filter, finalized.word());
                            }
                        }
                    }
                }

                if !filter_mask.iter().any(|x| *x) {
                    return;
                }

                let to_apply: Vec<_> = filter_mask
                    .iter()
                    .zip(filters)
                    .filter_map(
                        |(mask_val, filter)| {
                            if *mask_val {
                                Some(filter)
                            } else {
                                None
                            }
                        },
                    )
                    .collect();

                for ((group, disambig), use_mask_val) in groups.into_iter().zip(disambigs).zip(mask)
                {
                    if *use_mask_val {
                        for token in group.into_iter() {
                            let before = token.word().clone();

                            PosFilter::apply(&to_apply, token.word_mut());

                            if let Some(disambig) = disambig {
                                disambig.keep(token.word_mut());
                            }

                            if token.word().tags().is_empty() {
                                *token.word_mut() = before;
                            }
                        }
                    }
                }
            }
            Disambiguation::Nop => {}
        }
    }
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct DisambiguationChange {
    pub text: String,
    pub char_span: Range<usize>,
    pub before: Word<'static>,
    pub after: Word<'static>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum DisambiguationExample {
    Unchanged(String),
    Changed(DisambiguationChange),
}
