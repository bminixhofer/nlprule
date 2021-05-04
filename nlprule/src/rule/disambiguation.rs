use std::ops::Range;

use crate::{properties::PropertyGuardMut, types::*};
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

    fn keep(&self, data: &mut Tags) {
        data.retain(|x| self.is_word_data_match(x))
    }

    fn remove(&self, data: &mut Tags) {
        data.retain(|x| !self.is_word_data_match(x))
    }

    pub fn and(filters: &[&Self], data: &Tags) -> bool {
        data.iter()
            .any(|x| filters.iter().all(|filter| filter.is_word_data_match(x)))
    }

    pub fn apply(filters: &[Vec<&Self>], data: &mut Tags) {
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
    pub fn apply<'t>(
        &'t self,
        groups: Vec<Vec<&mut Token<'t>>>,
        guard: PropertyGuardMut,
    ) -> Result<(), crate::properties::Error> {
        match self {
            Disambiguation::Remove(data_or_filters) => {
                for (group, data_or_filter) in groups.into_iter().zip(data_or_filters) {
                    for token in group.into_iter() {
                        match data_or_filter {
                            either::Left(data) => {
                                guard.tags_mut(token)?.retain(|x| {
                                    !(x.pos() == data.pos()
                                        && (data.lemma().as_str().is_empty()
                                            || x.lemma() == data.lemma()))
                                });
                            }
                            either::Right(filter) => {
                                filter.remove(guard.tags_mut(token)?);
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
                                    let last = {
                                        let tags = guard.tags(token)?;
                                        tags.iter()
                                            .next()
                                            .and_then(|x| {
                                                if *x.lemma() != WordId::empty() {
                                                    Some(x.lemma().clone())
                                                } else {
                                                    None
                                                }
                                            })
                                            .unwrap_or_else(|| tags.id().clone())
                                    };

                                    guard.tags_mut(token)?.retain(|x| x.pos() == limit.pos());

                                    if guard.tags(token)?.is_empty() {
                                        if *retain_last {
                                            guard
                                                .tags_mut(token)?
                                                .push(WordData::new(last, limit.pos().clone()));
                                        } else {
                                            let lemma = guard.tags(token)?.id().clone();

                                            guard
                                                .tags_mut(token)?
                                                .push(WordData::new(lemma, limit.pos().clone()));
                                        }
                                    }
                                }
                            }
                            either::Right(filter) => {
                                for token in group.into_iter() {
                                    filter.keep(guard.tags_mut(token)?);
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
                                guard.tags(token)?.id().clone()
                            } else {
                                data.lemma().clone()
                            },
                            data.pos().clone(),
                        );

                        let tags = guard.tags_mut(token)?;

                        tags.push(data);
                        tags.retain(|x| !x.pos().as_str().is_empty());
                    }
                }
            }
            Disambiguation::Replace(datas) => {
                for (group, data) in groups.into_iter().zip(datas) {
                    for token in group.into_iter() {
                        let data = WordData::new(
                            if data.lemma().as_str().is_empty() {
                                guard.tags(token)?.id().clone()
                            } else {
                                data.lemma().clone()
                            },
                            data.pos().clone(),
                        );

                        let tags = guard.tags_mut(token)?;

                        tags.clear();
                        tags.push(data);
                    }
                }
            }
            Disambiguation::Unify(filters, disambigs, mask) => {
                let filters: Vec<_> = filters.iter().multi_cartesian_product().collect();

                let mut filter_mask: Vec<_> = filters.iter().map(|_| true).collect();

                for (group, use_mask_val) in groups.iter().zip(mask) {
                    for token in group.iter() {
                        if *use_mask_val {
                            for (mask_val, filter) in filter_mask.iter_mut().zip(filters.iter()) {
                                *mask_val = *mask_val && PosFilter::and(filter, guard.tags(token)?);
                            }
                        }
                    }
                }

                if !filter_mask.iter().any(|x| *x) {
                    return Ok(());
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
                            let tags = guard.tags_mut(token)?;
                            let before = tags.clone();

                            PosFilter::apply(&to_apply, tags);

                            if let Some(disambig) = disambig {
                                disambig.keep(tags);
                            }

                            if tags.is_empty() {
                                *tags = before;
                            }
                        }
                    }
                }
            }
            Disambiguation::Nop => {}
        }

        Ok(())
    }
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct DisambiguationChange {
    pub text: String,
    pub char_span: Range<usize>,
    pub before: Tags<'static>,
    pub after: Tags<'static>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum DisambiguationExample {
    Unchanged(String),
    Changed(DisambiguationChange),
}
