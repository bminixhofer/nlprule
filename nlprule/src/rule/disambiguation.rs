use crate::types::*;
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use std::borrow::Cow;

use super::engine::composition::PosMatcher;

#[derive(Serialize, Deserialize)]
pub struct POSFilter {
    matcher: PosMatcher,
}

impl POSFilter {
    pub fn new(matcher: PosMatcher) -> Self {
        POSFilter { matcher }
    }

    fn is_word_data_match(&self, data: &WordData) -> bool {
        self.matcher.is_match(data.pos_id)
    }

    fn keep(&self, data: &mut Word) {
        data.tags.retain(|x| self.is_word_data_match(x))
    }

    fn remove(&self, data: &mut Word) {
        data.tags.retain(|x| !self.is_word_data_match(x))
    }

    fn and(filters: &[&Self], data: &Word) -> bool {
        data.tags
            .iter()
            .any(|x| filters.iter().all(|filter| filter.is_word_data_match(x)))
    }

    fn apply(filters: &[Vec<&Self>], data: &mut Word) {
        data.tags.retain(|x| {
            filters
                .iter()
                .any(|filter| filter.iter().all(|f| f.is_word_data_match(x)))
        })
    }
}

#[derive(Serialize, Deserialize)]
pub enum Disambiguation {
    Remove(Vec<either::Either<OwnedWordData, POSFilter>>),
    Add(Vec<OwnedWordData>),
    Replace(Vec<OwnedWordData>),
    Filter(Vec<Option<either::Either<OwnedWordData, POSFilter>>>),
    Unify(Vec<Vec<POSFilter>>, Vec<Option<POSFilter>>, Vec<bool>),
    Nop,
}

impl Disambiguation {
    pub fn apply<'t>(&'t self, groups: Vec<Vec<&mut IncompleteToken<'t>>>, retain_last: bool) {
        match self {
            Disambiguation::Remove(data_or_filters) => {
                for (group, data_or_filter) in groups.into_iter().zip(data_or_filters) {
                    for token in group.into_iter() {
                        match data_or_filter {
                            either::Left(data) => {
                                token.word.tags.retain(|x| {
                                    !(x.pos_id == data.pos_id
                                        && (data.lemma.is_empty() || x.lemma == data.lemma))
                                });
                            }
                            either::Right(filter) => {
                                filter.remove(&mut token.word);
                            }
                        }
                    }
                }
            }
            Disambiguation::Filter(filters) => {
                for (group, maybe_filter) in groups.into_iter().zip(filters) {
                    if let Some(data_or_filter) = maybe_filter {
                        match data_or_filter {
                            either::Left(limit) => {
                                for token in group.into_iter() {
                                    let last = token
                                        .word
                                        .tags
                                        .get(0)
                                        .map_or(token.word.text, |x| x.lemma.as_ref())
                                        .to_string();

                                    token.word.tags.retain(|x| x.pos_id == limit.pos_id);

                                    if token.word.tags.is_empty() {
                                        token.word.tags.push(WordData::new(
                                            if retain_last {
                                                Cow::Owned(last)
                                            } else {
                                                token.word.text.into()
                                            },
                                            limit.pos_id,
                                        ));
                                    }
                                }
                            }
                            either::Right(filter) => {
                                for token in group.into_iter() {
                                    filter.keep(&mut token.word)
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
                            if data.lemma.is_empty() {
                                token.word.text
                            } else {
                                data.lemma.as_str()
                            },
                            data.pos_id,
                        );

                        token.word.tags.push(data);
                        let tagger = token.tagger;

                        token
                            .word
                            .tags
                            .retain(|x| !tagger.id_to_tag(x.pos_id).is_empty());
                    }
                }
            }
            Disambiguation::Replace(datas) => {
                for (group, data) in groups.into_iter().zip(datas) {
                    for token in group.into_iter() {
                        let data = WordData::new(
                            if data.lemma.is_empty() {
                                token.word.text
                            } else {
                                data.lemma.as_str()
                            },
                            data.pos_id,
                        );

                        token.word.tags.clear();
                        token.word.tags.push(data);
                    }
                }
            }
            Disambiguation::Unify(filters, disambigs, mask) => {
                let filters: Vec<_> = filters.iter().multi_cartesian_product().collect();

                let mut filter_mask: Vec<_> = filters.iter().map(|_| true).collect();

                for (group, use_mask_val) in groups.iter().zip(mask) {
                    for token in group.iter() {
                        if *use_mask_val {
                            let finalized: Token = (*token).clone().into();

                            for (mask_val, filter) in filter_mask.iter_mut().zip(filters.iter()) {
                                *mask_val = *mask_val && POSFilter::and(filter, &finalized.word);
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
                            let before = token.word.clone();

                            POSFilter::apply(&to_apply, &mut token.word);

                            if let Some(disambig) = disambig {
                                disambig.keep(&mut token.word);
                            }

                            if token.word.tags.is_empty() {
                                token.word = before;
                            }
                        }
                    }
                }
            }
            Disambiguation::Nop => {}
        }
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct DisambiguationChange {
    pub text: String,
    pub char_span: (usize, usize),
    pub before: OwnedWord,
    pub after: OwnedWord,
}

#[derive(Debug, Serialize, Deserialize)]
pub enum DisambiguationTest {
    Unchanged(String),
    Changed(DisambiguationChange),
}
