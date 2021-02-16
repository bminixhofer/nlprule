use crate::{types::*, utils::regex::SerializeRegex};
use onig::FindCaptures;
use serde::{Deserialize, Serialize};
pub mod composition;

use composition::{Composition, Group, MatchGraph};

use self::composition::GraphId;

#[derive(Serialize, Deserialize, Debug)]
pub struct TokenEngine {
    pub(crate) composition: Composition,
    pub(crate) antipatterns: Vec<Composition>,
}

impl TokenEngine {
    fn get_match<'t>(&'t self, tokens: &'t [Token], i: usize) -> Option<MatchGraph<'t>> {
        if let Some(graph) = self.composition.apply(tokens, i) {
            let mut blocked = false;

            // TODO: cache / move to outer loop
            for i in 0..tokens.len() {
                for antipattern in &self.antipatterns {
                    if let Some(anti_graph) = antipattern.apply(tokens, i) {
                        let anti_start = anti_graph.by_index(0).char_span.0;
                        let anti_end = anti_graph
                            .by_index(anti_graph.groups().len() - 1)
                            .char_span
                            .1;

                        let rule_start = graph.by_index(0).char_span.0;
                        let rule_end = graph.by_index(graph.groups().len() - 1).char_span.1;

                        if anti_start <= rule_end && rule_start <= anti_end {
                            blocked = true;
                            break;
                        }
                    }
                }
                if blocked {
                    break;
                }
            }

            if !blocked {
                return Some(graph);
            }
        }

        None
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub enum Engine {
    Token(TokenEngine),
    Text(SerializeRegex, DefaultHashMap<GraphId, usize>),
}

struct TokenMatches<'a> {
    engine: &'a TokenEngine,
    index: usize,
    mask: Vec<bool>,
}

struct TextMatches<'a, 't> {
    byte_idx_to_char_idx: DefaultHashMap<usize, usize>,
    id_to_idx: &'a DefaultHashMap<GraphId, usize>,
    captures: FindCaptures<'a, 't>,
}

enum InnerMatches<'a: 't, 't> {
    Token(TokenMatches<'a>),
    Text(TextMatches<'a, 't>),
}

pub struct EngineMatches<'a, 't> {
    tokens: &'t [Token<'t>],
    start: GraphId,
    end: GraphId,
    inner: InnerMatches<'a, 't>,
}

impl<'a, 't> Iterator for EngineMatches<'a, 't> {
    type Item = MatchGraph<'t>;

    fn next(&mut self) -> Option<Self::Item> {
        let tokens = self.tokens;
        let start_id = self.start;
        let end_id = self.end;

        match &mut self.inner {
            InnerMatches::Token(inner) => (inner.index..tokens.len()).find_map(|i| {
                inner.engine.get_match(tokens, i).and_then(|graph| {
                    let start_group = graph.by_id(start_id);
                    let end_group = graph.by_id(end_id);

                    let start = start_group.char_span.0;
                    let end = end_group.char_span.1;

                    if inner.mask[start..end].iter().all(|x| !x) {
                        inner.mask[start..end].iter_mut().for_each(|x| *x = true);

                        inner.index += 1;
                        Some(graph)
                    } else {
                        None
                    }
                })
            }),
            InnerMatches::Text(inner) => inner.captures.next().map(|captures| {
                let bi_to_ci = &inner.byte_idx_to_char_idx;
                let mut groups = Vec::new();

                for group in captures.iter_pos() {
                    if let Some(group) = group {
                        let start = *bi_to_ci
                            .get(&group.0)
                            .expect("byte index is at char boundary");
                        let end = *bi_to_ci
                            .get(&group.1)
                            .expect("byte index is at char boundary");

                        groups.push(Group::new((start, end)));
                    } else {
                        groups.push(Group::new((0, 0)));
                    }
                }

                MatchGraph::new(groups, inner.id_to_idx, tokens)
            }),
        }
    }
}

impl Engine {
    pub fn get_matches<'a, 't>(
        &'a self,
        tokens: &'t [Token],
        start: GraphId,
        end: GraphId,
    ) -> EngineMatches<'a, 't> {
        EngineMatches {
            tokens,
            start,
            end,
            inner: match &self {
                Engine::Token(engine) => InnerMatches::Token(TokenMatches {
                    engine,
                    index: 0,
                    mask: vec![false; tokens[0].sentence.chars().count()],
                }),
                Engine::Text(regex, id_to_idx) => {
                    let sentence = tokens[0].sentence;

                    let mut bi_to_ci: DefaultHashMap<usize, usize> = sentence
                        .char_indices()
                        .enumerate()
                        .map(|(ci, (bi, _))| (bi, ci))
                        .collect();
                    bi_to_ci.insert(sentence.len(), bi_to_ci.len());

                    InnerMatches::Text(TextMatches {
                        byte_idx_to_char_idx: bi_to_ci,
                        id_to_idx,
                        captures: regex.captures_iter(sentence),
                    })
                }
            },
        }
    }
}
