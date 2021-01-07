use std::collections::HashMap;

use crate::{types::*, utils::regex::SerializeRegex};
use serde::{Deserialize, Serialize};
#[allow(dead_code)]
pub mod composition;

use composition::{Composition, Group, MatchGraph};

#[derive(Serialize, Deserialize)]
pub struct TokenEngine {
    pub(crate) composition: Composition,
    pub(crate) antipatterns: Vec<Composition>,
}

impl TokenEngine {
    fn get_match<'t>(&'t self, tokens: &'t [&'t Token], i: usize) -> Option<MatchGraph<'t>> {
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

#[derive(Serialize, Deserialize)]
pub enum Engine {
    Token(TokenEngine),
    Text(SerializeRegex, HashMap<usize, usize>),
}

impl Engine {
    pub fn get_matches<'t>(
        &'t self,
        tokens: &'t [&'t Token],
        skip_mask: Option<&[bool]>,
        start: usize,
        end: usize,
    ) -> Vec<MatchGraph<'t>> {
        let mut graphs = Vec::new();

        match &self {
            Engine::Token(engine) => {
                let mut graph_info: Vec<_> = (0..tokens.len())
                    .into_iter()
                    .filter(|i| skip_mask.map_or(true, |x| !x[*i]))
                    .filter_map(|i| {
                        if let Some(graph) = engine.get_match(&tokens, i) {
                            let start_group = graph
                                .by_id(start)
                                .unwrap_or_else(|| panic!("group must exist in graph: {}", start));
                            let end_group = graph.by_id(end - 1).unwrap_or_else(|| {
                                panic!("group must exist in graph: {}", end - 1)
                            });

                            let start = start_group.char_span.0;
                            let end = end_group.char_span.1;
                            Some((graph, start, end))
                        } else {
                            None
                        }
                    })
                    .collect();

                graph_info.sort_by(|(_, start, _), (_, end, _)| start.cmp(end));
                let mut mask = vec![false; tokens[0].text.chars().count()];

                for (graph, start, end) in graph_info {
                    if mask[start..end].iter().all(|x| !x) {
                        graphs.push(graph);
                        mask[start..end].iter_mut().for_each(|x| *x = true);
                    }
                }
            }
            Engine::Text(regex, id_to_idx) => {
                // this is the entire text, NOT the text of one token
                let text = tokens[0].text;

                let mut byte_to_char_idx: HashMap<usize, usize> = text
                    .char_indices()
                    .enumerate()
                    .map(|(ci, (bi, _))| (bi, ci))
                    .collect();
                byte_to_char_idx.insert(text.len(), byte_to_char_idx.len());

                graphs.extend(regex.captures_iter(text).map(|captures| {
                    let mut groups = Vec::new();
                    for group in captures.iter_pos() {
                        if let Some(group) = group {
                            let start = *byte_to_char_idx.get(&group.0).unwrap();
                            let end = *byte_to_char_idx.get(&group.1).unwrap();

                            groups.push(Group::new((start, end)));
                        } else {
                            groups.push(Group::new((0, 0)));
                        }
                    }

                    MatchGraph::new(groups, id_to_idx, tokens.to_vec())
                }));
            }
        }

        graphs
    }
}
