use crate::rule::MatchGraph;
use crate::tokenizer::Tokenizer;
use crate::utils::regex::SerializeRegex;
use enum_dispatch::enum_dispatch;
use serde::{Deserialize, Serialize};

#[enum_dispatch]
#[derive(Serialize, Deserialize)]
pub enum Filter {
    NoDisambiguationEnglishPartialPosTagFilter,
}

#[enum_dispatch(Filter)]
pub trait Filterable {
    fn keep(&self, graph: &MatchGraph, tokenizer: &Tokenizer) -> bool;
}

#[derive(Serialize, Deserialize)]
pub struct NoDisambiguationEnglishPartialPosTagFilter {
    pub(crate) index: usize,
    pub(crate) regexp: SerializeRegex,
    pub(crate) postag_regexp: SerializeRegex,
    #[allow(dead_code)]
    pub(crate) negate_postag: bool,
}

impl Filterable for NoDisambiguationEnglishPartialPosTagFilter {
    fn keep(&self, graph: &MatchGraph, tokenizer: &Tokenizer) -> bool {
        if let Some(group) = graph.by_id(self.index) {
            group.tokens(graph.tokens()).all(|token| {
                if let Some(captures) = self.regexp.captures(&token.word.text.as_ref()) {
                    // get group 2 because `must_fully_match` adds one group
                    let tags = tokenizer.tagger().get_tags(
                        &captures.at(2).unwrap(),
                        tokenizer.options().always_add_lower_tags,
                        tokenizer.options().use_compound_split_heuristic,
                    );

                    tags.iter()
                        .any(|x| self.postag_regexp.is_match(x.pos.as_ref()))
                } else {
                    false
                }
            })
        } else {
            true
        }
    }
}
