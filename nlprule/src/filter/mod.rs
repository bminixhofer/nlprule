use crate::utils::regex::Regex;
use crate::{
    rule::{engine::composition::GraphId, MatchGraph},
    types::MatchSentence,
};
use enum_dispatch::enum_dispatch;
use serde::{Deserialize, Serialize};

#[enum_dispatch]
#[derive(Serialize, Deserialize)]
pub enum Filter {
    NoDisambiguationEnglishPartialPosTagFilter,
}

#[enum_dispatch(Filter)]
pub trait Filterable {
    fn keep(&self, sentence: &MatchSentence, graph: &MatchGraph) -> bool;
}

#[derive(Serialize, Deserialize)]
pub struct NoDisambiguationEnglishPartialPosTagFilter {
    pub(crate) id: GraphId,
    pub(crate) regexp: Regex,
    pub(crate) postag_regexp: Regex,
    #[allow(dead_code)]
    pub(crate) negate_postag: bool,
}

impl Filterable for NoDisambiguationEnglishPartialPosTagFilter {
    fn keep(&self, sentence: &MatchSentence, graph: &MatchGraph) -> bool {
        graph.by_id(self.id).tokens(sentence).all(|token| {
            if let Some(captures) = self.regexp.captures(&token.word.text.as_ref()) {
                let tags = sentence
                    .tagger()
                    .get_tags(&captures.get(1).unwrap().as_str());

                tags.iter()
                    .any(|x| self.postag_regexp.is_match(x.pos.as_ref()))
            } else {
                false
            }
        })
    }
}
