use crate::rule::{engine::composition::GraphId, MatchGraph, MatchSentence};
use crate::utils::regex::Regex;
use enum_dispatch::enum_dispatch;
use serde::{Deserialize, Serialize};

#[enum_dispatch]
#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum Filter {
    NoDisambiguationEnglishPartialPosTagFilter,
    /// `IsEnglishWordFilter`: keep the match if all referenced forms are
    /// (approximated) English words.
    IsEnglishWordFilter(IsEnglishWordFilter),
}

#[enum_dispatch(Filter)]
pub trait Filterable {
    fn keep(&self, sentence: &MatchSentence, graph: &MatchGraph) -> bool;
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct NoDisambiguationEnglishPartialPosTagFilter {
    pub(crate) id: GraphId,
    pub(crate) regexp: Regex,
    pub(crate) postag_regexp: Regex,
    #[allow(dead_code)]
    pub(crate) negate_postag: bool,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct IsEnglishWordFilter {
    /// Graph ids of the pattern positions given in `formPositions`.
    pub(crate) ids: Vec<GraphId>,
}

/// LT tags the forms with the *English* tagger and keeps the match if all are
/// known English words. We approximate "known to the English tagger" with a
/// frequency list of the ~20k most common English words.
impl Filterable for IsEnglishWordFilter {
    fn keep(&self, sentence: &MatchSentence, graph: &MatchGraph) -> bool {
        use once_cell::sync::Lazy;
        static ENGLISH_WORDS: Lazy<std::collections::HashSet<&'static str>> = Lazy::new(|| {
            include_str!("../../configs/en_common_words.txt")
                .lines()
                .collect()
        });
        self.ids.iter().all(|id| {
            graph
                .by_id(*id)
                .tokens(sentence)
                .all(|token| ENGLISH_WORDS.contains(token.word().as_str()))
        })
    }
}

impl Filterable for NoDisambiguationEnglishPartialPosTagFilter {
    fn keep(&self, sentence: &MatchSentence, graph: &MatchGraph) -> bool {
        graph.by_id(self.id).tokens(sentence).all(|token| {
            if let Some(captures) = self.regexp.captures(&token.word().as_str()) {
                let mut tags = sentence
                    .tagger()
                    .get_tags(&captures.get(1).unwrap().as_str());

                tags.any(|x| self.postag_regexp.is_match(x.pos().as_str()))
            } else {
                false
            }
        })
    }
}
