use crate::composition::MatchGraph;
use crate::tokenizer::TAGGER;
use crate::utils;
use onig::Regex;
use std::collections::HashMap;

pub trait Filter: Send + Sync {
    fn keep(&self, graph: &MatchGraph) -> bool;
}

trait FromArgs {
    fn from_args(args: HashMap<String, String>) -> Self;
}

struct NoDisambiguationEnglishPartialPosTagFilter {
    index: usize,
    regexp: Regex,
    postag_regexp: Regex,
    #[allow(dead_code)]
    negate_postag: bool,
}

impl FromArgs for NoDisambiguationEnglishPartialPosTagFilter {
    fn from_args(args: HashMap<String, String>) -> Self {
        if args.contains_key("negate_postag") {
            panic!("negate_postag not supported in NoDisambiguationEnglishPartialPosTagFilter");
        }

        NoDisambiguationEnglishPartialPosTagFilter {
            index: args.get("no").unwrap().parse::<usize>().unwrap() - 1,
            regexp: utils::new_regex(&args.get("regexp").unwrap(), true, true),
            postag_regexp: utils::new_regex(&args.get("postag_regexp").unwrap(), true, true),
            negate_postag: args.get("negate_postag").map_or(false, |x| x == "yes"),
        }
    }
}

impl Filter for NoDisambiguationEnglishPartialPosTagFilter {
    fn keep(&self, graph: &MatchGraph) -> bool {
        if let Some(group) = graph.by_id(self.index) {
            let tokens = &group.tokens;

            tokens.iter().all(|x| {
                if let Some(captures) = self.regexp.captures(&x.text) {
                    // get group 2 because `must_fully_match` adds one group
                    let tags = TAGGER.get_tags(&captures.at(2).unwrap());

                    tags.iter().any(|x| self.postag_regexp.is_match(&x.pos))
                } else {
                    false
                }
            })
        } else {
            true
        }
    }
}

pub fn get_filter(name: &str, args: HashMap<String, String>) -> Box<dyn Filter> {
    match name {
        "NoDisambiguationEnglishPartialPosTagFilter" => {
            Box::new(NoDisambiguationEnglishPartialPosTagFilter::from_args(args))
        }
        _ => panic!("unsupported filter {}", name),
    }
}
