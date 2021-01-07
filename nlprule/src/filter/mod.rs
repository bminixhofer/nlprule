use crate::tokenizer::Tokenizer;
use crate::utils::regex::SerializeRegex;
use crate::{rule::MatchGraph, Error};
use enum_dispatch::enum_dispatch;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[enum_dispatch]
#[derive(Serialize, Deserialize)]
pub enum Filter {
    NoDisambiguationEnglishPartialPosTagFilter,
}

#[enum_dispatch(Filter)]
pub trait Filterable {
    fn keep(&self, graph: &MatchGraph, tokenizer: &Tokenizer) -> bool;
}

trait FromArgs: Sized {
    fn from_args(args: HashMap<String, String>) -> Result<Self, Error>;
}

#[derive(Serialize, Deserialize)]
pub struct NoDisambiguationEnglishPartialPosTagFilter {
    index: usize,
    regexp: SerializeRegex,
    postag_regexp: SerializeRegex,
    #[allow(dead_code)]
    negate_postag: bool,
}

impl FromArgs for NoDisambiguationEnglishPartialPosTagFilter {
    fn from_args(args: HashMap<String, String>) -> Result<Self, Error> {
        if args.contains_key("negate_postag") {
            panic!("negate_postag not supported in NoDisambiguationEnglishPartialPosTagFilter");
        }

        Ok(NoDisambiguationEnglishPartialPosTagFilter {
            index: args.get("no").unwrap().parse::<usize>().unwrap(),
            regexp: SerializeRegex::new(&args.get("regexp").unwrap(), true, true)?,
            postag_regexp: SerializeRegex::new(&args.get("postag_regexp").unwrap(), true, true)?,
            negate_postag: args.get("negate_postag").map_or(false, |x| x == "yes"),
        })
    }
}

impl Filterable for NoDisambiguationEnglishPartialPosTagFilter {
    fn keep(&self, graph: &MatchGraph, tokenizer: &Tokenizer) -> bool {
        if let Some(group) = graph.by_id(self.index) {
            let tokens = &group.tokens(graph.tokens());

            tokens.iter().all(|x| {
                if let Some(captures) = self.regexp.captures(&x.word.text) {
                    // get group 2 because `must_fully_match` adds one group
                    let tags = tokenizer.tagger().get_tags(
                        &captures.at(2).unwrap(),
                        tokenizer.options().always_add_lower_tags,
                        tokenizer.options().use_compound_split_heuristic,
                    );

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

#[allow(dead_code)]
pub fn get_filter(name: &str, args: HashMap<String, String>) -> Result<Filter, Error> {
    match name {
        "NoDisambiguationEnglishPartialPosTagFilter" => {
            Ok(NoDisambiguationEnglishPartialPosTagFilter::from_args(args)?.into())
        }
        _ => Err(Error::Unexpected(format!("unsupported filter {}", name))),
    }
}
