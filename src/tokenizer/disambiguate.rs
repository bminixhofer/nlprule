use crate::rule::DisambiguationRule;
use crate::structure::read_disambiguation_rules;
use crate::tokenizer::tag::Tagger;
use crate::tokenizer::IncompleteToken;
use log::warn;
use std::path::Path;
use std::{convert::TryFrom, error::Error};

pub struct Disambiguator {
    rules: Vec<DisambiguationRule>,
    tagger: Tagger,
}

pub struct DisambiguatorOptions {
    allow_errors: bool,
    ids: Vec<String>,
}

impl Default for DisambiguatorOptions {
    fn default() -> Self {
        DisambiguatorOptions {
            allow_errors: false,
            ids: Vec::new(),
        }
    }
}

impl Disambiguator {
    pub fn from_xml<P: AsRef<Path>>(
        path: P,
        tagger: Tagger,
        options: DisambiguatorOptions,
    ) -> Result<Self, Box<dyn Error>> {
        let rules = read_disambiguation_rules(path);
        let error = None;

        let rules: Vec<_> = rules
            .into_iter()
            .filter_map(|x| match x {
                Ok((rule_structure, id)) => match DisambiguationRule::try_from(rule_structure) {
                    Ok(mut rule) => {
                        if error.is_none() && (options.ids.is_empty() || options.ids.contains(&id))
                        {
                            rule.set_id(id);
                            Some(rule)
                        } else {
                            None
                        }
                    }
                    Err(x) => {
                        error = Some(format!("{}", x));
                        None
                    }
                },
                Err(x) => {
                    error = Some(format!("{}", x));
                    None
                }
            })
            .collect();

        if let Some(x) = error {
            if options.allow_errors {
                warn!("Error constructing Disambiguator: {}", x)
            } else {
                return Err("Error constructing Disambiguator: {}", x);
            }
        }

        Ok(Disambiguator { tagger, rules })
    }

    pub fn rules(&self) -> &Vec<DisambiguationRule> {
        &self.rules
    }

    pub fn apply_up_to_id(
        &self,
        mut tokens: Vec<IncompleteToken>,
        id: &str,
    ) -> Vec<IncompleteToken> {
        for rule in &self.rules {
            if rule.id == id {
                break;
            }

            tokens = rule.apply(tokens);
        }

        tokens
    }

    pub fn apply(&self, mut tokens: Vec<IncompleteToken>) -> Vec<IncompleteToken> {
        for rule in &self.rules {
            tokens = rule.apply(tokens);
        }

        tokens
    }
}
