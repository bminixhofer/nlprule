use crate::rule::DisambiguationRule;
use crate::structure::read_disambiguation_rules;
use crate::tokenizer::IncompleteToken;
use std::convert::TryFrom;
use std::path::Path;

pub struct Disambiguator {
    rules: Vec<DisambiguationRule>,
}

impl Disambiguator {
    pub fn from_xml<P: AsRef<Path>>(path: P) -> Self {
        let rules = read_disambiguation_rules(path);

        let rules: Vec<_> = rules
            .into_iter()
            .filter_map(|x| match x {
                Ok((rule_structure, id)) => match DisambiguationRule::try_from(rule_structure) {
                    Ok(mut rule) => {
                        rule.set_id(id);

                        Some(rule)
                    }
                    Err(_) => None,
                },
                Err(_) => None,
            })
            .collect();

        Disambiguator { rules }
    }

    pub fn apply_up_to_id(&self, tokens: &mut Vec<IncompleteToken>, id: &str) {
        for rule in &self.rules {
            if rule.id == id {
                break;
            }

            rule.apply(tokens);
        }
    }

    pub fn apply(&self, tokens: &mut Vec<IncompleteToken>) {
        for rule in &self.rules {
            rule.apply(tokens);
        }
    }
}
