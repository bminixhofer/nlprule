use crate::rule::DisambiguationRule;
use crate::structure::read_disambiguation_rules;
use crate::tokenizer::Token;
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

    pub fn apply<'a>(&self, mut tokens: Vec<Token<'a>>) -> Vec<Token<'a>> {
        for rule in &self.rules {
            tokens = rule.apply(tokens);
        }

        tokens
    }
}
