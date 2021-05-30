mod structure;

use fs_err::File;
use std::{io::BufReader, path::PathBuf};
use log::warn;

use crate::{
    compile::{BuildComponent, BuildInfo, Error},
    rule::id::Category,
};

use super::*;

/// Options for a disambiguator.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct DisambiguatorLangOptions {
    /// Whether to allow errors while constructing the tokenizer.
    pub allow_errors: bool,
    /// Disambiguation Rule selectors to use in this tokenizer.
    #[serde(default)]
    pub ids: Vec<Selector>,
    /// Disambiguation Rule selectors to ignore in this tokenizer.
    #[serde(default)]
    pub ignore_ids: Vec<Selector>,
}

#[derive(Deserialize)]
pub struct DisambiguatorPaths {
    disambiguator_xml: PathBuf,
    disambiguator_options: PathBuf,
}

impl BuildComponent for Disambiguator {
    type Paths = DisambiguatorPaths;

    fn build(paths: DisambiguatorPaths, build_info: Option<&mut BuildInfo>) -> Result<Self, Error> {
        let build_info = build_info.ok_or(Error::BuildInfoUnset)?;

        let options: DisambiguatorLangOptions =
            serde_json::from_reader(BufReader::new(File::open(&paths.disambiguator_options)?))?;
        let rules = structure::parse::read_disambiguation_rules(paths.disambiguator_xml);

        let mut error = None;

        let rules: Vec<_> = rules
            .into_iter()
            .filter_map(|x| match x {
                Ok((rule_structure, group, _)) => {
                    let id = Category::new("DISAMBIGUATION");

                    let id = if let Some(group) = &group {
                        id.join(group.id.as_str()).join(group.n)
                    } else {
                        id.join(
                            rule_structure
                                .id
                                .as_ref()
                                .expect("ID must be set if not in group."),
                        )
                        .join(0)
                    };

                    match DisambiguationRule::from_rule_structure(rule_structure, build_info) {
                        Ok(mut rule) => {
                            if error.is_none()
                                && (options.ids.is_empty()
                                    || options.ids.iter().any(|x| x.is_match(&id)))
                                && !options.ignore_ids.iter().any(|x| x.is_match(&id))
                            {
                                rule.id = id;

                                Some(rule)
                            } else {
                                None
                            }
                        }
                        Err(x) => {
                            if error.is_none() {
                                error = Some(format!("[Rule] {}", x));
                            }
                            None
                        }
                    }
                }
                Err(x) => {
                    if error.is_none() {
                        error = Some(format!("[Structure] {}", x));
                    }
                    None
                }
            })
            .collect();

        if let Some(x) = error {
            if options.allow_errors {
                warn!("Error constructing Disambiguator: {}", x)
            } else {
                return Err(Error::Unexpected(format!(
                    "Error constructing Disambiguator: {}",
                    x
                )));
            }
        }

        Ok(Disambiguator {
            rules,
            properties: Default::default(),
        })
    }
}

/// Language-dependent options for a rule set.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct RulesLangOptions {
    /// Whether to allow errors while constructing the rules.
    pub allow_errors: bool,
    /// Grammar Rule selectors to use in this set.
    #[serde(default)]
    pub ids: Vec<Selector>,
    /// Grammar Rule selectors to ignore in this set.
    #[serde(default)]
    pub ignore_ids: Vec<Selector>,
}

#[derive(Deserialize)]
pub struct RulesPaths {
    rules_xml: PathBuf,
    rules_options: PathBuf,
}

impl BuildComponent for Rules {
    type Paths = RulesPaths;

    fn build(paths: RulesPaths, build_info: Option<&mut BuildInfo>) -> Result<Self, Error> {
        let build_info = build_info.ok_or(Error::BuildInfoUnset)?;

        let options: RulesLangOptions =
            serde_json::from_reader(BufReader::new(File::open(&paths.rules_options)?))?;
        let rules = structure::parse::read_rules(paths.rules_xml);
        let mut errors: DefaultHashMap<String, usize> = DefaultHashMap::new();

        let rules: Vec<_> = rules
            .into_iter()
            .filter_map(|x| match x {
                Ok((rule_structure, group, category)) => {
                    let category = category.expect("grammar rules must have category");
                    let id = Category::new(category.id.as_str());

                    let id = if let Some(group) = &group {
                        id.join(group.id.as_str()).join(group.n)
                    } else {
                        id.join(
                            rule_structure
                                .id
                                .as_ref()
                                .expect("ID must be set if not in group."),
                        )
                        .join(0)
                    };

                    let rule_on = match rule_structure.default.as_deref() {
                        Some("off") | Some("temp_off") => false,
                        Some("on") | None => true,
                        Some(x) => panic!("unknown `default` value: {}", x),
                    };

                    let group_on = match group.as_ref().and_then(|x| x.default.as_deref()) {
                        Some("off") | Some("temp_off") => false,
                        Some("on") | None => true,
                        Some(x) => panic!("unknown `default` value: {}", x),
                    };

                    let category_on = match category.default.as_deref() {
                        Some("off") | Some("temp_off") => false,
                        Some("on") | None => true,
                        Some(x) => panic!("unknown `default` value: {}", x),
                    };

                    let name = rule_structure.name.as_ref().map_or_else(
                        || {
                            let group = group.as_ref().expect("must have group if name not set");
                            group.name.clone()
                        },
                        |x| x.clone(),
                    );

                    match Rule::from_rule_structure(rule_structure, build_info) {
                        Ok(mut rule) => {
                            if (options.ids.is_empty()
                                || options.ids.iter().any(|x| x.is_match(&id)))
                                && !options.ignore_ids.iter().any(|x| x.is_match(&id))
                            {
                                rule.id = id;
                                rule.name = name;
                                rule.category_name = category.name;
                                rule.category_type = category.kind;
                                rule.enabled = category_on && group_on && rule_on;
                                Some(rule)
                            } else {
                                None
                            }
                        }
                        Err(x) => {
                            *errors.entry(format!("[Rule] {}", x)).or_insert(0) += 1;
                            None
                        }
                    }
                }
                Err(x) => {
                    *errors.entry(format!("[Structure] {}", x)).or_insert(0) += 1;
                    None
                }
            })
            .collect();

        if !errors.is_empty() {
            let mut errors: Vec<(String, usize)> = errors.into_iter().collect();
            errors.sort_by_key(|x| -(x.1 as i32));

            warn!(
                "Errors constructing Rules: {:#?}",
                &errors
                    .iter()
                    .map(|(message, number)| format!("{} (n={})", message, number))
                    .collect::<Vec<_>>()
            );
        }

        Ok(Rules {
            rules,
            properties: Default::default(),
        })
    }
}
