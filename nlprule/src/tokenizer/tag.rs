use super::WordData;
use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs::File;
use std::io::BufRead;

#[derive(Serialize, Deserialize)]
pub struct Tagger {
    tags: HashMap<String, Vec<WordData>>,
    groups: HashMap<String, Vec<String>>,
}

impl Tagger {
    pub fn from_dumps(paths: &[&str], remove_paths: &[&str]) -> std::io::Result<Self> {
        let mut tags = HashMap::new();
        let mut groups = HashMap::new();

        let mut disallowed: Vec<String> = Vec::new();

        for path in remove_paths {
            let file = File::open(path)?;
            let reader = std::io::BufReader::new(file);

            for line in reader.lines() {
                let line = line?;
                if line.starts_with('#') {
                    continue;
                }

                disallowed.push(line.to_string());
            }
        }

        for path in paths {
            let file = File::open(path)?;
            let reader = std::io::BufReader::new(file);

            for line in reader.lines() {
                let line = line?;
                if line.starts_with('#') {
                    continue;
                }

                let parts: Vec<_> = line.split('\t').collect();

                if disallowed.contains(&line) {
                    continue;
                }

                let word = parts[0].to_string();
                let inflection = parts[1].to_string();
                let tag = parts[2].to_string();

                let group = groups.entry(inflection.clone()).or_insert_with(Vec::new);
                if !group.contains(&word) {
                    group.push(word.clone());
                }

                tags.entry(word)
                    .or_insert_with(Vec::new)
                    .push(WordData::new(inflection, tag));
            }
        }

        Ok(Tagger { tags, groups })
    }

    fn get_strict_tags(
        &self,
        word: &str,
        add_lower: bool,
        add_lower_if_empty: bool,
    ) -> Vec<WordData> {
        let mut tags = self.tags.get(word).cloned().unwrap_or_else(Vec::new);
        let lower = word.to_lowercase();

        lazy_static! {
            static ref IS_ENGLISH: bool = std::env::var("RULE_LANG").unwrap() == "en";
        }

        if (add_lower || (add_lower_if_empty && tags.is_empty()) || *IS_ENGLISH)
            && (word != lower
                && (crate::utils::is_title_case(word) || crate::utils::is_uppercase(word)))
        {
            tags.extend(self.tags.get(&lower).cloned().unwrap_or_else(Vec::new));
        }

        tags
    }

    pub fn get_tags(&self, word: &str, add_lower: bool) -> Vec<WordData> {
        let mut tags = self.get_strict_tags(word, add_lower, true);

        lazy_static! {
            static ref IS_GERMAN: bool = std::env::var("RULE_LANG").unwrap() == "de";
        }

        // compound splitting heuristic, seems to work reasonably well
        if *IS_GERMAN && tags.is_empty() {
            let n_chars = word.chars().count() as isize;

            if n_chars >= 7 {
                let indices = word
                    .char_indices()
                    .take(std::cmp::max(n_chars - 4, 0) as usize)
                    .skip(1)
                    .map(|x| x.0);

                for i in indices {
                    let next = if word.chars().next().unwrap().is_uppercase() {
                        crate::utils::apply_to_first(&word[i..], |c| c.to_uppercase().collect())
                    } else {
                        word[i..].to_string()
                    };

                    let next_tags = self.get_strict_tags(&next, add_lower, false);

                    if !next_tags.is_empty() {
                        tags = next_tags
                            .into_iter()
                            .map(|mut x| {
                                x.lemma = format!("{}{}", &word[..i], x.lemma.to_lowercase());
                                x
                            })
                            .collect();
                        break;
                    }
                }
            }
        }

        tags
    }

    pub fn get_group_members(&self, word: &str) -> Vec<&str> {
        self.groups
            .get(word)
            .map(|vec| vec.iter().map(|x| x.as_str()).collect())
            .unwrap_or_else(Vec::new)
    }
}
