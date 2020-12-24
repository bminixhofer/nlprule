use super::WordData;
use fst::{raw::Fst, Map};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::BufRead;

#[derive(Serialize, Deserialize)]
pub struct Tagger {
    tags: HashMap<u32, HashMap<u32, Vec<u16>>>,
    tag_store: Vec<u8>,
    word_store: Vec<u8>,
    groups: HashMap<u32, Vec<u32>>,
}

impl Tagger {
    fn get_lines<S1: AsRef<str>, S2: AsRef<str>>(
        paths: &[S1],
        remove_paths: &[S2],
    ) -> std::io::Result<Vec<(String, String, String)>> {
        let mut output = Vec::new();
        let mut disallowed: Vec<String> = Vec::new();

        for path in remove_paths {
            let file = File::open(path.as_ref())?;
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
            let file = File::open(path.as_ref())?;
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

                output.push((word, inflection, tag))
            }
        }

        Ok(output)
    }

    fn map_from_set(set: HashSet<&String>) -> Map<Vec<u8>> {
        let mut items: Vec<_> = set.into_iter().collect();
        items.sort_unstable();
        Map::from_iter(
            items
                .iter()
                .enumerate()
                .map(|(key, value)| (value, key as u64)),
        )
        .unwrap()
    }

    pub fn from_dumps<S1: AsRef<str>, S2: AsRef<str>>(
        paths: &[S1],
        remove_paths: &[S2],
    ) -> std::io::Result<Self> {
        let mut tags = HashMap::new();
        let mut groups = HashMap::new();

        let mut tag_store = HashSet::new();
        let mut word_store = HashSet::new();

        let lines = Tagger::get_lines(paths, remove_paths)?;

        for (word, inflection, tag) in lines.iter() {
            word_store.insert(word);
            word_store.insert(inflection);
            tag_store.insert(tag);
        }

        let word_store = Tagger::map_from_set(word_store);
        let tag_store = Tagger::map_from_set(tag_store);

        for (word, inflection, tag) in lines.iter() {
            let word = word.to_string();

            let word_id = word_store.get(word).unwrap();
            let inflection_id = word_store.get(inflection).unwrap();
            let tag_id = tag_store.get(tag).unwrap();

            let group = groups.entry(inflection_id as u32).or_insert_with(Vec::new);
            if !group.contains(&(word_id as u32)) {
                group.push(word_id as u32);
            }

            tags.entry(word_id as u32)
                .or_insert_with(HashMap::new)
                .entry(inflection_id as u32)
                .or_insert_with(Vec::new)
                .push(tag_id as u16);
        }

        Ok(Tagger {
            tags,
            groups,
            word_store: word_store.as_fst().as_bytes().to_vec(),
            tag_store: tag_store.as_fst().as_bytes().to_vec(),
        })
    }

    fn get_raw(&self, word: &str) -> Vec<WordData> {
        let tag_store = Fst::new(&self.tag_store).unwrap();
        let word_store = Fst::new(&self.word_store).unwrap();

        if let Some(map) = word_store
            .get(word)
            .and_then(|x| self.tags.get(&(x.value() as u32)))
        {
            let mut output = Vec::new();

            for (key, value) in map.iter() {
                for tag_id in value {
                    output.push(WordData::new(
                        String::from_utf8(word_store.get_key(*key as u64).unwrap()).unwrap(),
                        String::from_utf8(tag_store.get_key(*tag_id as u64).unwrap()).unwrap(),
                    ))
                }
            }

            output
        } else {
            Vec::new()
        }
    }

    fn get_strict_tags(
        &self,
        word: &str,
        add_lower: bool,
        add_lower_if_empty: bool,
    ) -> Vec<WordData> {
        let mut tags = self.get_raw(word);
        let lower = word.to_lowercase();

        if (add_lower || (add_lower_if_empty && tags.is_empty()))
            && (word != lower
                && (crate::utils::is_title_case(word) || crate::utils::is_uppercase(word)))
        {
            tags.extend(self.get_raw(&lower));
        }

        tags
    }

    pub fn get_tags(
        &self,
        word: &str,
        add_lower: bool,
        use_compound_split_heuristic: bool,
    ) -> Vec<WordData> {
        let mut tags = self.get_strict_tags(word, add_lower, true);

        // compound splitting heuristic, seems to work reasonably well
        if use_compound_split_heuristic && tags.is_empty() {
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

    pub fn get_group_members(&self, word: &str) -> Vec<String> {
        let word_store = Fst::new(&self.word_store).unwrap();

        word_store
            .get(word)
            .and_then(|x| self.groups.get(&(x.value() as u32)))
            .map(|vec| {
                vec.iter()
                    .map(|x| String::from_utf8(word_store.get_key(*x as u64).unwrap()).unwrap())
                    .collect()
            })
            .unwrap_or_else(Vec::new)
    }
}
