use super::WordData;
use std::collections::{HashMap, HashSet};
use std::fs::{read_dir, File};
use std::io::BufRead;
use std::path::Path;

pub struct Tagger {
    tags: HashMap<String, HashSet<WordData>>,
}

impl Tagger {
    pub fn from_dumps<P: AsRef<Path>>(path: P) -> std::io::Result<Self> {
        let mut tags = HashMap::new();

        for entry in read_dir(path)? {
            let entry = entry?;
            let file = File::open(entry.path())?;
            let reader = std::io::BufReader::new(file);

            for line in reader.lines() {
                let line = line?;
                if line.starts_with('#') {
                    continue;
                }

                let parts: Vec<_> = line.split('\t').collect();

                let word = parts[0].to_string();
                let inflection = parts[1].to_string();
                let tag = parts[2].to_string();

                tags.entry(word)
                    .or_insert_with(HashSet::new)
                    .insert(WordData::new(inflection, tag));
            }
        }

        Ok(Tagger { tags })
    }

    pub fn get_tags(&self, word: &str) -> HashSet<WordData> {
        let mut tags = self.tags.get(word).cloned().unwrap_or_else(HashSet::new);
        tags.extend(
            self.tags
                .get(&word.to_lowercase())
                .cloned()
                .unwrap_or_else(HashSet::new),
        );

        if tags.is_empty() {
            tags.insert(WordData::new(word.to_string(), String::new()));
        }

        tags
    }
}
