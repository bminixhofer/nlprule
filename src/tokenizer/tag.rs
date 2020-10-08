use super::WordData;
use std::collections::HashMap;
use std::fs::File;
use std::io::BufRead;

pub struct Tagger {
    tags: HashMap<String, Vec<WordData>>,
}

impl Tagger {
    pub fn from_dumps(paths: &[&str], remove_paths: &[&str]) -> std::io::Result<Self> {
        let mut tags = HashMap::new();
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

                tags.entry(word)
                    .or_insert_with(Vec::new)
                    .push(WordData::new(inflection, tag));
            }
        }

        Ok(Tagger { tags })
    }

    pub fn get_tags(&self, word: &str) -> Vec<WordData> {
        let mut tags = self.tags.get(word).cloned().unwrap_or_else(Vec::new);
        let lower = word.to_lowercase();

        if word != lower && crate::utils::is_title_case(word) || crate::utils::is_uppercase(word) {
            tags.extend(self.tags.get(&lower).cloned().unwrap_or_else(Vec::new));
        }

        tags
    }
}
