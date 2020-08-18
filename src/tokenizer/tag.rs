use std::collections::HashMap;
use std::fs::{read_dir, File};
use std::io::BufRead;
use std::path::Path;

pub struct Tagger {
    tags: HashMap<String, Vec<String>>,
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
                let tag = parts[2].to_string();

                tags.entry(word).or_insert_with(Vec::new).push(tag);
            }
        }

        Ok(Tagger { tags })
    }

    pub fn get_tags(&self, word: &str) -> Vec<&str> {
        if let Some(tags) = self.tags.get(word) {
            tags.iter().map(|x| x.as_str()).collect()
        } else {
            vec!["UNKNOWN"]
        }
    }
}
