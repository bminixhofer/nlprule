use std::collections::{HashMap, HashSet};
use std::fs::{read_dir, File};
use std::io::BufRead;
use std::path::Path;

pub struct Inflecter {
    inflections: HashMap<String, (Vec<String>, Vec<String>)>,
}

impl Inflecter {
    pub fn from_dumps<P: AsRef<Path>>(path: P) -> std::io::Result<Self> {
        let mut inflect_groups = HashMap::new();
        let mut inflections = HashMap::new();

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
                let inflection = parts[1];

                inflect_groups
                    .entry(inflection.to_string())
                    .or_insert_with(|| {
                        vec![inflection.to_string()]
                            .into_iter()
                            .collect::<HashSet<_>>()
                    })
                    .insert(word);
            }
        }

        for group in inflect_groups.values() {
            for entry in group {
                let existing = inflections
                    .entry(entry.to_lowercase())
                    .or_insert_with(|| (Vec::new(), Vec::new()));
                existing.0.extend(group.iter().cloned());
                existing.1.extend(group.iter().map(|x| x.to_lowercase()));
            }
        }

        Ok(Inflecter { inflections })
    }

    pub fn get_inflections(&self, word: &str) -> (Vec<String>, Vec<String>) {
        let lower = word.to_lowercase();

        if let Some(inflections) = self.inflections.get(&lower) {
            let mut group = inflections.0.clone();
            let mut lower_group = inflections.1.clone();

            group.push(word.to_string());
            lower_group.push(lower);

            (group, lower_group)
        } else {
            (vec![word.to_string()], vec![lower])
        }
    }
}
