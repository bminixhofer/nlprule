//! A morphological synthesizer: looks up inflected forms of a lemma for a
//! part-of-speech tag. Mirrors the behavior of LanguageTool's
//! `BaseSynthesizer` / `ManualSynthesizer` (Morfologik-based lookup with
//! manual additions and removals).
//!
//! Storage is compact: a single FST maps `lemma|POS` keys to rows of interned
//! form strings, so multi-million-entry dictionaries (Portuguese has ~17M)
//! stay at a size comparable to the original Morfologik FST.

use crate::utils::regex::Regex;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Language-specific behavior of the synthesizer,
/// mirroring overrides of `BaseSynthesizer` subclasses in LanguageTool.
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum SynthesizerKind {
    Default,
    /// GermanSynthesizer: POS tag correction appends case information.
    German,
    /// CatalanSynthesizer: verb synthesis over a valencia-variant dictionary,
    /// falling back to central-variant verb tags when the base lookup is empty.
    Catalan,
}

impl Default for SynthesizerKind {
    fn default() -> Self {
        SynthesizerKind::Default
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Synthesizer {
    /// Serialized `fst::Map` mapping `lemma|POS` (sorted) to a value:
    /// * even values (>= 2) encode a single form: `(value - 2) / 2` into `strings`
    /// * odd values encode an extended row: `(value - 1) / 2` into `rows`
    map_bytes: Vec<u8>,
    /// Extended rows (keys with more than one form): row index -> form string indices.
    rows: Vec<Vec<u32>>,
    /// Interned form strings.
    strings: Vec<String>,
    /// (lemma, pos tag) -> manually added forms (`added.txt`)
    manual: HashMap<(String, String), Vec<String>>,
    /// (lemma, pos tag) -> forms to remove (`removed.txt`, `do-not-synthesize.txt`)
    removed: HashMap<(String, String), Vec<String>>,
    kind: SynthesizerKind,
}

impl Synthesizer {
    pub(crate) fn new(kind: SynthesizerKind) -> Self {
        Synthesizer {
            map_bytes: Vec::new(),
            rows: Vec::new(),
            strings: Vec::new(),
            manual: HashMap::new(),
            removed: HashMap::new(),
            kind,
        }
    }

    pub(crate) fn kind(&self) -> SynthesizerKind {
        self.kind
    }

    pub(crate) fn insert_manual(&mut self, lemma: String, pos: String, form: String) {
        self.manual.entry((lemma, pos)).or_default().push(form);
    }

    pub(crate) fn insert_removed(&mut self, lemma: String, pos: String, form: String) {
        self.removed.entry((lemma, pos)).or_default().push(form);
    }

    fn map(&self) -> fst::Map<&[u8]> {
        fst::Map::new(&self.map_bytes[..]).expect("synthesizer FST must be valid")
    }

    fn value_forms(&self, value: u64) -> Vec<String> {
        if value % 2 == 0 {
            // single form encoded directly in the value
            vec![self.strings[((value - 2) / 2) as usize].clone()]
        } else {
            // extended row
            self.rows[((value - 1) / 2) as usize]
                .iter()
                .map(|i| self.strings[*i as usize].clone())
                .collect()
        }
    }

    /// Lookup all inflected forms of `lemma` with POS tag `pos`.
    /// Mirrors `BaseSynthesizer.lookup`.
    pub fn lookup(&self, lemma: &str, pos: &str) -> Vec<String> {
        let mut results = Vec::new();

        let key = format!("{}|{}", lemma, pos);
        if !self.map_bytes.is_empty() {
            if let Some(value) = self.map().get(&key) {
                results = self.value_forms(value);
            }
        }

        if let Some(manual_forms) = self.manual.get(&(lemma.to_string(), pos.to_string())) {
            results.extend(manual_forms.iter().cloned());
        }

        if let Some(remove_forms) = self.removed.get(&(lemma.to_string(), pos.to_string())) {
            results.retain(|x| !remove_forms.contains(x));
        }

        results
    }

    /// Get all inflected forms of `lemma` where the POS tag matches `pos_regex`
    /// (compiled for a full match, mirroring Java's `Pattern.matches()`).
    /// Mirrors `BaseSynthesizer.synthesize(token, posTag, true)`.
    /// The returned forms are deduplicated and sorted, like LanguageTool's TreeSet behavior.
    pub fn synthesize_regex(&self, lemma: &str, pos_regex: &Regex) -> Vec<String> {
        if self.kind == SynthesizerKind::Catalan {
            return self.synthesize_regex_catalan(lemma, pos_regex);
        }
        self.synthesize_regex_base(lemma, pos_regex)
    }

    /// Mirrors `CatalanSynthesizer.synthesize(token, posTag, true)`: regional
    /// verb variants are encoded in the last tag character (the base dictionary
    /// is the valencia one), so an empty first pass retries with the
    /// central-variant (`ca-ES`) tag set. Lemmas with a space synthesize the
    /// verb part and re-append the rest.
    fn synthesize_regex_catalan(&self, lemma: &str, pos_regex: &Regex) -> Vec<String> {
        const LEMMAS_TO_IGNORE: [&str; 4] = ["enterar", "sentar", "conseguir", "alcançar"];
        if LEMMAS_TO_IGNORE.contains(&lemma) {
            return Vec::new();
        }
        let pattern = pos_regex.as_str();
        let (verb_lemma, add_after) = if pattern.starts_with('V') {
            match lemma.find(' ') {
                Some(i) => (&lemma[..i], Some(&lemma[i + 1..])),
                None => (lemma, None),
            }
        } else {
            (lemma, None)
        };
        let append = |mut results: Vec<String>| -> Vec<String> {
            if let Some(rest) = add_after {
                results = results
                    .into_iter()
                    .map(|form| format!("{} {}", form, rest))
                    .collect();
            }
            results
        };
        let results = self.synthesize_regex_base(verb_lemma, pos_regex);
        if !results.is_empty() {
            return append(results);
        }
        // verbs whose last tag char encodes the regional variant
        if pattern.starts_with('V')
            && pattern
                .chars()
                .last()
                .map(|c| "CVBXYZ0123456".contains(c))
                .unwrap_or(false)
        {
            let variant = format!("{}[0CXY12]", &pattern[..pattern.len() - 1]);
            let results = self.synthesize_regex_base(verb_lemma, &Regex::new(variant));
            return append(results);
        }
        append(results)
    }

    fn synthesize_regex_base(&self, lemma: &str, pos_regex: &Regex) -> Vec<String> {
        use fst::{IntoStreamer, Streamer};

        let mut results = Vec::new();

        // stream all keys with the prefix "lemma|"
        let prefix = format!("{}|", lemma);
        let map = self.map();
        let mut stream = map
            .range()
            .ge(prefix.as_bytes())
            .into_stream();

        while let Some((key, row)) = stream.next() {
            let key = String::from_utf8_lossy(key);
            if !key.starts_with(&prefix) {
                break;
            }

            let pos = &key[prefix.len()..];
            if pos_regex.is_match(pos) {
                results.extend(self.value_forms(row));
            }
        }

        for ((m_lemma, m_pos), forms) in self.manual.iter() {
            if m_lemma == lemma && pos_regex.is_match(m_pos) {
                results.extend(forms.iter().cloned());
            }
        }

        for ((r_lemma, r_pos), forms) in self.removed.iter() {
            if r_lemma == lemma && pos_regex.is_match(r_pos) {
                results.retain(|x| !forms.contains(x));
            }
        }

        results.sort();
        results.dedup();
        results
    }

    /// Mirrors `BaseSynthesizer.getTargetPosTag`: by default, the last of the
    /// matched POS tags is returned. The Catalan synthesizer sorts the tags
    /// with `PostagComparator` first (3rd person > 1st, indicative > other
    /// moods).
    pub fn get_target_pos_tag(&self, pos_tags: &[&str], target_pos_tag: &str) -> String {
        if pos_tags.is_empty() {
            return target_pos_tag.to_string();
        }
        if self.kind == SynthesizerKind::Catalan {
            let mut sorted: Vec<&str> = pos_tags.to_vec();
            sorted.sort_by(|a, b| postag_comparator(a, b));
            return sorted[sorted.len() - 1].to_string();
        }
        // return the last one to keep the previous results
        pos_tags[pos_tags.len() - 1].to_string()
    }

    /// Mirrors `BaseSynthesizer.getPosTagCorrection` (and overrides e.g. in German).
    pub fn pos_tag_correction(&self, pos_tag: String) -> String {
        match self.kind {
            SynthesizerKind::German | SynthesizerKind::Default | SynthesizerKind::Catalan => pos_tag,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.map_bytes.is_empty()
    }
}

#[cfg(feature = "compile")]
mod build {
    use super::*;

    /// Builds a synthesizer from the build directory dumps:
    /// * `synth.dump`: lines of `lemma|POS<TAB>form`, **sorted by key**
    /// * manual lists: lines of `form<TAB>lemma<TAB>POS`
    ///
    /// Missing files are simply skipped.
    pub(crate) fn from_dumps(
        synth_dump: &std::path::Path,
        manual: &[std::path::PathBuf],
        kind: SynthesizerKind,
    ) -> Synthesizer {
        use std::collections::HashMap;
        use std::io::{BufRead, BufReader};

        let mut synthesizer = Synthesizer::new(kind);

        // interned strings
        let mut string_ids: HashMap<String, u32> = HashMap::new();

        // build the FST streaming over the sorted dump
        if let Ok(file) = std::fs::File::open(synth_dump) {
            let reader = BufReader::new(file);

            let mut map_builder = fst::MapBuilder::memory();
            let mut rows: Vec<Vec<u32>> = Vec::new();
            let mut strings: Vec<String> = Vec::new();
            // consecutive identical keys are merged (multi-form rows are rare)
            let mut current_key: Option<(String, Vec<u32>)> = None;

            macro_rules! flush_current {
                () => {
                    if let Some((key, form_ids)) = current_key.take() {
                        let value = if form_ids.len() == 1 {
                            // encode the single form directly in the FST value
                            form_ids[0] as u64 * 2 + 2
                        } else {
                            rows.push(form_ids.clone());
                            // odd values mark an extended row
                            rows.len() as u64 * 2 - 1
                        };
                        map_builder
                            .insert(key.as_str(), value)
                            .expect("keys must be sorted");
                    }
                };
            }

            for line in reader.lines() {
                let line = match line {
                    Ok(line) => line,
                    Err(_) => continue,
                };
                if line.is_empty() {
                    continue;
                }

                let mut parts = line.splitn(2, '\t');
                let key = parts.next().unwrap_or("");
                let form = match parts.next() {
                    Some(form) => form,
                    None => continue,
                };
                if key.is_empty() || !key.contains('|') {
                    continue;
                }

                let form_id = match string_ids.get(form) {
                    Some(id) => *id,
                    None => {
                        let id = strings.len() as u32;
                        strings.push(form.to_string());
                        string_ids.insert(form.to_string(), id);
                        id
                    }
                };

                match &mut current_key {
                    Some((current, form_ids)) if current == key => {
                        form_ids.push(form_id);
                    }
                    _ => {
                        flush_current!();
                        current_key = Some((key.to_string(), vec![form_id]));
                    }
                }
            }
            flush_current!();

            log::info!(
                "Synthesizer FST: {} extended rows, {} forms",
                rows.len(),
                strings.len()
            );

            synthesizer.map_bytes = map_builder.into_inner().expect("FST must be buildable");
            synthesizer.rows = rows;
            synthesizer.strings = strings;
        }

        for path in manual {
            if let Ok(content) = std::fs::read_to_string(path) {
                for line in content.lines() {
                    let line = line.trim_start();
                    if line.is_empty() || line.starts_with('#') {
                        continue;
                    }

                    let mut parts = line.split('\t');
                    let form = parts.next().unwrap_or("").to_string();
                    let lemma = parts.next().unwrap_or("").to_string();
                    let pos = parts.next().unwrap_or("").to_string();

                    if form.is_empty() || lemma.is_empty() || pos.is_empty() {
                        continue;
                    }

                    if path.file_name().map_or(false, |x| x == "removed.txt")
                        || path
                            .file_name()
                            .map_or(false, |x| x == "do-not-synthesize.txt")
                    {
                        synthesizer.insert_removed(lemma, pos, form);
                    } else {
                        synthesizer.insert_manual(lemma, pos, form);
                    }
                }
            }
        }

        synthesizer
    }
}

#[cfg(feature = "compile")]
pub(crate) use build::from_dumps;

/// `CatalanSynthesizer.PostagComparator`: gives priority to 3rd person over
/// 1st and to the indicative over other moods.
fn postag_comparator(a: &str, b: &str) -> std::cmp::Ordering {
    use std::cmp::Ordering;
    let ca: Vec<char> = a.chars().collect();
    let cb: Vec<char> = b.chars().collect();
    if ca.len() > 4 && cb.len() > 4 {
        if a.contains("3S") && b == "1S" {
            return Ordering::Greater;
        }
        if a.contains("1S") && b.contains("3S") {
            return Ordering::Less;
        }
        if a == "VMIP2P00" && b == "VMIS3S00" {
            return Ordering::Greater;
        }
        if b == "VMIP2P00" && a == "VMIS3S00" {
            return Ordering::Less;
        }
        if ca[2] == 'I' && cb[2] != 'I' {
            return Ordering::Greater;
        }
        if cb[2] == 'I' && ca[2] != 'I' {
            return Ordering::Less;
        }
        if ca[4] == '3' && cb[4] == '1' {
            return Ordering::Greater;
        }
        if cb[4] == '1' && ca[4] == '3' {
            return Ordering::Less;
        }
    }
    Ordering::Equal
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn manual_and_removed() {
        let mut synth = Synthesizer::new(SynthesizerKind::Default);
        synth.insert_manual("run".into(), "VBZ".into(), "runs".into());
        synth.insert_removed("walk".into(), "VBP".into(), "walk".into());

        assert_eq!(synth.lookup("run", "VBZ"), vec!["runs".to_string()]);
        assert!(synth.lookup("walk", "VBP").is_empty());
        assert_eq!(
            synth.get_target_pos_tag(&["NN", "VBZ"], "XX"),
            "VBZ".to_string()
        );
    }
}
