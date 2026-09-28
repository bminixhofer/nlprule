//! Port of LanguageTool's `ArabicTagger` prefix/suffix stemming
//! (`additionalTags`) and the `ArabicTagManager` flag arithmetic it needs
//! (`modifyPosTag`/`addTag`, LT 6.5). Runs after the plain dictionary lookup
//! so that e.g. "السلامة" (definite article + noun) gets the readings of
//! "سلامة" with the merged definite/procletic flags, like the Java tagger.

use crate::rule::filter_java::{
    ar_get_flag, ar_is_majrour, ar_is_noun, ar_is_stopword, ar_is_unattached_noun, ar_is_verb,
    ar_set_flag,
};

fn ar_is_future_tense(postag: &str) -> bool {
    ar_is_verb(postag) && ar_get_flag(postag, "TENSE") == 'f'
}

/// Port of `ArabicTagManager.addTag` (single flag). `flag_string` uses LT's
/// `TYPE;FLAG` notation (`"CONJ;W"`); a bare `"W"` has no type.
pub(crate) fn ar_add_tag_string(postag: &str, flag_string: &str) -> Option<String> {
    let mut it = flag_string.splitn(2, ';');
    let first = it.next().unwrap_or("");
    let (flag_type, flag) = match it.next() {
        Some(second) => (first, second),
        None => ("", first),
    };
    ar_add_tag(postag, flag_type, flag)
}

fn ar_add_tag(postag: &str, flag_type: &str, flag: &str) -> Option<String> {
    let mut postag = postag.to_string();
    match flag {
        "W" => {
            postag = ar_set_flag(&postag, "CONJ", 'W');
        }
        "K" => {
            if ar_is_noun(&postag) {
                if ar_is_majrour(&postag) {
                    postag = ar_set_flag(&postag, "JAR", 'K');
                } else {
                    return None;
                }
            } else {
                return None;
            }
        }
        "B" => {
            if ar_is_noun(&postag) {
                if ar_is_majrour(&postag) {
                    postag = ar_set_flag(&postag, "JAR", 'B');
                } else {
                    return None;
                }
            } else {
                return None;
            }
        }
        "L" => {
            if ar_is_noun(&postag) {
                if ar_is_majrour(&postag) {
                    postag = ar_set_flag(&postag, "JAR", 'L');
                } else {
                    return None;
                }
            } else {
                // verb case
                postag = ar_set_flag(&postag, "ISTIQBAL", 'L');
            }
        }
        "D" => {
            // the noun must not be attached (note: LT stores the definite
            // flag as 'L' in the PRONOUN section - kept faithfully)
            if ar_is_unattached_noun(&postag) {
                postag = ar_set_flag(&postag, "PRONOUN", 'L');
            } else {
                return None;
            }
        }
        "S" => {
            if ar_is_future_tense(&postag) {
                postag = ar_set_flag(&postag, "ISTIQBAL", 'S');
            } else {
                return None;
            }
        }
        _ => {}
    }
    // case of Pronoun
    if flag_type == "PRONOUN" && !flag.is_empty() && flag != "D" {
        postag = ar_set_flag(&postag, flag_type, flag.chars().next().unwrap_or('-'));
    }
    Some(postag)
}

/// Port of `ArabicTagManager.modifyPosTag`: apply all flags, `None` as soon
/// as one is incompatible.
fn ar_modify_pos_tag(postag: &str, tags: &[String]) -> Option<String> {
    let mut postag = postag.to_string();
    for tag in tags {
        postag = ar_add_tag_string(&postag, tag)?;
    }
    Some(postag)
}

pub(crate) fn remove_tashkeel(word: &str) -> String {
    word.chars()
        .filter(|c| !matches!(*c as u32, 0x64B..=0x652 | 0x670 | 0x640))
        .collect()
}

/// The single-flag operations `getTags` can produce (old-style pronoun tags,
/// LT's default).
const FLAG_OPS: [&str; 7] = [
    "CONJ;W", "JAR;K", "JAR;B", "JAR;L", "PRONOUN;D", "ISTIQBAL;S", "PRONOUN;H",
];

/// Closure of `ar_add_tag_string` over `ops`, applied up to `depth` times.
/// Used at compile time to extend the tag store so the merged postags get an
/// id at runtime.
pub(crate) fn expand_tag_closure(tags: Vec<String>, depth: usize) -> Vec<String> {
    let mut seen: std::collections::HashSet<String> = tags.iter().cloned().collect();
    let mut frontier = tags;
    for _ in 0..depth {
        let mut next = Vec::new();
        for tag in &frontier {
            for op in FLAG_OPS {
                if let Some(merged) = ar_add_tag_string(tag, op) {
                    if seen.insert(merged.clone()) {
                        next.push(merged);
                    }
                }
            }
        }
        if next.is_empty() {
            break;
        }
        frontier = next;
    }
    seen.into_iter().collect()
}

/// Prefix split points as BYTE offsets (LT counts characters; the affixes
/// are all single Arabic letters, 2 bytes each after tashkeel stripping).
fn get_prefix_index_list(word: &str) -> Vec<usize> {
    let mut indexes = vec![0usize];
    let starts = |pats: &[&str]| pats.iter().any(|p| word.starts_with(p));
    let char_n = |n: usize| word
        .char_indices()
        .nth(n)
        .map(|(b, _)| b)
        .unwrap_or(word.len());

    // four letters
    if starts(&["وكال", "وبال", "فكال", "فبال"]) {
        indexes.push(char_n(4));
    }
    // three letters
    if starts(&["ولل", "فلل", "فال", "وال", "بال", "كال"]) {
        indexes.push(char_n(3));
    }
    // two letters
    if starts(&[
        "لل", "وك", "ول", "وب", "فك", "فل", "فب", "ال", "فسأ", "فسن", "فسي", "فست", "وسأ", "وسن",
        "وسي", "وست",
    ]) {
        indexes.push(char_n(2));
    }
    // one letter
    if starts(&["ك", "ل", "ب", "و", "ف", "سأ", "سن", "سي", "ست"]) {
        indexes.push(char_n(1));
    }
    indexes
}

fn get_suffix_index_list(word: &str) -> Vec<usize> {
    let mut indexes = vec![word.len()];
    let strip_bytes = |n_chars: usize| {
        word.char_indices()
            .rev()
            .nth(n_chars - 1)
            .map(|(b, _)| b)
            .unwrap_or(0)
    };
    let suffix_pos = if word.ends_with('ك') {
        strip_bytes(1)
    } else if word.ends_with("هما") || word.ends_with("كما") {
        strip_bytes(3)
    } else if word.ends_with("ها")
        || word.ends_with("هم")
        || word.ends_with("هن")
        || word.ends_with("كم")
        || word.ends_with("كن")
        || word.ends_with("نا")
    {
        strip_bytes(2)
    } else {
        return indexes;
    };
    indexes.push(suffix_pos);
    indexes
}

/// Port of `ArabicTagger.getTags`: the flag list derived from the split
/// prefix/suffix.
fn get_tags(word: &str, pos_start: usize, pos_end: usize) -> Vec<String> {
    // byte offsets: all Arabic affix chars are single-byte-safe here because
    // tashkeel was stripped and Arabic letters are 2-byte UTF-8 but slicing
    // works on char boundaries of the original string
    let prefix = &word[..pos_start];
    let suffix = &word[pos_end..];
    let mut tags = Vec::new();

    let mut prefix = prefix.to_string();
    // prefixes - first place
    if prefix.starts_with('و') || prefix.starts_with('ف') {
        tags.push("CONJ;W".to_string());
        prefix = prefix
            .strip_prefix('و')
            .or_else(|| prefix.strip_prefix('ف'))
            .unwrap_or(&prefix)
            .to_string();
    }
    // second place
    if prefix.starts_with('ك') {
        tags.push("JAR;K".to_string());
    } else if prefix.starts_with('ل') {
        tags.push("JAR;L".to_string());
    } else if prefix.starts_with('ب') {
        tags.push("JAR;B".to_string());
    } else if prefix.starts_with('س') {
        tags.push("ISTIQBAL;S".to_string());
    }
    // last place
    if prefix.ends_with("ال") || prefix.ends_with("لل") {
        tags.push("PRONOUN;D".to_string());
    }
    // suffixes (old-style pronoun tags)
    if matches!(
        suffix,
        "ني" | "نا" | "ك" | "كما" | "كم" | "كن" | "ه" | "ها" | "هما" | "هم" | "هن"
    ) {
        tags.push("PRONOUN;H".to_string());
    }
    tags
}

/// Port of `ArabicTagger.getStem`.
fn get_stem(word: &str, pos_start: usize, pos_end: usize) -> Vec<String> {
    let mut stem = word[pos_start..].to_string();
    if pos_end != word.len() {
        // convert attached pronouns to the ه model form
        for pronoun in ["هما", "كما", "ها", "هم", "هن", "كم", "كن", "نا", "ك"] {
            if let Some(stripped) = stem.strip_suffix(pronoun) {
                stem = format!("{}ه", stripped);
                break;
            }
        }
    }
    let mut stems = Vec::new();
    let prefix = &word[..pos_start];
    if prefix.ends_with("لل") {
        stems.push(format!("ل{}", stem)); // للاعب => ل + لاعب
    }
    stems.push(stem);
    stems
}

/// Port of `ArabicTagger.additionalTags`: derive extra readings for `word`
/// from the dictionary readings of its stripped stems. `lookup` returns the
/// `(lemma, postag)` pairs of a stem.
pub(crate) fn additional_tags<F>(word: &str, lookup: F) -> Vec<(String, String)>
where
    F: Fn(&str) -> Vec<(String, String)>,
{
    let striped = remove_tashkeel(word);
    let mut out = Vec::new();

    for i in get_prefix_index_list(&striped) {
        for j in get_suffix_index_list(&striped) {
            if i == 0 && j == striped.len() {
                continue;
            }
            if i > j || j > striped.len() {
                continue;
            }
            let tags = get_tags(&striped, i, j);
            for stem in get_stem(&striped, i, j) {
                for (lemma, postag) in lookup(&stem) {
                    if let Some(merged) = ar_modify_pos_tag(&postag, &tags) {
                        out.push((lemma, merged));
                    }
                }
            }
        }
    }
    out
}

/// Whether the plain readings make the word a stop word (no additional
/// stemming then, like `ArabicTagger.isStopWord`).
pub(crate) fn is_stopword_reading(readings: &[(String, String)]) -> bool {
    readings
        .iter()
        .any(|(_, pos)| ar_is_stopword(pos))
}
