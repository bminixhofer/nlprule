//! Creates the nlprule binaries from a *build directory*. Usage information in /build/README.md.

use fs::File;
use fs_err as fs;

use std::{
    hash::{Hash, Hasher},
    io::{self, BufReader, BufWriter},
    num::ParseIntError,
    path::{Path, PathBuf},
    str::FromStr,
    sync::Arc,
};

use crate::{
    rules::Rules,
    tokenizer::{chunk::Chunker, multiword::MultiwordTagger, tag::Tagger, Tokenizer},
    types::DefaultHasher,
};
use log::info;

use self::parse_structure::{BuildInfo, RegexCache};
use thiserror::Error;

mod drop_audit;
mod impls;

/// Debug helper exposing failing rule XML.
mod parse_structure;
mod structure;
mod utils;

struct BuildFilePaths {
    lang_code_path: PathBuf,
    tag_paths: Vec<PathBuf>,
    tag_remove_paths: Vec<PathBuf>,
    chunker_path: PathBuf,
    disambiguation_path: PathBuf,
    grammar_path: PathBuf,
    multiword_tag_path: PathBuf,
    common_words_path: PathBuf,
    regex_cache_path: PathBuf,
    srx_path: PathBuf,
    synth_dump_path: PathBuf,
    do_not_synthesize_path: PathBuf,
    speller_wordlist_path: PathBuf,
    multitoken_list_path: PathBuf,
    confusion_pairs_path: PathBuf,
    added_compound_path: PathBuf,
    simple_replace_path: PathBuf,
}

impl BuildFilePaths {
    fn new<P: AsRef<Path>>(build_dir: P) -> Self {
        let p = build_dir.as_ref();
        BuildFilePaths {
            lang_code_path: p.join("lang_code.txt"),
            tag_paths: vec![p.join("tags/output.dump"), p.join("tags/added.txt")],
            tag_remove_paths: vec![p.join("tags/removed.txt")],
            chunker_path: p.join("chunker.json"),
            disambiguation_path: p.join("disambiguation.xml"),
            grammar_path: p.join("grammar.xml"),
            multiword_tag_path: p.join("tags/multiwords.txt"),
            common_words_path: p.join("common.txt"),
            regex_cache_path: p.join("regex_cache.bin"),
            srx_path: p.join("segment.srx"),
            synth_dump_path: p.join("tags/synth.dump"),
            do_not_synthesize_path: p.join("tags/do-not-synthesize.txt"),
            speller_wordlist_path: p.join("filters/speller.txt"),
            multitoken_list_path: p.join("filters/multitoken.txt"),
            confusion_pairs_path: p.join("filters/confusion_pairs.txt"),
            added_compound_path: p.join("filters/added_compound.txt"),
            simple_replace_path: p.join("filters/replace.txt"),
        }
    }
}

#[derive(Error, Debug)]
#[allow(missing_docs)]
pub enum Error {
    #[error(transparent)]
    Io(#[from] std::io::Error),
    #[error(transparent)]
    Serialization(#[from] bincode::Error),
    #[error(transparent)]
    NlpruleError(#[from] crate::Error),
    #[error(transparent)]
    Json(#[from] serde_json::Error),
    #[error(transparent)]
    Srx(#[from] srx::Error),
    #[error("language options do not exist for '{lang_code}'")]
    LanguageOptionsDoNotExist { lang_code: String },
    #[error(transparent)]
    RegexSyntax(#[from] regex_syntax::ast::Error),
    #[error("regex compilation error: {0}")]
    Regex(Box<dyn std::error::Error + Send + Sync + 'static>),
    #[error("unexpected condition: {0}")]
    Unexpected(String),
    #[error("feature not implemented: {0}")]
    Unimplemented(String),
    #[error(transparent)]
    ParseError(#[from] ParseIntError),
    #[error("unknown error: {0}")]
    Other(#[from] Box<dyn std::error::Error + Send + Sync + 'static>),
}

/// Compiles the binaries from a build directory.
pub fn compile(
    build_dir: impl AsRef<Path>,
    rules_dest: impl io::Write,
    tokenizer_dest: impl io::Write,
) -> Result<(), Error> {
    let paths = BuildFilePaths::new(&build_dir);

    let lang_code = fs::read_to_string(paths.lang_code_path)?;

    info!(
        "Reading common words from {}.",
        paths.common_words_path.display()
    );
    let common_words = fs::read_to_string(paths.common_words_path)?
        .lines()
        .map(|x| x.to_string())
        .collect();

    let tokenizer_lang_options = utils::tokenizer_lang_options(&lang_code).ok_or_else(|| {
        Error::LanguageOptionsDoNotExist {
            lang_code: lang_code.clone(),
        }
    })?;

    let rules_lang_options =
        utils::rules_lang_options(&lang_code).ok_or_else(|| Error::LanguageOptionsDoNotExist {
            lang_code: lang_code.clone(),
        })?;

    let tagger_lang_options =
        utils::tagger_lang_options(&lang_code).ok_or_else(|| Error::LanguageOptionsDoNotExist {
            lang_code: lang_code.clone(),
        })?;

    info!("Creating tagger.");
    let tagger = Tagger::from_dumps(
        &paths.tag_paths,
        &paths.tag_remove_paths,
        &common_words,
        tagger_lang_options,
    )?;

    let mut hasher = DefaultHasher::default();
    let mut word_store = tagger.word_store().iter().collect::<Vec<_>>();
    word_store.sort_by(|a, b| a.1.cmp(b.1));
    word_store.hash(&mut hasher);
    let word_store_hash = hasher.finish();

    let regex_cache = if let Ok(file) = File::open(&paths.regex_cache_path) {
        let cache: RegexCache = bincode::deserialize_from(BufReader::new(file))?;
        if *cache.word_hash() == word_store_hash {
            info!(
                "Regex cache at {} is valid.",
                paths.regex_cache_path.display()
            );
            cache
        } else {
            info!("Regex cache was provided but is not valid. Rebuilding.");
            RegexCache::new(word_store_hash)
        }
    } else {
        info!(
            "No regex cache provided. Building and writing to {}.",
            paths.regex_cache_path.display()
        );
        RegexCache::new(word_store_hash)
    };

    let mut build_info = BuildInfo::new(Arc::new(tagger), regex_cache);
    build_info.set_lang(lang_code.clone());

    // auxiliary data for the ported Java rule filters
    let filter_data = build_filter_data(&BuildFilePaths::new(&build_dir), lang_code.trim());

    // build the morphological synthesizer (used to inflect lemmas in suggestions)
    // from the synth dictionary dump and the manual addition / removal lists
    let synth_kind = match lang_code.trim() {
        "de" => crate::rule::synthesizer::SynthesizerKind::German,
        "ca" => crate::rule::synthesizer::SynthesizerKind::Catalan,
        _ => crate::rule::synthesizer::SynthesizerKind::Default,
    };
    let synthesizer = crate::rule::synthesizer::from_dumps(
        &paths.synth_dump_path,
        &[
            paths.tag_paths[1].clone(),
            paths.tag_remove_paths[0].clone(),
            paths.do_not_synthesize_path.clone(),
        ],
        synth_kind,
    );

    if synthesizer.is_empty() {
        info!(
            "No synthesis dictionary found at {}. `match` elements with a `postag` \
             will fall back to the plain token text.",
            paths.synth_dump_path.display()
        );
    } else {
        info!("Built morphological synthesizer.");
    }

    build_info.set_synthesizer(Some(Arc::new(synthesizer)));
    let chunker = if paths.chunker_path.exists() {
        info!("{} exists. Building chunker.", paths.chunker_path.display());
        let reader = BufReader::new(File::open(paths.chunker_path)?);
        let chunker = Chunker::from_json(reader)?;
        Some(chunker)
    } else {
        None
    };
    let multiword_tagger = if paths.multiword_tag_path.exists() {
        info!(
            "{} exists. Building multiword tagger.",
            paths.multiword_tag_path.display()
        );
        Some(MultiwordTagger::from_dump(
            paths.multiword_tag_path,
            &build_info,
        )?)
    } else {
        None
    };

    info!("Creating tokenizer.");
    let tokenizer = Tokenizer::from_xml(
        &paths.disambiguation_path,
        &mut build_info,
        chunker,
        multiword_tagger,
        srx::SRX::from_str(&fs::read_to_string(&paths.srx_path)?)?.language_rules(lang_code.clone()),
        tokenizer_lang_options,
    )?;
    tokenizer.to_writer(tokenizer_dest)?;

    build_info.set_lang(lang_code.clone());
    info!("Creating grammar rules.");
    let rules = Rules::from_xml(&paths.grammar_path, &mut build_info, rules_lang_options, filter_data);
    rules.to_writer(rules_dest)?;

    // we need to write the regex cache after building the rules, otherwise it isn't fully populated
    let f = BufWriter::new(File::create(&paths.regex_cache_path)?);
    bincode::serialize_into(f, build_info.mut_regex_cache())?;

    Ok(())
}

/// Builds the [FilterData] (speller, multitoken table, confusion pairs,
/// compound parts) from the optional `filters/` files of the build dir.
fn build_filter_data(
    paths: &BuildFilePaths,
    lang_code: &str,
) -> crate::rule::filter_data::FilterData {
    use crate::rule::filter_data::*;
    use std::collections::HashMap;

    let mut data = FilterData {
        multitoken_speller_check: matches!(lang_code, "en" | "de" | "pt" | "nl"),
        ..Default::default()
    };

    // speller word list -> FST
    if paths.speller_wordlist_path.exists() {
        let text = fs::read_to_string(&paths.speller_wordlist_path).unwrap_or_default();
        let mut forms: Vec<String> = Vec::new();
        let mut seen = std::collections::HashSet::new();
        for line in text.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') {
                continue;
            }
            if seen.insert(line.to_string()) {
                forms.push(line.to_string());
            }
        }

        // optional frequency table (morfologik's 0..=25 rank per form)
        let mut freq_by_form: HashMap<String, u8> = HashMap::new();
        let freq_path = paths.speller_wordlist_path.with_file_name("freq.txt");
        if freq_path.exists() {
            if let Ok(text) = fs::read_to_string(&freq_path) {
                for line in text.lines() {
                    let mut it = line.splitn(2, '\t');
                    let form = it.next().unwrap_or("").trim();
                    let freq: u8 = it.next().unwrap_or("0").trim().parse().unwrap_or(0);
                    if !form.is_empty() {
                        freq_by_form.insert(form.to_string(), freq.min(25));
                    }
                }
            }
        }

        // sorted key -> (index << 5) | frequency. Dictionaries without
        // case conversion (pl: encoder=none) keep original-case keys
        let case_sensitive = matches!(lang_code, "pl");
        let mut keyed: Vec<(String, usize, u8)> = forms
            .iter()
            .enumerate()
            .map(|(i, f)| {
                (
                    if case_sensitive {
                        f.clone()
                    } else {
                        f.to_lowercase()
                    },
                    i,
                    freq_by_form.get(f).copied().unwrap_or(0),
                )
            })
            .collect();
        keyed.sort_by(|a, b| a.0.cmp(&b.0));
        keyed.dedup_by(|a, b| a.0 == b.0);

        let mut builder = fst::MapBuilder::memory();
        for (key, idx, freq) in keyed {
            let _ = builder.insert(key.as_bytes(), ((idx as u64) << 5) | freq as u64);
        }
        data.speller = Some(SpellerDict {
            map_bytes: builder.into_inner().unwrap_or_default(),
            forms,
            case_sensitive: false,
        });
    }

    // multitoken suggestion list
    if paths.multitoken_list_path.exists() {
        let text = fs::read_to_string(&paths.multitoken_list_path).unwrap_or_default();
        let mut suggester = MultitokenSuggester::default();
        for line in text.lines() {
            let line = line.split('#').next().unwrap_or("").trim().to_string();
            if line.is_empty() {
                continue;
            }
            let key = MultitokenSuggester::normalize_key(&line);
            if !key.contains(' ') {
                // one-token suggestions are provided by other rules
                continue;
            }
            let first_char = key.chars().next().unwrap_or(' ');
            suggester
                .by_char
                .entry(first_char)
                .or_default()
                .entry(key.clone())
                .or_default()
                .push(line.clone());
            suggester
                .no_spaces
                .entry(key.replace(' ', ""))
                .or_default()
                .push(line);
        }
        data.multitoken = Some(suggester);
    }

    // confusion pairs: wrong;correct;POSTAG
    if paths.confusion_pairs_path.exists() {
        let text = fs::read_to_string(&paths.confusion_pairs_path).unwrap_or_default();
        let mut map: HashMap<String, Vec<(String, String)>> = HashMap::new();
        for line in text.lines() {
            let line = line.trim();
            if line.is_empty() || line.starts_with('#') {
                continue;
            }
            let parts: Vec<&str> = line.split(';').collect();
            if parts.len() != 3 {
                continue;
            }
            map.entry(parts[0].trim().to_lowercase())
                .or_default()
                .push((parts[1].trim().to_string(), parts[2].trim().to_string()));
        }
        data.confusion_pairs = Some(map);
    }

    // German added compounds: part1;part2
    if paths.added_compound_path.exists() {
        let text = fs::read_to_string(&paths.added_compound_path).unwrap_or_default();
        let mut map: HashMap<String, Vec<String>> = HashMap::new();
        for line in text.lines() {
            let line = line.split('#').next().unwrap_or("").trim();
            if line.is_empty() {
                continue;
            }
            let parts: Vec<&str> = line.split(';').collect();
            if parts.len() != 2 {
                continue;
            }
            map.entry(parts[0].trim().to_lowercase())
                .or_default()
                .push(parts[1].trim().to_lowercase());
        }
        data.added_compound = Some(map);
    }

    // English contractions (ContractionSpellingRule, an older
    // AbstractSimpleReplaceRule without sub-rule ids, case-sensitive)
    let contractions_path = paths.simple_replace_path.parent().unwrap().join("contractions.txt");
    if lang_code.trim() == "en" && contractions_path.exists() {
        let text = fs::read_to_string(&contractions_path).unwrap_or_default();
        let mut table = crate::rule::filter_data::SimpleReplaceTable {
            prefix: "EN_CONTRACTION_SPELLING".to_string(),
            sub_ids: false,
            by_first_word: Default::default(),
        };
        for line in text.lines() {
            let line = line.split('#').next().unwrap_or("").trim().to_string();
            if line.is_empty() || !line.contains('=') {
                continue;
            }
            let (wrong, correct) = line.split_once('=').unwrap();
            let wrong = wrong.trim();
            let corrects: Vec<String> = correct
                .split('|')
                .map(|c| c.trim().to_string())
                .filter(|c| !c.is_empty())
                .collect();
            if wrong.is_empty() || corrects.is_empty() {
                continue;
            }
            table
                .by_first_word
                .entry(wrong.to_string())
                .or_default()
                .push((wrong.to_string(), corrects));
        }
        data.simple_replace = Some(table);
    }

    // SimpleReplaceRule phrase table (nl: rules/nl/replace.txt, 66k lines -
    // too large to compile as per-entry grammar rules, loaded as a lookup
    // table like LT's dictionary-backed rule)
    if paths.simple_replace_path.exists() {
        let text = fs::read_to_string(&paths.simple_replace_path).unwrap_or_default();
        let mut table = crate::rule::filter_data::SimpleReplaceTable {
            prefix: format!("{}_SIMPLE_REPLACE", lang_code.trim().to_uppercase()),
            // only Dutch's AbstractSimpleReplaceRule2 assigns per-entry
            // sub-rule ids; ar's older rule reports the plain id
            sub_ids: lang_code.trim() == "nl",
            by_first_word: Default::default(),
        };
        let mut n = 0usize;
        for line in text.lines() {
            let line = line.split('#').next().unwrap_or("").trim().to_string();
            if line.is_empty() || !line.contains('=') {
                continue;
            }
            let (wrong, correct) = line.split_once('=').unwrap();
            let wrong = wrong.trim();
            let corrects: Vec<String> = correct
                .split('|')
                .map(|c| c.trim().to_string())
                .filter(|c| !c.is_empty())
                .collect();
            if wrong.is_empty() || corrects.is_empty() {
                continue;
            }
            if let Some(first) = wrong.split_whitespace().next() {
                table
                    .by_first_word
                    .entry(first.to_string())
                    .or_default()
                    .push((wrong.to_string(), corrects));
                n += 1;
            }
        }
        info!("Loaded {} simple-replace phrases.", n);
        data.simple_replace = Some(table);
    }

    data
}
