//! Runs the rule set over sentences read from stdin (one per line) and
//! prints one JSON object per line: the rule IDs and spans that fired.
//! Used by `build/compare_server.py` to compare against the official
//! LanguageTool HTTP server.

use nlprule::{Rules, Tokenizer};
use std::io::{self, BufRead, Write};

fn main() {
    let args: Vec<String> = std::env::args().collect();
    // `--dump-rules <rules.bin> [lang]`: inventory of loaded rules for the
    // coverage audit tool; prints one JSON object and exits.
    if args.get(1).map_or(false, |a| a == "--dump-rules") {
        let rules = match args.get(3) {
            Some(lang) => Rules::with_lang(&args[2], lang).unwrap(),
            None => Rules::new(&args[2]).unwrap(),
        };
        serde_json::ser::to_writer(io::stdout(), &rules.dump_rules()).unwrap();
        println!();
        return;
    }
    let mut tokenizer = Tokenizer::new(&args[1]).unwrap();
    // optional third argument: the language code, enables the language
    // specific behavior of the built-in Java-only rules and the
    // language-specific tokenizer modes (Breton apostrophes)
    if args.get(3).map_or(false, |l| l == "br") {
        tokenizer.set_breton_apostrophes(true);
    }
    // LT language-specific tokenizers: English apostrophe/hyphen glue and
    // Catalan contraction splits are runtime flags (not serialized in the
    // tokenizer bin)
    match args.get(3).map(|l| l.as_str()) {
        Some("en") | Some("en-US") | Some("en-GB") => {
            tokenizer.set_english_apostrophes(true)
        }
        Some("ca") => tokenizer.set_split_contractions("ca"),
        _ => {}
    }
    let mut rules = match args.get(3) {
        Some(lang) => Rules::with_lang(&args[2], lang).unwrap(),
        None => Rules::new(&args[2]).unwrap(),
    };
    // optional NLPRULE_ENABLE="CAT/GROUP/N,..." env var: explicitly
    // activate `default="off"` rules for probe testing (task-3)
    if let Ok(enable) = std::env::var("NLPRULE_ENABLE") {
        for part in enable.split(',').filter(|x| !x.is_empty()) {
            let selector: nlprule::rule::id::Selector =
                core::convert::TryInto::try_into(part).unwrap();
            let n = rules.set_enabled_matching(&selector, true);
            eprintln!("enabled {n} rules for selector {part}");
        }
    }

    // optional NLPRULE_PICKY=1: activate `tags="picky"` rules (LT
    // level=picky). The HTTP front-end spawns one picky and one standard
    // worker per language and routes by the request's `level` parameter.
    if std::env::var("NLPRULE_PICKY").map_or(false, |v| v == "1") {
        let n = rules.set_picky_enabled(true);
        eprintln!("picky mode: enabled {n} rules");
    }

    let stdin = io::stdin();
    let stdout = io::stdout();
    let mut out = io::BufWriter::new(stdout.lock());
    // optional NLPRULE_FULL_ID=1: emit the full nlprule source id
    // (CATEGORY/GROUP/N) instead of the LT-facing group id, so the HTTP
    // front-end can recover the rule category for LT-style responses
    let full_id = std::env::var("NLPRULE_FULL_ID").map_or(false, |v| v == "1");

    for (i, line) in stdin.lock().lines().enumerate() {
        let text = line.unwrap();
        // (full_id, byte_start, byte_end, [replacement, ...]) — all
        // replacement alternatives so the HTTP front-end can emit one LT
        // replacement object per alternative
        let mut hits: Vec<(String, usize, usize, Vec<String>)> = Vec::new();
        for suggestion in rules.suggest(&text, &tokenizer) {
            // nlprule id format: CATEGORY/RULE_OR_GROUP/N -> LT id: RULE_OR_GROUP
            let id = if full_id {
                suggestion.source().to_string()
            } else {
                suggestion
                    .source()
                    .split('/')
                    .nth(1)
                    .unwrap_or(suggestion.source())
                    .to_string()
            };
            hits.push((
                id,
                suggestion.span().byte().start,
                suggestion.span().byte().end,
                suggestion.replacements().to_vec(),
            ));
        }
        serde_json::ser::to_writer(
            &mut out,
            &serde_json::json!({ "i": i, "text": text, "hits": hits }),
        )
        .unwrap();
        writeln!(out).unwrap();
        // flush per line: long-lived consumers (the HTTP front-end keeps the
        // process open) must not wait for the 8 KB BufWriter to fill up
        out.flush().unwrap();
    }
}
