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

    let stdin = io::stdin();
    let stdout = io::stdout();
    let mut out = io::BufWriter::new(stdout.lock());

    for (i, line) in stdin.lock().lines().enumerate() {
        let text = line.unwrap();
        let mut hits: Vec<(String, usize, usize, String)> = Vec::new();
        for suggestion in rules.suggest(&text, &tokenizer) {
            // nlprule id format: CATEGORY/RULE_OR_GROUP/N -> LT id: RULE_OR_GROUP
            let id = suggestion
                .source()
                .split('/')
                .nth(1)
                .unwrap_or(suggestion.source())
                .to_string();
            hits.push((
                id,
                suggestion.span().byte().start,
                suggestion.span().byte().end,
                suggestion.replacements().first().cloned().unwrap_or_default(),
            ));
        }
        serde_json::ser::to_writer(
            &mut out,
            &serde_json::json!({ "i": i, "text": text, "hits": hits }),
        )
        .unwrap();
        writeln!(out).unwrap();
    }
}
