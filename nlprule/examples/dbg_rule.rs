//! Debug helper: print all rules whose id contains the given substring,
//! with their enabled state, then apply them to a text.

use nlprule::{Rules, Tokenizer};

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let tokenizer = Tokenizer::new(&args[1]).unwrap();
    let rules = Rules::with_lang(&args[2], &args[3]).unwrap();
    let needle = &args[4];
    let text = &args[5];

    for rule in rules.rules() {
        if rule.id().to_string().contains(needle.as_str()) {
            println!(
                "rule {} enabled={}",
                rule.id(),
                rule.enabled()
            );
        }
    }

    for sentence in tokenizer.pipe(text) {
        for s in rules.apply(&sentence) {
            println!(
                "{}\t{}\t{:?}",
                s.source(),
                &text[s.span().byte().start..s.span().byte().end],
                s.replacements()
            );
        }
    }
}
