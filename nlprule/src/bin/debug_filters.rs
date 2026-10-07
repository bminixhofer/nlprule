use nlprule::{Rules, Tokenizer};

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let tokenizer = Tokenizer::new(&args[1]).unwrap();
    let rules = Rules::new(&args[2]).unwrap();
    let text = &args[3];
    let sentence = tokenizer.pipe(text).next().expect("at least one sentence");
    for suggestion in rules.apply(&sentence) {
        println!(
            "{}\t{}\t{:?}\t{:?}",
            suggestion.source(),
            &text[suggestion.span().byte().start..suggestion.span().byte().end],
            suggestion.replacements(),
            suggestion.message()
        );
    }
}
