use clap::Clap;
use nlprule::{
    rules::Rules,
    tokenizer::{finalize, Tokenizer},
};

#[derive(Clap)]
#[clap(
    version = "1.0",
    author = "Benjamin Minixhofer <bminixhofer@gmail.com>"
)]
struct Opts {
    text: String,
    #[clap(long, short)]
    tokenizer: String,
    #[clap(long, short)]
    rules: String,
}

fn main() {
    env_logger::init();
    let opts = Opts::parse();

    let tokenizer = Tokenizer::new(opts.tokenizer).unwrap();
    let rules = Rules::new(opts.rules).unwrap();

    let incomplete_tokens = tokenizer.disambiguate(tokenizer.tokenize(&opts.text));

    println!("Tokens: {:#?}", incomplete_tokens);
    println!(
        "Suggestions: {:#?}",
        rules.apply(&finalize(incomplete_tokens), &tokenizer)
    );
}
