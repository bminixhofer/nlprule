use std::sync::Arc;

use clap::Clap;
use nlprule::{rules::Rules, tokenizer::Tokenizer};

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

    let tokenizer = Arc::new(Tokenizer::new(opts.tokenizer).unwrap());
    let rules = Rules::new(opts.rules, tokenizer.clone()).unwrap();

    let tokens = tokenizer.pipe(&opts.text);

    println!("Tokens: {:#?}", tokens);
    println!("Suggestions: {:#?}", rules.suggest(&opts.text));
}
