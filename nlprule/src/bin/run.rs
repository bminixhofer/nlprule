use clap::Clap;
use nlprule_core::{
    rule::Rules,
    tokenizer::{finalize, Tokenizer},
};
use std::{fs::File, io::BufReader};

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

    let reader = BufReader::new(File::open(opts.tokenizer).unwrap());
    let tokenizer: Tokenizer = bincode::deserialize_from(reader).unwrap();

    let reader = BufReader::new(File::open(opts.rules).unwrap());
    let rules: Rules = bincode::deserialize_from(reader).unwrap();

    let incomplete_tokens = tokenizer.disambiguate(tokenizer.tokenize(&opts.text));

    println!("Tokens: {:#?}", incomplete_tokens);
    println!(
        "Suggestions: {:#?}",
        rules.apply(&finalize(incomplete_tokens), &tokenizer)
    );
}
