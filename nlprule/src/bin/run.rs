use clap::Clap;
use nlprule::tokenizer::Tokenizer;
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
}

fn main() {
    env_logger::init();
    let opts = Opts::parse();

    let reader = BufReader::new(File::open(opts.tokenizer).unwrap());
    let tokenizer: Tokenizer = bincode::deserialize_from(reader).unwrap();

    println!(
        "{:#?}",
        tokenizer.disambiguate(tokenizer.tokenize(&opts.text))
    );
}
