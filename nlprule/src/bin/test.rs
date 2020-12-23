use clap::Clap;
use nlprule::{rule::Rules, tokenizer::Tokenizer};
use std::{fs::File, io::BufReader};

#[derive(Clap)]
#[clap(
    version = "1.0",
    author = "Benjamin Minixhofer <bminixhofer@gmail.com>"
)]
struct Opts {
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
    let rules_container: Rules = bincode::deserialize_from(reader).unwrap();
    let rules = rules_container.rules();

    println!("Runnable rules: {}", rules.len());

    println!(
        "Rules passing tests: {}",
        rules
            .iter()
            .fold(0, |count, rule| count + rule.test(&tokenizer) as usize)
    );
}
