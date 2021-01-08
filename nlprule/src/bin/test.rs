use clap::Clap;
use nlprule::{rules::Rules, tokenizer::Tokenizer};

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
    #[clap(long, short)]
    ids: Vec<String>,
}

fn main() {
    env_logger::init();
    let opts = Opts::parse();

    let tokenizer = Tokenizer::new(opts.tokenizer).unwrap();
    let rules_container = Rules::new(opts.rules).unwrap();
    let rules = rules_container.rules();

    println!("Runnable rules: {}", rules.len());

    let mut passes = 0;
    for rule in rules {
        if opts.ids.is_empty() || opts.ids.contains(&rule.id().to_string()) {
            passes += rule.test(&tokenizer) as usize;
        }
    }

    println!("Rules passing tests: {}", passes);
    if passes == rules.len() {
        std::process::exit(0);
    } else {
        std::process::exit(1);
    }
}
