use clap::Parser;
use nlprule::{rules::Rules, tokenizer::Tokenizer};

#[derive(Parser)]
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
    /// Language code (enables language-specific tokenizer modes, e.g. Breton apostrophes)
    #[clap(long)]
    lang: Option<String>,
}

fn main() {
    env_logger::init();
    let opts = Opts::parse();

    let mut tokenizer = Tokenizer::new(opts.tokenizer).unwrap();
    if opts.lang.as_deref() == Some("br") {
        tokenizer.set_breton_apostrophes(true);
    }
    let rules_container = Rules::new(opts.rules).unwrap();
    let rules = rules_container.rules();

    println!("Runnable rules: {}", rules.len());

    let mut passes = 0;
    for rule in rules {
        if opts.ids.is_empty() || opts.ids.contains(&rule.id().to_string()) {
            passes += rule.test_with_synth_and_data(&tokenizer, rules_container.synthesizer().map(|x| &**x), Some(rules_container.filter_data())) as usize;
        }
    }

    println!("Rules passing tests: {}", passes);
    if passes == rules.len() {
        std::process::exit(0);
    } else {
        std::process::exit(1);
    }
}
