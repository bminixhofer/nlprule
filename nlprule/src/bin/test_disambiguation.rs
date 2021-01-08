use clap::Clap;
use nlprule::tokenizer::Tokenizer;

#[derive(Clap)]
#[clap(
    version = "1.0",
    author = "Benjamin Minixhofer <bminixhofer@gmail.com>"
)]
struct Opts {
    #[clap(long)]
    stop_at_error: bool,
    #[clap(long, short)]
    tokenizer: String,
}

fn main() {
    env_logger::init();
    let opts = Opts::parse();

    let tokenizer = Tokenizer::new(opts.tokenizer).unwrap();
    let rules = tokenizer.rules();

    println!("Last ID: {}", rules[rules.len() - 1].id());
    println!("Runnable rules: {}", rules.len());

    let mut passes = 0;

    for rule in rules {
        if rule.test(&tokenizer) {
            passes += 1;
        } else if opts.stop_at_error {
            break;
        }
    }

    println!("Rules passing tests: {}", passes);
    if passes == rules.len() {
        std::process::exit(0);
    } else {
        std::process::exit(1);
    }
}
