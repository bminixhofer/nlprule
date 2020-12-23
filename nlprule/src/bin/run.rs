use clap::Clap;
use nlprule::tokenizer::{chunk::Chunker, tag::Tagger, Tokenizer, TokenizerOptions};
use std::sync::Arc;

#[derive(Clap)]
#[clap(
    version = "1.0",
    author = "Benjamin Minixhofer <bminixhofer@gmail.com>"
)]
struct Opts {
    text: String,
}

fn main() {
    env_logger::init();
    let opts = Opts::parse();

    let tagger = Tagger::from_dumps(
        &[
            &format!(
                "data/dumps/{}/output.dump",
                std::env::var("RULE_LANG").unwrap()
            ),
            &format!(
                "data/dumps/{}/added.txt",
                std::env::var("RULE_LANG").unwrap()
            ),
        ],
        &[&format!(
            "data/dumps/{}/removed.txt",
            std::env::var("RULE_LANG").unwrap()
        )],
    )
    .unwrap();

    let tokenizer = Tokenizer::from_xml(
        format!(
            "data/disambiguation.{}.canonic.xml",
            std::env::var("RULE_LANG").unwrap()
        ),
        Arc::new(tagger),
        if std::env::var("RULE_LANG").unwrap() == "en" {
            Some(Chunker::new())
        } else {
            None
        },
        TokenizerOptions {
            allow_errors: true,
            ..TokenizerOptions::default()
        },
    )
    .unwrap();

    println!(
        "{:#?}",
        tokenizer.disambiguate(tokenizer.tokenize(&opts.text))
    );
}
