use clap::Clap;
use nlprule::{
    rule::{Rules, RulesOptions},
    tokenizer::{chunk::Chunker, tag::Tagger, Tokenizer, TokenizerOptions},
};
use std::sync::Arc;

#[derive(Clap)]
#[clap(
    version = "1.0",
    author = "Benjamin Minixhofer <bminixhofer@gmail.com>"
)]
struct Opts {
    ids: Vec<String>,
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
        TokenizerOptions::default(),
    )
    .unwrap();
    let rules_container = Rules::from_xml(
        format!(
            "data/grammar.{}.canonic.xml",
            std::env::var("RULE_LANG").unwrap()
        ),
        Arc::new(tokenizer),
        RulesOptions {
            allow_errors: true,
            ids: opts.ids,
        },
    );
    let rules = rules_container.rules();
    println!("Runnable rules: {}", rules.len());

    println!(
        "Rules passing tests: {}",
        rules.iter().fold(0, |count, rule| count
            + rule.test(rules_container.tokenizer()) as usize)
    );
}
