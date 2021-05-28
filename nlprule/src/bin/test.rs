use nlprule::{
    components::{
        chunker::Chunker,
        multiword_tagger::MultiwordTagger,
        rules::{Disambiguator, Rules},
        tokenizer::Tokenizer,
        Component,
    },
    properties::{tokenize, CreatePipe, Pipeline, Tokenize},
};

// use clap::Clap;
// use nlprule::{rules::Rules, tokenizer::Tokenizer};

// #[derive(Clap)]
// #[clap(
//     version = "1.0",
//     author = "Benjamin Minixhofer <bminixhofer@gmail.com>"
// )]
// struct Opts {
//     #[clap(long, short)]
//     tokenizer: String,
//     #[clap(long, short)]
//     rules: String,
//     #[clap(long, short)]
//     ids: Vec<String>,
// }

// fn main() {
//     env_logger::init();
//     let opts = Opts::parse();

//     let tokenizer = Tokenizer::new(opts.tokenizer).unwrap();
//     let rules_container = Rules::new(opts.rules).unwrap();
//     let rules = rules_container.rules();

//     println!("Runnable rules: {}", rules.len());

//     let mut passes = 0;
//     for rule in rules {
//         if opts.ids.is_empty() || opts.ids.contains(&rule.id().to_string()) {
//             if let Ok(true) = rule.test(&tokenizer) {
//                 passes += 1;
//             }
//         }
//     }

//     println!("Rules passing tests: {}", passes);
//     if passes == rules.len() {
//         std::process::exit(0);
//     } else {
//         std::process::exit(1);
//     }
// }

fn main() -> Result<(), nlprule::Error> {
    env_logger::init();

    // let tokenizer = Pipeline::new((
    //     Tokenizer::new("new_storage/en/tokenizer.bin")?,
    //     MultiwordTagger::new("new_storage/en/multiword_tagger.bin")?,
    //     Chunker::new("new_storage/en/chunker.bin")?,
    //     Disambiguator::new("new_storage/en/disambiguator.bin")?,
    //     Rules::new("new_storage/en/rules.bin")?,
    // ))?;

    let tokenizer = Pipeline::new((
        Tokenizer::new("new_storage/de/tokenizer.bin")?,
        // MultiwordTagger::new("new_storage/de/multiword_tagger.bin")?,
        // Chunker::new("new_storage/en/chunker.bin")?,
        Disambiguator::new("new_storage/de/disambiguator.bin")?,
        Rules::new("new_storage/de/rules.bin")?,
    ))?;

    tokenizer.test()?;

    Ok(())
}
