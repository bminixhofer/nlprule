use nlprule::lang::{de, en, es};

use clap::Clap;

#[derive(Clap)]
#[clap(
    version = "1.0",
    author = "Benjamin Minixhofer <bminixhofer@gmail.com>"
)]
struct Opts {
    #[clap(long, short)]
    lang: String,
}

fn main() -> Result<(), nlprule::Error> {
    env_logger::init();
    let opts = Opts::parse();

    match opts.lang.as_ref() {
        "de" => de::correcter().test()?,
        "en" => en::correcter().test()?,
        "es" => es::correcter().test()?,
        x => panic!("language code '{}' does not exist!", x),
    }

    Ok(())
}
