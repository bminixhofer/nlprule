use clap::Clap;
use nlprule::compile::{compile, BuildOptions, Error};

fn main() -> Result<(), Error> {
    env_logger::init();
    let opts = BuildOptions::parse();

    compile(&opts)
}
