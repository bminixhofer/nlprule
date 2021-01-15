use clap::Clap;
use nlprule::compile::{compile, BuildOptions};

fn main() {
    env_logger::init();
    let opts = BuildOptions::parse();

    compile(&opts);
}
