use clap::Clap;
use nlprule::compile::{compile, BuildOptions, CompileError};

fn main() -> Result<(), CompileError> {
    env_logger::init();
    let opts = BuildOptions::parse();

    compile(&opts)
}
