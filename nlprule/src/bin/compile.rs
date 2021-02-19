use clap::Clap;
use nlprule::compile::{compile, BuildOptions, Error};
use std::io::BufWriter;
use fs_err as fs;

fn main() -> Result<(), Error> {
    env_logger::init();
    let opts = BuildOptions::parse();

    let tokenizer_sink = BufWriter::new(fs::File::create(&opts.tokenizer_out)?);
    let rules_sink = BufWriter::new(fs::File::create(&opts.rules_out)?);

    compile(opts.build_dir, rules_sink, tokenizer_sink)
}
