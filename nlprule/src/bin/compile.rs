use clap::Clap;
use nlprule::compile::{compile, BuildOptions, Error};

fn main() -> Result<(), Error> {
    env_logger::init();
    let opts = BuildOptions::parse();

    let tokenizer_sink = BufWriter::new(File::create(&opts.tokenizer_out)?);
    let rules_sink = BufWriter::new(File::create(&opts.rules_out)?);

    compile(opts.build_dir, rules_sink, tokenizer_sink)
}
