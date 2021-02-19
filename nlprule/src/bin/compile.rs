use clap::Clap;
use nlprule::compile::{compile, BuildOptions, Error};
use std::io::BufWriter;
use fs_err as fs;

#[derive(clap::Clap)]
#[clap(
    version = env!("CARGO_PKG_VERSION"),
    author = "Benjamin Minixhofer <bminixhofer@gmail.com>"
)]
pub struct BuildOptions {
    #[clap(long, parse(from_os_str))]
    pub build_dir: PathBuf,
    #[clap(long, parse(from_os_str))]
    pub tokenizer_out: PathBuf,
    #[clap(long, parse(from_os_str))]
    pub rules_out: PathBuf,
}

fn main() -> Result<(), Error> {
    env_logger::init();
    let opts = BuildOptions::parse();

    let tokenizer_sink = BufWriter::new(fs::File::create(&opts.tokenizer_out)?);
    let rules_sink = BufWriter::new(fs::File::create(&opts.rules_out)?);

    compile(opts.build_dir, rules_sink, tokenizer_sink)
}
