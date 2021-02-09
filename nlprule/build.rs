//! Prepares binaries for inlining at compile time. Some components can fail (e. g. if there is no internet connection) so this is gated behind a feature flag.
//! Follows a simple procedure:
//! For all languages:
//! 1. If distributed binaries are found for the current version, download them to ~/.cache/nlprule and copy them to the OUT_DIR
//! 2. If no distributed binaries are found (i. e. a dev build is used), download the latest build directories to ~/.cache/nlprule, build the binaries and copy them to OUT_DIR
//!     - This might fail if an old dev build which is incompatible with the latest build dir is used. In that case, a readable error message is shown.
//! The following environment vars can be set:
//!     - NLPRULE_BINARY_LANGUAGES: a comma separated list of language codes for which to acquire binaries. If not set, all supported languages are used.

use nlprule_core::compile::{self, utils::supported_language_codes};
use std::{fs, io::Read, path::Path};

/// Stores the binaries for the given language in out_dir.
/// Tries downloading the binaries from their distribution source.
/// If they are not found (i. e. a dev version of nlprule is used) downloads + caches the build directory and builds the binaries.
fn acquire_binary<P1: AsRef<Path>, P2: AsRef<Path>>(
    lang_code: &str,
    out_dir: P1,
    cache_dir: P2,
    verbose: bool,
) {
    let tokenizer_out = out_dir
        .as_ref()
        .join(format!("{}_tokenizer.bin", lang_code));
    let rules_out = out_dir.as_ref().join(format!("{}_rules.bin", lang_code));

    let mut build_from_build_dir = false;

    for (binary, out) in [
        (nlprule_request::Binary::Tokenizer, &tokenizer_out),
        (nlprule_request::Binary::Rules, &rules_out),
    ]
    .iter()
    {
        let response = nlprule_request::get_binary(
            env!("CARGO_PKG_VERSION"),
            lang_code,
            *binary,
            Some(cache_dir.as_ref()),
        );

        (match response {
            Ok(mut reader) => {
                let mut bytes = Vec::new();
                reader
                    .read_to_end(&mut bytes)
                    .expect("reading binary bytes failed");

                fs::write(out, bytes).expect("writing binary to file failed");

                Ok(())
            }
            Err(nlprule_request::Error::RequestError(error)) => {
                if let Some(404) = error.status().map(|x| x.as_u16()) {
                    // binaries for this version are not distributed, fall back to building them
                    build_from_build_dir = true;
                    break;
                }

                Err(error.into())
            }
            Err(x) => Err(x),
        })
        .expect("error loading binary")
    }

    if build_from_build_dir {
        if verbose {
            println!(
                "cargo:warning=Building NLPRule binaries from build directory. This is expected if you are using a development version of NLPRule. This is NOT expected if you are using a release.",
            );
        }

        let build_dir_path = cache_dir.as_ref().join("build_dirs").join(lang_code);
        if !build_dir_path.exists() {
            nlprule_request::get_build_dir(lang_code, &build_dir_path)
                .expect("error loading build directory");
        }

        compile::compile(&compile::BuildOptions {
            build_dir: build_dir_path,
            rules_out,
            tokenizer_out,
        })
        .expect("Compiling from build directory failed. Please upgrade to a more recent development version of NLPRule.")
    }
}

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    let out_dir = std::env::var("OUT_DIR").expect("OUT_DIR must be set when build.rs is run");
    let out_dir = Path::new(&out_dir);

    let project_dir = directories::ProjectDirs::from("", "", "nlprule");
    // this should be CARGO_ARTIFACT_DIR once it is merged: https://github.com/rust-lang/rfcs/pull/3035
    // if the project dir is not set, we can fall back to OUT_DIR but the binaries are frequently redownloaded in that case
    let cache_dir = project_dir.as_ref().map_or(out_dir, |x| x.cache_dir());

    let language_codes: Vec<String> =
        if let Ok(languages) = std::env::var("NLPRULE_BINARY_LANGUAGES") {
            languages.split(',').map(|x| x.trim().to_string()).collect()
        } else {
            supported_language_codes()
                .iter()
                .map(|x| x.to_string())
                .collect()
        };

    for (i, lang_code) in language_codes.iter().enumerate() {
        acquire_binary(lang_code, &out_dir, &cache_dir, i == 0);
    }
}
