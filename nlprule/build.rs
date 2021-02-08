use std::{fs, io::Read, path::Path};

#[cfg(windows)]
const HOST_FAMILY: &str = "windows";

#[cfg(unix)]
const HOST_FAMILY: &str = "unix";

fn main() {
    // see https://github.com/rust-lang/rust/issues/75075#issuecomment-671370162
    #[cfg(any(windows, unix))]
    {
        println!("cargo:rust-cfg=host_family={}", HOST_FAMILY);
    }

    let out_dir = std::env::var("OUT_DIR").expect("OUT_DIR must be set when build.rs is run");
    let out_dir = Path::new(&out_dir);

    let project_dir = directories::ProjectDirs::from("", "", "nlprule");
    // this should be CARGO_ARTIFACT_DIR once it is merged: https://github.com/rust-lang/rfcs/pull/3035
    // if the project dir is not set, we can fall back to OUT_DIR but the binaries are frequently redownloaded in that case
    let cache_dir = project_dir.as_ref().map_or(out_dir, |x| x.cache_dir());

    let mut build_from_build_dir = false;

    for lang_code in &["en", "de"] {
        for (binary, filename) in [
            (
                nlprule_request::Binary::Tokenizer,
                format!("{}_tokenizer.bin", lang_code),
            ),
            (
                nlprule_request::Binary::Rules,
                format!("{}_rules.bin", lang_code),
            ),
        ]
        .iter()
        {
            let response = nlprule_request::get_binary(
                env!("CARGO_PKG_VERSION"),
                lang_code,
                *binary,
                Some(cache_dir),
            );

            (match response {
                Ok(mut reader) => {
                    let mut bytes = Vec::new();
                    reader
                        .read_to_end(&mut bytes)
                        .expect("reading binary bytes failed");

                    fs::write(out_dir.join(filename), bytes)
                        .expect("writing binary to file failed");

                    Ok(())
                }
                Err(nlprule_request::Error::RequestError(error)) => {
                    if let Some(404) = error.status().map(|x| x.as_u16()) {
                        build_from_build_dir = true;
                        break;
                    }

                    Err(error.into())
                }
                Err(x) => Err(x),
            })
            .expect("error loading binary")
        }
    }

    if build_from_build_dir {
        println!(
            "cargo:warning=Building NLPRule binaries from build directory. This is expected if you are using a development version of NLPRule. This is NOT expected if you are using a release.",
        );
    }

    println!("cargo:warning={}", out_dir.display());
}
