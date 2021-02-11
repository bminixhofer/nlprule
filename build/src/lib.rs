//! A helper crate for downloading NLPRule resources from their distribution source.

use flate2::read::GzDecoder;
use nlprule::compile;
use std::{
    fs,
    io::{self, Cursor, Read},
    path::{Path, PathBuf},
};
use thiserror::Error;
use zip::result::ZipError;

#[derive(Debug, Error)]
pub enum Error {
    #[error("request error: {0}")]
    RequestError(#[from] reqwest::Error),
    #[error("i/o error: {0}")]
    IOError(#[from] io::Error),
    #[error("zip error: {0}")]
    ZipError(#[from] ZipError),
}

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub enum Binary {
    Tokenizer,
    Rules,
}

impl Binary {
    fn filename(&self, lang_code: &str) -> String {
        match &self {
            Binary::Tokenizer => format!("{}_tokenizer.bin", lang_code),
            Binary::Rules => format!("{}_rules.bin", lang_code),
        }
    }
}

pub fn get_binary<P: AsRef<Path>>(
    version: &str,
    lang_code: &str,
    binary: Binary,
    cache_dir: Option<P>,
) -> Result<impl Read, Error> {
    let filename = binary.filename(lang_code);

    let mut cache_path: Option<PathBuf> = None;

    if let Some(dir) = cache_dir {
        cache_path = Some(dir.as_ref().join(version).join(lang_code).join(&filename));
    }

    // if the file can be read, the data is already cached
    if let Some(path) = &cache_path {
        if let Ok(bytes) = fs::read(path) {
            return Ok(Cursor::new(bytes));
        }
    }

    // ... otherwise, request the data from the URL ...
    let bytes = reqwest::blocking::get(&format!(
        "https://github.com/bminixhofer/nlprule/releases/download/{}/{}.gz",
        version, filename
    ))?
    .error_for_status()?
    .bytes()?;

    let mut gz = GzDecoder::new(&bytes[..]);
    let mut buffer = Vec::new();
    gz.read_to_end(&mut buffer)?;

    // ... and then cache the data at the provided file, if one was found
    if let Some(path) = &cache_path {
        fs::create_dir_all(path.parent().expect("path must have parent"))?;
        fs::write(path, &buffer)?;
    }

    Ok(Cursor::new(buffer))
}

pub fn get_build_dir<P: AsRef<Path>>(lang_code: &str, out_dir: P) -> Result<(), Error> {
    let bytes = reqwest::blocking::get(&format!(
        "https://f000.backblazeb2.com/file/nlprule/{}.zip",
        lang_code
    ))?
    .error_for_status()?
    .bytes()?;

    // extract the zip file and write to directory, a bit annoying that this is so verbose
    // adapted from https://github.com/zip-rs/zip/blob/master/examples/extract.rs
    let mut archive = zip::ZipArchive::new(Cursor::new(bytes))?;

    for i in 0..archive.len() {
        let mut file = archive.by_index(i).unwrap();
        let outpath = match file.enclosed_name() {
            Some(path) => out_dir
                .as_ref()
                // the first component of the path is the zip file name e. g. "en" so we skip it
                .join(path.iter().skip(1).collect::<PathBuf>()),
            None => continue,
        };

        if (&*file.name()).ends_with('/') {
            fs::create_dir_all(&outpath).unwrap();
        } else {
            if let Some(p) = outpath.parent() {
                if !p.exists() {
                    fs::create_dir_all(&p).unwrap();
                }
            }
            let mut outfile = fs::File::create(&outpath).unwrap();
            io::copy(&mut file, &mut outfile).unwrap();
        }

        // Get and Set permissions
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;

            if let Some(mode) = file.unix_mode() {
                fs::set_permissions(&outpath, fs::Permissions::from_mode(mode)).unwrap();
            }
        }
    }

    Ok(())
}

/// Stores the binaries for the given language in out_dir.
/// Tries downloading the binaries from their distribution source.
/// If they are not found (i. e. a dev version of nlprule is used) downloads + caches the build directory and builds the binaries.

/// Gets the language codes for the currently supported languages in ISO 639-1 (two-letter) format e. g. "en".
pub fn supported_language_codes() -> Vec<&'static str> {
    include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/", "languages.txt"))
        .lines()
        .collect()
}

#[derive(Debug)]
pub struct BinaryBuilder {
    language_codes: Vec<String>,
    out_dir: PathBuf,
    cache_dir: Option<PathBuf>,
    fallback_to_build_dir: bool,
    build_dir: Option<PathBuf>,
}

impl BinaryBuilder {
    fn build_language(&self, lang_code: &str) {
        let tokenizer_out = self.out_dir.join(format!("{}_tokenizer.bin", lang_code));
        let rules_out = self.out_dir.join(format!("{}_rules.bin", lang_code));

        let mut did_not_find_binaries = false;

        for (binary, out) in [
            (Binary::Tokenizer, &tokenizer_out),
            (Binary::Rules, &rules_out),
        ]
        .iter()
        {
            let response = get_binary(
                env!("CARGO_PKG_VERSION"),
                lang_code,
                *binary,
                self.cache_dir.as_ref(),
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
                Err(Error::RequestError(error)) => {
                    if let Some(404) = error.status().map(|x| x.as_u16()) {
                        // binaries for this version are not distributed, fall back to building them
                        did_not_find_binaries = true;
                        break;
                    }

                    Err(error.into())
                }
                Err(x) => Err(x),
            })
            .expect("error loading binary")
        }

        if did_not_find_binaries && self.fallback_to_build_dir {
            // it is possible that the build dirs are cached too long i. e. not downloaded again although a new version is available
            // this could lead to problems but is not easy to fix so it will stay this way unless problems are reported
            let build_dir = self
                .build_dir.as_ref()
                .unwrap_or_else(
                    || self.cache_dir.as_ref().expect("need somewhere to store build dirs: either `cache_dir` or `build_dir_path` must be set if `fallback_to_build_dir` is true."),
                )
                .join(lang_code);

            if !build_dir.exists() {
                get_build_dir(lang_code, &build_dir).expect("error loading build directory");
            }

            compile::compile(&compile::BuildOptions {
                build_dir,
                rules_out,
                tokenizer_out,
            })
            .expect("Compiling from build directory failed. Upgrading to a more recent development version of NLPRule might fix this problem.");
        } else if did_not_find_binaries {
            panic!(
                "Did not find binaries for version {}. \
                 If this is a development version, try setting `fallback_to_build_dir` to build the binaries yourself. \
                 If this is a release, this should NOT happen.",
                env!("CARGO_PKG_VERSION")
            );
        }
    }

    pub fn new<P: AsRef<Path>>(language_codes: Option<&[&str]>, out_dir: P) -> Self {
        let language_codes: Vec<_> = language_codes.map_or_else(
            || {
                supported_language_codes()
                    .into_iter()
                    .map(|x| x.to_owned())
                    .collect()
            },
            |x| x.iter().map(|x| x.to_string()).collect(),
        );

        let project_dir = directories::ProjectDirs::from("", "", "nlprule");
        // this should be CARGO_ARTIFACT_DIR once it is merged: https://github.com/rust-lang/rfcs/pull/3035
        let cache_dir = project_dir.as_ref().map(|x| x.cache_dir().to_owned());
        let build_dir = cache_dir.as_ref().map(|x| x.join("build_dirs"));

        BinaryBuilder {
            language_codes,
            out_dir: out_dir.as_ref().to_owned(),
            cache_dir,
            fallback_to_build_dir: false,
            build_dir,
        }
    }

    pub fn out_dir(mut self, out_dir: PathBuf) -> Self {
        self.out_dir = out_dir;
        self
    }

    pub fn cache_dir(mut self, cache_dir: Option<PathBuf>) -> Self {
        self.cache_dir = cache_dir;
        self
    }

    pub fn fallback_to_build_dir(mut self, fallback_to_build_dir: bool) -> Self {
        self.fallback_to_build_dir = fallback_to_build_dir;
        self
    }

    pub fn build_dir(mut self, build_dir: Option<PathBuf>) -> Self {
        self.build_dir = build_dir;
        self
    }

    pub fn build(&self) {
        for lang_code in &self.language_codes {
            self.build_language(lang_code);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn getting_binary_works() -> Result<(), Error> {
        // this is nice to keep roughly in sync with the latest released version but it is not necessary
        get_binary::<String>("0.3.0", "en", Binary::Rules, None)?;

        Ok(())
    }

    #[test]
    fn getting_build_dir_works() -> Result<(), Error> {
        let tempdir = tempdir::TempDir::new("build_dir_test")?;
        let tempdir = tempdir.path();

        get_build_dir("en", &tempdir)?;

        assert_eq!(fs::read_to_string(tempdir.join("lang_code.txt"))?, "en");

        Ok(())
    }

    #[test]
    fn binary_builder_works() -> Result<(), Error> {
        let tempdir = tempdir::TempDir::new("builder_test")?;
        let tempdir = tempdir.path();

        BinaryBuilder::new(None, tempdir)
            .fallback_to_build_dir(true)
            .build();

        Ok(())
    }
}
