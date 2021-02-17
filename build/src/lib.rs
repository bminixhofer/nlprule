//! This crate provides a builder to make it easier to use the correct binaries for [nlprule](https://github.com/bminixhofer/nlprule).
//! See `README.md` for details.

use flate2::read::GzDecoder;
use nlprule::{compile, rules_filename, tokenizer_filename};
use std::{
    fs::{self, File},
    io::{self, BufWriter, Cursor, Read},
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
    #[error("error postprocessing binaries: {0}")]
    PostprocessingError(Box<dyn std::error::Error>),
}

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub enum Binary {
    Tokenizer,
    Rules,
}

impl Binary {
    fn filename(&self, lang_code: &str) -> String {
        match &self {
            Binary::Tokenizer => tokenizer_filename(lang_code),
            Binary::Rules => rules_filename(lang_code),
        }
    }
}

/// Stores the binaries for the given language and version in out_dir.
/// Tries downloading the binaries from their distribution source.
/// Optionally caches them at some directory.
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
        let mut file = archive.by_index(i)?;
        let outpath = match file.enclosed_name() {
            Some(path) => out_dir
                .as_ref()
                // the first component of the path is the zip file name e. g. "en" so we skip it
                .join(path.iter().skip(1).collect::<PathBuf>()),
            None => continue,
        };

        if (&*file.name()).ends_with('/') {
            fs::create_dir_all(&outpath)?;
        } else {
            if let Some(p) = outpath.parent() {
                if !p.exists() {
                    fs::create_dir_all(&p)?;
                }
            }
            let mut outfile = fs::File::create(&outpath)?;
            io::copy(&mut file, &mut outfile)?;
        }

        // Get and Set permissions
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;

            if let Some(mode) = file.unix_mode() {
                fs::set_permissions(&outpath, fs::Permissions::from_mode(mode))?;
            }
        }
    }

    Ok(())
}

/// Gets the language codes for the currently supported languages in ISO 639-1 (two-letter) format e. g. "en".
pub fn supported_language_codes() -> Vec<&'static str> {
    include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/", "languages.txt"))
        .lines()
        .collect()
}

/// Places all nlprule binaries for the given languages in some directory.
#[derive(Debug)]
pub struct BinaryBuilder {
    language_codes: Vec<String>,
    out_dir: PathBuf,
    version: String,
    cache_dir: Option<PathBuf>,
    fallback_to_build_dir: bool,
    build_dir: Option<PathBuf>,
    outputs: Vec<PathBuf>,
}

impl BinaryBuilder {
    /// Acquires the rule and tokenizer binaries for one language by:
    /// - Trying to download them from their distribution source (or load them local cache).
    /// - If they are not found (i. e. a dev version of nlprule is used) and `fallback_to_build_dir`is true
    /// downloads the latest build directory  and builds the binaries from it.
    /// This can still fail if the dev version is sufficiently outdated for the latest build dir.
    /// In that case, the user can't be helped.
    fn build_language(&mut self, lang_code: &str) {
        let tokenizer_out = self.out_dir.join(tokenizer_filename(lang_code));
        let rules_out = self.out_dir.join(rules_filename(lang_code));

        let mut did_not_find_binaries = false;

        for (binary, out) in [
            (Binary::Tokenizer, &tokenizer_out),
            (Binary::Rules, &rules_out),
        ]
        .iter()
        {
            let response = get_binary(&self.version, lang_code, *binary, self.cache_dir.as_ref());

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
                rules_out: rules_out.clone(),
                tokenizer_out: tokenizer_out.clone(),
            })
            .expect("Compiling from build directory failed. Upgrading to a more recent development version of NLPRule might fix this problem.");
        } else if did_not_find_binaries {
            panic!(
                "Did not find binaries for version {}. \
                 If this is a development version, try setting `fallback_to_build_dir` to build the binaries yourself. \
                 If this is a release, this should NOT happen.",
                self.version
            );
        }

        self.outputs.push(tokenizer_out);
        self.outputs.push(rules_out);
    }

    /// Creates a new binary builder. `language_codes` must be in ISO 639-1 (two-letter) format.
    /// If `language_codes` is `None`, uses all supported languages.
    /// If this is used in a `build.rs`, `out_dir` should probably be the OUT_DIR environment variable.
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

        let version = env!("CARGO_PKG_VERSION").to_owned();

        BinaryBuilder {
            language_codes,
            out_dir: out_dir.as_ref().to_owned(),
            version,
            cache_dir,
            fallback_to_build_dir: false,
            build_dir,
            outputs: Vec::new(),
        }
    }

    /// Sets the version for which to fetch binaries.
    /// The version of `nlprule-build` (kept in sync with `nlprule` version) by default.
    /// Typically does not need to be modified.
    pub fn version<S: Into<String>>(mut self, version: S) -> Self {
        self.version = version.into();
        self
    }

    /// Sets the out directory.
    pub fn out_dir(mut self, out_dir: PathBuf) -> Self {
        self.out_dir = out_dir;
        self
    }

    /// Sets the cache directory. The user cache directory at e. g. `~/.cache/nlprule` bz default.
    pub fn cache_dir(mut self, cache_dir: Option<PathBuf>) -> Self {
        self.cache_dir = cache_dir;
        self
    }

    /// Sets whether to fallback to building from the build directory if no distributed binaries are found
    /// (i. e. a development version of nlprule is used).
    pub fn fallback_to_build_dir(mut self, fallback_to_build_dir: bool) -> Self {
        self.fallback_to_build_dir = fallback_to_build_dir;
        self
    }

    /// Sets the path the build directories should be stored at.
    /// Only relevant if `fallback_to_build_dir` is true.
    /// `cache_dir.join("build_dirs")` by default.
    pub fn build_dir(mut self, build_dir: Option<PathBuf>) -> Self {
        self.build_dir = build_dir;
        self
    }

    /// Builds by {downloading, copying, building} the binaries to the out directory.
    pub fn build(mut self) -> Self {
        for lang_code in self.language_codes[..].to_vec() {
            self.build_language(&lang_code);
        }
        self
    }

    /// Validates the binaries by checking if they can be loaded by nlprule.
    pub fn validate(self) -> Self {
        for lang_code in &self.language_codes {
            let tokenizer_out = self.out_dir.join(tokenizer_filename(lang_code));
            let rules_out = self.out_dir.join(rules_filename(lang_code));

            nlprule::Rules::new(rules_out).unwrap_or_else(|e| {
                panic!(
                    "failed to validate rules binary for {lang_code}: {error}",
                    lang_code = lang_code,
                    error = e
                )
            });
            nlprule::Tokenizer::new(tokenizer_out).unwrap_or_else(|e| {
                panic!(
                    "failed to validate tokenizer binary for {lang_code}: {error}",
                    lang_code = lang_code,
                    error = e
                )
            });
        }

        self
    }

    /// Gets the paths to all files this builder created.
    pub fn outputs(&self) -> &[PathBuf] {
        &self.outputs
    }

    /// Applies the given postprocessing function to the binaries e. g. for compression.
    /// Modifies the output path by the given path function.
    ///
    /// # Example
    ///
    /// ```no_run
    /// # use nlprule_build::BinaryBuilder;
    /// # use std::io::Write;
    /// # let tempdir = tempdir::TempDir::new("builder_test")?;
    /// # let tempdir = tempdir.path();
    /// #
    /// # let mut builder = BinaryBuilder::new(Some(&["en"]), tempdir).version("0.3.0");
    /// builder
    ///    .build()
    ///    .validate()
    ///    .postprocess(
    ///        |buffer, writer| {
    ///            Ok(
    ///                flate2::write::GzEncoder::new(writer, flate2::Compression::default())
    ///                    .write_all(&buffer)?,
    ///            )
    ///        },
    ///        |p| {
    ///            let mut path = p.as_os_str().to_os_string();
    ///            path.push(".gz");
    ///            path
    ///        },
    ///    )?;
    /// # Ok::<(), nlprule_build::Error>(())
    /// ```
    pub fn postprocess<F, C, P: AsRef<Path>>(
        mut self,
        proc_fn: C,
        path_fn: F,
    ) -> Result<Self, Error>
    where
        C: Fn(Vec<u8>, BufWriter<File>) -> Result<(), Box<dyn std::error::Error>>,
        F: Fn(PathBuf) -> P,
    {
        for (i, path) in self.outputs.to_vec().iter().enumerate() {
            let buffer = fs::read(&path)?;

            let new_path = path_fn(path.clone());
            let new_path = new_path.as_ref();

            let writer = BufWriter::new(File::create(new_path)?);

            proc_fn(buffer, writer).map_err(Error::PostprocessingError)?;

            if new_path != path {
                self.outputs[i] = new_path.to_path_buf();
                fs::remove_file(path)?;
            }
        }

        Ok(self)
    }
}

#[cfg(test)]
mod tests {
    use io::Write;

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
            .build()
            .validate();

        Ok(())
    }

    #[test]
    fn binary_builder_works_with_released_version() -> Result<(), Error> {
        let tempdir = tempdir::TempDir::new("builder_test")?;
        let tempdir = tempdir.path();

        BinaryBuilder::new(Some(&["en"]), tempdir)
            .version("0.3.0")
            .build();

        Ok(())
    }

    #[test]
    fn binary_builder_works_with_smush() -> Result<(), Error> {
        let tempdir = tempdir::TempDir::new("builder_test")?;
        let tempdir = tempdir.path();

        BinaryBuilder::new(Some(&["en"]), tempdir)
            .version("0.3.0")
            .build()
            .postprocess(
                |buffer, mut writer| {
                    Ok(writer.write_all(&smush::encode(
                        &buffer,
                        smush::Codec::Gzip,
                        smush::Quality::Default,
                    )?)?)
                },
                |p| {
                    let mut path = p.as_os_str().to_os_string();
                    path.push(".gz");
                    path
                },
            )?;

        let tokenizer_path = tempdir
            .join(Path::new(&tokenizer_filename("en")))
            .with_extension("bin.gz");
        assert!(tokenizer_path.exists());
        smush::decode(&fs::read(tokenizer_path)?, smush::Codec::Gzip).unwrap();

        Ok(())
    }

    #[test]
    fn binary_builder_works_with_flate2() -> Result<(), Error> {
        let tempdir = tempdir::TempDir::new("builder_test")?;
        let tempdir = tempdir.path();

        let builder = BinaryBuilder::new(Some(&["en"]), tempdir)
            .version("0.3.0")
            .build()
            .postprocess(
                |buffer, writer| {
                    Ok(
                        flate2::write::GzEncoder::new(writer, flate2::Compression::default())
                            .write_all(&buffer)?,
                    )
                },
                |p| {
                    let mut path = p.as_os_str().to_os_string();
                    path.push(".gz");
                    path
                },
            )?;

        assert_eq!(
            builder.outputs(),
            &[
                tempdir.join("en_tokenizer.bin.gz"),
                tempdir.join("en_rules.bin.gz")
            ]
        );

        let rules_path = tempdir
            .join(Path::new(&rules_filename("en")))
            .with_extension("bin.gz");
        assert!(rules_path.exists());

        let encoded = fs::read(rules_path)?;
        let mut decoder = flate2::read::GzDecoder::new(&encoded[..]);

        let mut decoded = Vec::new();
        decoder.read_to_end(&mut decoded).unwrap();

        Ok(())
    }
}
