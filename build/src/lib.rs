//! This crate provides a builder to make it easier to use the correct binaries for [nlprule](https://github.com/bminixhofer/nlprule).
//! See `README.md` for details.

use flate2::bufread::GzDecoder;
use fs::File;
use fs_err as fs;
use nlprule::{compile, rules_filename, tokenizer_filename};
use reqwest::blocking::Response;
use std::fs::Permissions;
use std::{
    io::{self, BufReader, BufWriter, Cursor, Read, Write, Seek, SeekFrom},
    path::{Path, PathBuf},
    result,
};
use zip::result::ZipError;

pub type OtherError = Box<dyn std::error::Error + Send + Sync + 'static>;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    RequestError(#[from] reqwest::Error),
    #[error("Binaries were not found on the remote")]
    BinariesNotFound,
    #[error("Failed to validate {1:?} binary for lang {0}")]
    ValidationFailed(String, Binary, #[source] nlprule::Error),
    #[error(transparent)]
    IOError(#[from] io::Error),
    #[error(transparent)]
    ZipError(#[from] ZipError),
    #[error("error postprocessing binaries: {0}")]
    PostprocessingError(#[source] OtherError),
    #[error("error transforming binaries: {0}")]
    TransformError(#[source] OtherError),
    #[error("Collation failed")]
    CollationFailed(#[source] nlprule::compile::Error)
}

pub type Result<T> = std::result::Result<T, Error>;

/// Definition of the data transformation for the network retrieved, binencoded rules and tokenizer datasets.
pub trait TransformDataFn:
    for<'w> Fn(
        Box<dyn Read>,
        Box<dyn Write + 'w>,
    ) -> result::Result<(), OtherError>
{
}

impl<T> TransformDataFn for T where
    T: for<'w> Fn(
        Box<dyn Read>,
        Box<dyn Write + 'w>,
    ) -> result::Result<(), OtherError>
{
}

/// Definition of the path transformation for the network retrieved, binencoded rules and tokenizer datasets.
pub trait TransformPathFn:
    Fn(PathBuf) -> result::Result<PathBuf, OtherError>
{
}

impl<T> TransformPathFn for T where
    T: Fn(PathBuf) -> result::Result<PathBuf, OtherError>
{
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
///
/// This implicitly unpacks the originally gzip'd sources and returns
/// an in-memory buffer.
fn obtain_binary_from_github_release(
    version: &str,
    lang_code: &str,
    binary: Binary,
) -> Result<Cursor<Vec<u8>>> {
    let filename = binary.filename(lang_code);

    // ... otherwise, request the data from the URL ...
    let bytes = reqwest::blocking::get(&format!(
        "https://github.com/bminixhofer/nlprule/releases/download/{}/{}.gz",
        version, filename
    ))?
    .error_for_status()
    .map_err(|e| {
        if let Some(404) = e.status().map(|x| x.as_u16()) {
            Error::BinariesNotFound
        } else {
            e.into()
        }
    })?
    .bytes()?;

    let mut gz = GzDecoder::new(&bytes[..]);
    let mut buffer = Vec::new();
    gz.read_to_end(&mut buffer)?;

    Ok(Cursor::new(buffer))
}

fn construct_cache_path(
    version: &str,
    lang_code: &str,
    binary: Binary,
    cache_dir: Option<&PathBuf>,
    transform_path_fn: Option<&dyn TransformPathFn>,
) -> Result<Option<PathBuf>> {
    let filename = binary.filename(lang_code);

    cache_dir
        .map(move |dir| {
            let path = dir.join(version).join(lang_code).join(&filename);
            Ok(if let Some(transform_path_fn) = transform_path_fn {
                transform_path_fn(path).map_err(Error::TransformError)?
            } else {
                path
            })
        })
        .transpose()
}

/// Returns a `dyn Read` type, that is either directly feed
/// from an in-memory slice or from the on disk cache.
/// If the on-disk cache is disabled or is not present,
/// it will attempt to download it via [`obtain_binary_from_github_release`].
/// The data is then written into `dest`.
/// Also updates the cache.
fn obtain_binary_cache_or_github(
    version: &str,
    lang_code: &str,
    binary: Binary,
    cache_dir: Option<&PathBuf>,
    transform_path_fn: Option<&dyn TransformPathFn>,
    transform_data_fn: Option<&dyn TransformDataFn>,
) -> Result<Box<dyn Read>> {
    let cache_path = construct_cache_path(
        version,
        lang_code,
        binary,
        cache_dir.clone(),
        transform_path_fn,
    )?;

    // if the file can be read, the data is already cached and the transform was applied before
    if let Some(ref cache_path) = cache_path {
        if let Ok(bytes) = fs::read(cache_path) {
            return Ok(Box::new(Cursor::new(bytes)));
        }
    }

    // the binencoded data from github
    let reader_binenc = obtain_binary_from_github_release(version, lang_code, binary)?;

    // apply the transform if any to an intermediate buffer
    let mut reader_transformed = if let Some(transform_data_fn) = transform_data_fn {
        // TODO this is not optimal, the additional copy is a bit annoying
        let mut intermediate = Box::new(Cursor::new(Vec::<u8>::with_capacity(4096 * 1024)));
        transform_data_fn(Box::new(reader_binenc), Box::new(&mut intermediate)).map_err(Error::TransformError)?;
        intermediate
    } else {
        Box::new(reader_binenc)
    };

    // update the cache entry
    if let Some(ref cache_path) = cache_path {
        fs::create_dir_all(cache_path.parent().expect("path must have parent"))?;
        let mut cache_file = fs::OpenOptions::new()
            .truncate(true)
            .create(true)
            .write(true)
            .open(cache_path)?;
        io::copy(&mut reader_transformed, &mut cache_file)?;
    }

    // move the cursor back to the beginning
    reader_transformed.seek(SeekFrom::Start(0_u64))?;

    Ok(reader_transformed)
}

fn assure_binary_availability(
    version: &str,
    lang_code: &str,
    binary: Binary,
    cache_dir: Option<&PathBuf>,
    transform_path_fn: Option<&dyn TransformPathFn>,
    transform_data_fn: Option<&dyn TransformDataFn>,
    out: PathBuf,
) -> Result<()> {
    let mut source = obtain_binary_cache_or_github(
        version,
        lang_code,
        binary,
        cache_dir,
        transform_path_fn,
        transform_data_fn,
    )?;

    let mut out_file = fs::OpenOptions::new()
        .truncate(true)
        .create(true)
        .write(true)
        .open(out)?;
    io::copy(&mut source, &mut out_file)?;
    Ok(())
}

pub fn get_build_dir<P: AsRef<Path>>(lang_code: &str, out_dir: P) -> Result<()> {
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
                fs::set_permissions(&outpath, Permissions::from_mode(mode))?;
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
pub struct BinaryBuilder {
    language_codes: Vec<String>,
    out_dir: PathBuf,
    version: String,
    cache_dir: Option<PathBuf>,
    fallback_to_build_dir: bool,
    build_dir: Option<PathBuf>,
    outputs: Vec<PathBuf>,
    transform_path_fn: Option<Box<dyn TransformPathFn>>,
    transform_data_fn: Option<Box<dyn TransformDataFn>>,
}

impl BinaryBuilder {
    /// ```plain
    /// github release resource --[fn transform]--> $cache_dir --[fn postprocess]--> $OUT_DIR/
    /// ```
    ///
    /// ```plain
    /// github release resource --[fn transform]--> $OUT_DIR --[fn postprocess]--> $OUT_DIR/
    /// ```
    ///
    /// Acquires the rule and tokenizer binaries for one language by:
    /// - Trying to download them from their distribution source (or load them local cache).
    /// - If they are not found (i. e. a dev version of nlprule is used) and `fallback_to_build_dir`is true
    /// downloads the latest build directory  and builds the binaries from it.
    /// This can still fail if the dev version is sufficiently outdated for the latest build dir.
    /// In that case, the user is encouraged to update to a release or a newer git sha.
    fn build_language(&mut self, lang_code: &str) -> Result<()> {
        // adjust the destination path for now
        let path_transform = |out: PathBuf| -> Result<PathBuf> {
            Ok(if let Some(ref transform_path_fn) = self.transform_path_fn {
                transform_path_fn(out).map_err(Error::TransformError)?
            } else {
                out
            })
        };

        let tokenizer_out = path_transform(self.out_dir.join(tokenizer_filename(lang_code)))?;
        let rules_out = path_transform(self.out_dir.join(rules_filename(lang_code)))?;

        let mut did_not_find_binaries = false;

        for (binary, out) in &[
            (Binary::Tokenizer, &tokenizer_out),
            (Binary::Rules, &rules_out),
        ] {
            let out = out.to_owned().to_owned();
            if let Err(e) = assure_binary_availability(
                &self.version,
                lang_code,
                *binary,
                self.cache_dir.as_ref(),
                self.transform_path_fn.as_ref().map(|x| x.as_ref()),
                self.transform_data_fn.as_ref().map(|x| x.as_ref()),
                out,
            ) {
                if let Error::BinariesNotFound = e {
                    did_not_find_binaries = true;
                    break;
                }
            }
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


            let mut rules_sink = BufWriter::new(fs::OpenOptions::new().truncate(true).create(true).write(true).open(&rules_out)?);
            let mut tokenizer_sink = BufWriter::new(fs::OpenOptions::new().truncate(true).create(true).write(true).open(&tokenizer_out)?);
            if let Some(ref transform_data_fn) = self.transform_data_fn {
                let mut transfer_buffer_rules = Cursor::new(Vec::new());
                let mut transfer_buffer_tokenizer = Cursor::new(Vec::new());

                compile::compile(
                    build_dir,
                    &mut transfer_buffer_rules,
                    &mut transfer_buffer_tokenizer,
                ).map_err(Error::CollationFailed)?;

                transform_data_fn(Box::new(transfer_buffer_rules), Box::new(rules_sink)).map_err(Error::TransformError)?;
                transform_data_fn(Box::new(transfer_buffer_tokenizer), Box::new(tokenizer_sink)).map_err(Error::TransformError)?;

            } else {
                compile::compile(
                    build_dir,
                    &mut rules_sink,
                    &mut tokenizer_sink,
                ).map_err(Error::CollationFailed)?;
            };

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
        Ok(())
    }

    /// Creates a new binary builder. `language_codes` must be in ISO 639-1 (two-letter) format.
    /// If `language_codes` is `&[]`, uses all supported languages.
    /// If this is used in a `build.rs`, `out_dir` should probably be the OUT_DIR environment variable.
    pub fn new<P: AsRef<Path>>(language_codes: &[&str], out_dir: P) -> Self {
        let language_codes: Vec<_> = if language_codes.is_empty() {
            supported_language_codes()
                .into_iter()
                .map(ToOwned::to_owned)
                .collect()
        } else {
            language_codes
                .iter()
                .map(ToOwned::to_owned)
                .map(ToOwned::to_owned)
                .collect::<Vec<String>>()
        };

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
            transform_data_fn: None,
            transform_path_fn: None,
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
    pub fn build(mut self) -> Result<Self> {
        self.language_codes
            .clone()
            .into_iter()
            .try_for_each(|lang_code| self.build_language(&lang_code))?;
        Ok(self)
    }

    /// Validates the binaries by checking if they can be loaded by nlprule.
    pub fn validate(self) -> Result<Self> {
        for lang_code in &self.language_codes {
            let tokenizer_out = self.out_dir.join(tokenizer_filename(lang_code));
            let rules_out = self.out_dir.join(rules_filename(lang_code));

            nlprule::Rules::new(rules_out)
                .map_err(|e| Error::ValidationFailed(lang_code.to_owned(), Binary::Rules, e))?;
            nlprule::Tokenizer::new(tokenizer_out)
                .map_err(|e| Error::ValidationFailed(lang_code.to_owned(), Binary::Tokenizer, e))?;
        }

        Ok(self)
    }

    /// Gets the paths to all files this builder created.
    pub fn outputs(&self) -> &[PathBuf] {
        &self.outputs
    }

    /// Applies the given transformation before placing a binencoded file into the cache
    /// Allows for easier compression usage.
    /// Modifies the cached path by the given path function.
    ///
    /// The resulting files will then reside in the given cache dir if any.
    ///
    /// Attention: Any compression applied here, must be undone in the
    /// `fn postprocess` provided closure to retain the original binenc file
    /// to be consumed by the application code.
    pub fn transform(mut self, proc_fn: &'static (dyn TransformDataFn), path_fn: &'static (dyn TransformPathFn)) -> Self
    {
        self.transform_data_fn = Some(Box::new(proc_fn));
        self.transform_path_fn = Some(Box::new(path_fn));
        self
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
    /// # let mut builder = BinaryBuilder::new(&["en"], tempdir).version("0.3.0");
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
    pub fn postprocess<F, C, P>(mut self, proc_fn: C, path_fn: F) -> Result<Self>
    where
        C: Fn(
            BufReader<File>,
            BufWriter<File>,
        ) -> result::Result<(), OtherError>,
        F: Fn(PathBuf) -> P,
        P: AsRef<Path>,
    {
        for (i, path) in self.outputs.clone().into_iter().enumerate() {
            let reader = BufReader::new(fs::File::open(&path)?);

            let new_path = path_fn(path.clone());
            let new_path = new_path.as_ref();

            let writer = BufWriter::new(File::create(new_path)?);

            proc_fn(reader, writer).map_err(Error::PostprocessingError)?;

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
    fn getting_binary_works() -> Result<()> {
        // this is nice to keep roughly in sync with the latest released version but it is not necessary
        let tempdir = tempdir::TempDir::new("build_dir")?;
        let tempdir = tempdir.path().join("foo.bin");
        assure_binary_availability("0.3.0", "en", Binary::Rules, None, None, None, tempdir)?;

        Ok(())
    }

    #[test]
    fn getting_build_dir_works() -> Result<()> {
        let _ = env_logger::builder().is_test(true).try_init();

        let tempdir = tempdir::TempDir::new("build_dir_test")?;
        let tempdir = tempdir.path();

        get_build_dir("en", &tempdir)?;

        assert_eq!(fs::read_to_string(tempdir.join("lang_code.txt"))?, "en");

        Ok(())
    }

    #[test]
    fn binary_builder_works() -> Result<()> {
        let tempdir = tempdir::TempDir::new("builder_test")?;
        let tempdir = tempdir.path();

        BinaryBuilder::new(&[], tempdir)
            .fallback_to_build_dir(true)
            .build()?
            .validate()?;

        Ok(())
    }

    #[test]
    fn binary_builder_works_with_released_version() -> Result<()> {
        let tempdir = tempdir::TempDir::new("builder_test")?;
        let tempdir = tempdir.path();

        BinaryBuilder::new(&["en"], tempdir)
            .version("0.3.0")
            .build()?;

        Ok(())
    }

    #[test]
    fn binary_builder_works_with_smush() -> Result<()> {
        let tempdir = tempdir::TempDir::new("builder_test")?;
        let tempdir = tempdir.path();

        BinaryBuilder::new(&["en"], tempdir)
            .version("0.3.0")
            .build()?
            .postprocess(
                |mut buffer, mut writer| {
                    let mut tmp = Vec::new();
                    buffer.read_to_end(&mut tmp)?;
                    Ok(writer.write_all(&smush::encode(
                        &tmp,
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
    fn binary_builder_works_with_flate2() -> Result<()> {
        let tempdir = tempdir::TempDir::new("builder_test")?;
        let tempdir = tempdir.path();

        let builder = BinaryBuilder::new(&["en"], tempdir)
            .version("0.3.0")
            .build()?
            .postprocess(
                |mut buffer, writer| {
                    let mut tmp = Vec::new();
                    buffer.read_to_end(&mut tmp)?;
                    Ok(
                        flate2::write::GzEncoder::new(writer, flate2::Compression::default())
                            .write_all(&tmp)?,
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

    #[test]
    fn build_with_zstd_transform() -> Result<()> {
        let tempdir = tempdir::TempDir::new("builder_test")?;
        let tempdir = tempdir.path();

        let builder = BinaryBuilder::new(&["en"], tempdir)
            .version("0.3.0")
            .transform(
                &|mut buffer, mut writer| {
                    let mut data = Vec::new();
                    buffer.read_to_end(&mut data)?;
                    let data = smush::encode(
                        data.as_slice(),
                        smush::Codec::Zstd,
                        smush::Quality::Maximum,
                    )?;
                    writer.write_all(&data)?;

                    // XXX hangs forever, both with `smush` and `brotli` directly
                    // let _ = brotli::BrotliCompress(&mut buffer, &mut sink, &Default::default())?;
                    Ok(())
                },
                &|p: PathBuf| {
                    let mut s = p.to_string_lossy().to_string();
                    s.push_str(".zstd");
                    Ok(PathBuf::from(s))
                },
            )
            .build()?
            .postprocess(
                |mut buffer, mut writer| {
                    dbg!("postprocessing");
                    let mut tmp = Vec::new();
                    buffer.read_to_end(&mut tmp)?;
                    let data = smush::decode(tmp.as_slice(), smush::Codec::Zstd)?;
                    writer.write_all(data.as_slice())?;
                    Ok(())
                    // XXX
                    // Ok(brotli::BrotliDecompress(&mut buffer, &mut writer)?)
                },
                |p| {
                    let path = p.to_string_lossy();
                    assert!(path.ends_with(".zstd"));
                    let end = path.len().saturating_sub(".zstd".len());
                    assert_ne!(end, 0);
                    path[..end].to_owned()
                },
            )?;

        assert_eq!(
            builder.outputs(),
            &[
                tempdir.join("en_tokenizer.bin"),
                tempdir.join("en_rules.bin")
            ]
        );

        let rules_path = tempdir
            .join(Path::new(&rules_filename("en")))
            .with_extension("bin");
        assert!(rules_path.exists());

        // The following will always fail since the versions will mismatch and rebuilding does not make sense
        // `get_build_dir` is tested separately
        //
        // ```rust,no_run
        // let _ = nlprule::Rules::new(rules_path)
        // .map_err(|e| Error::ValidationFailed("en".to_owned(), Binary::Rules, e))?;
        // ```
        Ok(())
    }
}
