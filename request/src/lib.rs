//! A helper crate for downloading NLPRule resources from their distribution source.

use flate2::read::GzDecoder;
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
    fn filename(&self) -> &'static str {
        match &self {
            Binary::Tokenizer => "tokenizer.bin",
            Binary::Rules => "rules.bin",
        }
    }
}

pub fn get_binary<P: AsRef<Path>>(
    version: &str,
    lang_code: &str,
    binary: Binary,
    cache_dir: Option<P>,
) -> Result<impl Read, Error> {
    let filename = format!("{}_{}", lang_code, binary.filename());

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
}
