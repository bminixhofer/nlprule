use std::path::{Path, PathBuf};

const MANIFEST_DIR: &str = env!("CARGO_MANIFEST_DIR");

pub fn binary_path(lang_code: &str, name: &str) -> PathBuf {
    Path::new(MANIFEST_DIR)
        .join(lang_code)
        .join(format!("{}.bin", name))
}

#[allow(unused)]
macro_rules! binary {
    ($component: ty, $lang_code:literal, $name:literal) => {{
        use crate::components::Component;

        let mut bytes: &'static [u8] = include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/storage/",
            $lang_code,
            "/",
            $name,
            ".bin"
        ));

        <$component>::from_reader(&mut bytes)
    }};
}

#[allow(unused)]
const ERROR_MSG: &str = "binaries are pre-tested.";

#[cfg(feature = "binaries-de")]
pub mod de;
#[cfg(feature = "binaries-en")]
pub mod en;
#[cfg(feature = "binaries-es")]
pub mod es;
