#[allow(unused)]
macro_rules! binary {
    ($component: ty, $lang_code:literal, $binary_name:literal) => {{
        use crate::components::Component;

        let mut bytes: &'static [u8] = include_bytes!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/storage/",
            $lang_code,
            "/",
            $binary_name,
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
