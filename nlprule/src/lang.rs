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

const ERROR_MSG: &str = "binaries are pre-tested.";

pub mod de;
pub mod en;
pub mod es;
