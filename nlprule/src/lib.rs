pub use nlprule_core::*;

// fix for https://github.com/rust-lang/rust/issues/75075#issuecomment-671370162
#[cfg(host_family = "windows")]
#[macro_export]
#[doc(hidden)]
macro_rules! path_separator {
    () => {
        r"\"
    };
}
#[cfg(not(host_family = "windows"))]
#[macro_export]
#[doc(hidden)]
macro_rules! path_separator {
    () => {
        r"/"
    };
}

// making this a trait `FromLangCode` would be nice but is blocked by https://github.com/rust-lang/rust/issues/53749
#[macro_export]
macro_rules! tokenizer {
    ($lang_code:literal) => {{
        let bytes = include_bytes!(concat!(
            env!("OUT_DIR"),
            $crate::path_separator!(),
            $lang_code,
            "_tokenizer.bin"
        ));

        $crate::Tokenizer::from_reader(std::io::Cursor::new(bytes))
    }};
}

#[macro_export]
macro_rules! rules {
    ($lang_code:literal) => {{
        let bytes = include_bytes!(concat!(
            env!("OUT_DIR"),
            $crate::path_separator!(),
            $lang_code,
            "_rules.bin"
        ));

        $crate::Tokenizer::from_reader(std::io::Cursor::new(bytes))
    }};
}

#[cfg(test)]
mod tests {
    use super::{rules, tokenizer};

    #[test]
    fn tokenizer_from_lang_code_works() {
        let _tokenizer = tokenizer!("en").unwrap();
    }

    #[test]
    fn rules_from_lang_code_works() {
        let _rules = rules!("en").unwrap();
    }
}
