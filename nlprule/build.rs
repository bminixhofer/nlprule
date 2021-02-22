//! Compiles the language build configurations in configs/ into two files (one for the tokenizer, one for the rules)
//! so they can be inlined. These configs are included at compile time because they define the neccessary parameters to
//! run the rules for a language correctly. They are NOT user configuration.

use fs::File;
use fs_err as fs;
use std::{collections::HashMap, io::BufWriter, path::Path};

fn main() {
    let path = env!("CARGO_MANIFEST_DIR");
    let path = Path::new(path).join("configs");

    let out_dir =
        std::env::var("OUT_DIR").expect("OUT_DIR env var must be set when build.rs is run");
    let out_dir = Path::new(&out_dir);

    println!("cargo:rerun-if-changed={}", path.display());

    for (filename, joined_filename) in &[
        ("tokenizer.json", "tokenizer_configs.json"),
        ("rules.json", "rules_configs.json"),
        ("tagger.json", "tagger_configs.json"),
    ] {
        let mut config_map: HashMap<String, serde_json::Value> = HashMap::new();

        for entry in fs::read_dir(&path).expect("must be able to read config dir") {
            let entry = entry.expect("must be able to read config dir entry");

            println!("cargo:rerun-if-changed={}", entry.path().display());

            if entry.path().is_dir() {
                let lang_code = entry
                    .path()
                    .file_name()
                    .expect("directory must have name")
                    .to_str()
                    .expect("directory name must be unicode")
                    .to_string();

                let path = entry.path().join(filename);

                println!("cargo:rerun-if-changed={}", path.display());

                let json_str = fs::read_to_string(path)
                    .unwrap_or_else(|_| panic!("{} for 'lang_code' must exist", filename));

                config_map.insert(
                    lang_code,
                    serde_json::from_str(&json_str)
                        .unwrap_or_else(|_| panic!("{} for language must be valid json", filename)),
                );
            }
        }

        let config_writer = BufWriter::new(
            File::create(out_dir.join(joined_filename))
                .expect("must be able to create file in out dir"),
        );
        serde_json::to_writer_pretty(config_writer, &config_map)
            .expect("must be able to write JSON to file");
    }
}
