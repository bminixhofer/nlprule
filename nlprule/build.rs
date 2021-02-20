//! Compiles the language build configurations in configs/ into two files (one for the tokenizer, one for the rules)
//! so they can be inlined. These configs are included at compile time because they define the neccessary parameters to
//! run the rules for a language correctly. They are NOT user configuration.

use fs::File;
use fs_err as fs;
use std::{collections::HashMap, io::BufWriter, path::Path};

fn main() {
    let path = env!("CARGO_MANIFEST_DIR");
    let path = Path::new(path).join("configs");

    let mut tokenizer_config_map: HashMap<String, serde_json::Value> = HashMap::new();
    let mut rules_config_map: HashMap<String, serde_json::Value> = HashMap::new();

    println!("cargo:rerun-if-changed={}", path.display());

    for entry in fs::read_dir(path).expect("must be able to read config dir") {
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

            let tokenizer_path = entry.path().join("tokenizer.json");
            let rules_path = entry.path().join("rules.json");

            println!("cargo:rerun-if-changed={}", tokenizer_path.display());
            println!("cargo:rerun-if-changed={}", rules_path.display());

            let tokenizer_json =
                fs::read_to_string(tokenizer_path).expect("tokenizer.json for language must exist");
            let rules_json =
                fs::read_to_string(rules_path).expect("rules.json for language must exist");

            tokenizer_config_map.insert(
                lang_code.clone(),
                serde_json::from_str(&tokenizer_json).expect("tokenizer.json must be valid json"),
            );
            rules_config_map.insert(
                lang_code,
                serde_json::from_str(&rules_json).expect("tokenizer.json must be valid json"),
            );
        }
    }

    let out_dir =
        std::env::var("OUT_DIR").expect("OUT_DIR env var must be set when build.rs is run");
    let out_dir = Path::new(&out_dir);

    let tokenizer_config_writer = BufWriter::new(
        File::create(out_dir.join("tokenizer_configs.json"))
            .expect("must be able to create file in out dir"),
    );
    serde_json::to_writer_pretty(tokenizer_config_writer, &tokenizer_config_map)
        .expect("must be able to write JSON to file");

    let rules_config_writer = BufWriter::new(
        File::create(out_dir.join("rules_configs.json"))
            .expect("must be able to create file in out dir"),
    );
    serde_json::to_writer_pretty(rules_config_writer, &rules_config_map)
        .expect("must be able to write JSON to file");
}
