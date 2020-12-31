# Building the tokenizer and rule binary

1. Dump LT tags, download added and removed tags.
2. Download `disambigation.xml` and `grammar.xml` for the language.
    - Canonicalize with e. g. `xmlstarlet c14n`
3. (optional) Prepare a chunker using `src/tokenizer/serialize_chunker.py`.
4. Run the compile script.
E. g. for english:

```bash
RUST_LOG=WARN cargo run --all-features --release --bin compile -- --tag-paths data/dumps/en/output.dump data/dumps/en/added.txt --tag-remove-paths data/dumps/en/removed.txt --disambiguation-path data/disambiguation.en.canonic.xml --tokenizer-config-path configs/en/tokenizer.json --grammar-path data/grammar.en.canonic.xml --rules-config-path configs/en/rules.json --common-words-path data/de_common.txt --chunker-path data/chunker.json --out-tokenizer-path storage/en/tokenizer.bin --out-rules-path storage/en/rules.bin
```

or for German (no chunker):
```bash
RUST_LOG=WARN cargo run --all-features --release --bin compile -- --tag-paths data/dumps/de/output.dump data/dumps/de/added.txt --tag-remove-paths data/dumps/de/removed.txt --disambiguation-path data/disambiguation.de.canonic.xml --tokenizer-config-path configs/de/tokenizer.json --grammar-path data/grammar.de.canonic.xml --rules-config-path configs/de/rules.json --common-words-path data/de_common.txt --out-tokenizer-path storage/de/tokenizer.bin --out-rules-path storage/de/rules.bin
```

## Testing

Run all tests for disambiguation rules with the `test_disambiguation` binary

```bash
cargo run --all-features --release --bin test_disambiguation -- --tokenizer tokenizer.bin
```

and for the grammar rules with the `test` binary

```bash
cargo run --all-features --release --bin test -- --tokenizer tokenizer.bin --rules rules.bin
```