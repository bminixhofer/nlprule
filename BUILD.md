# Building the tokenizer and rule binary

1. Dump LT tags, download added and removed tags.
2. Download `disambigation.xml` and `grammar.xml` for the language.
    - Canonicalize with e. g. `xmlstarlet c14n`
3. (optional) Prepare a chunker using `src/tokenizer/serialize_chunker.py`.
4. Run the compile script.
E. g. for english:

```bash
RUST_LOG=WARN cargo run --all-features --release --bin compile -- --tag-paths data/en/tags/output.dump data/en/tags/added.txt --tag-remove-paths data/en/tags/removed.txt --disambiguation-path data/en/disambiguation.canonic.xml --tokenizer-config-path configs/en/tokenizer.json --grammar-path data/en/grammar.canonic.xml --rules-config-path configs/en/rules.json --common-words-path data/en/common.txt --chunker-path data/en/chunker.json --out-tokenizer-path storage/en_tokenizer.bin --out-rules-path storage/en_rules.bin
```

or for German (no chunker):
```bash
RUST_LOG=WARN cargo run --all-features --release --bin compile -- --tag-paths data/de/tags/output.dump data/de/tags/added.txt --tag-remove-paths data/de/tags/removed.txt --disambiguation-path data/de/disambiguation.canonic.xml --tokenizer-config-path configs/de/tokenizer.json --grammar-path data/de/grammar.canonic.xml --rules-config-path configs/de/rules.json --common-words-path data/de/common.txt --out-tokenizer-path storage/de_tokenizer.bin --out-rules-path storage/de_rules.bin
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