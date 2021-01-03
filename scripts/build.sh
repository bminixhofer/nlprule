RUST_LOG=WARN cargo run --all-features --release --bin compile -- --tag-paths data/dumps/en/output.dump data/dumps/en/added.txt --tag-remove-paths data/dumps/en/removed.txt --disambiguation-path data/disambiguation.en.canonic.xml --tokenizer-config-path configs/en/tokenizer.json --grammar-path data/grammar.en.canonic.xml --rules-config-path configs/en/rules.json --common-words-path data/en_common.txt --chunker-path data/chunker.json --out-tokenizer-path storage/en/tokenizer.bin --out-rules-path storage/en/rules.bin

RUST_LOG=WARN cargo run --all-features --release --bin compile -- --tag-paths data/dumps/de/output.dump data/dumps/de/added.txt --tag-remove-paths data/dumps/de/removed.txt --disambiguation-path data/disambiguation.de.canonic.xml --tokenizer-config-path configs/de/tokenizer.json --grammar-path data/grammar.de.canonic.xml --rules-config-path configs/de/rules.json --common-words-path data/de_common.txt --out-tokenizer-path storage/de/tokenizer.bin --out-rules-path storage/de/rules.bin

gzip storage/de/rules.bin storage/de/tokenizer.bin
gzip storage/en/rules.bin storage/en/tokenizer.bin
