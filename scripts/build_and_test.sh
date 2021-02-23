# this script assumes the build directories are in data/
# only for convenience
mkdir -p storage
RUST_LOG=INFO cargo run --all-features --bin compile -- --build-dir data/$1 --tokenizer-out storage/$1_tokenizer.bin --rules-out storage/$1_rules.bin
RUST_LOG=WARN cargo run --all-features --bin test_disambiguation -- --tokenizer storage/$1_tokenizer.bin
RUST_LOG=WARN cargo run --all-features --bin test -- --tokenizer storage/$1_tokenizer.bin --rules storage/$1_rules.bin