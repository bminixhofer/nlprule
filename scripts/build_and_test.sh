# this script assumes the build directories are in data/
# only for convenience
mkdir -p storage

# x-- => only compile
# -xx => test_disambiguation and test
# xxx or flags not set => everything
flags=${2:-"xxx"}

if [ "${flags:0:1}" == "x" ] 
then
    RUST_LOG=INFO cargo run --all-features --bin compile -- --build-dir data/$1 --tokenizer-out storage/$1_tokenizer.bin --rules-out storage/$1_rules.bin
fi

if [ "${flags:1:1}" == "x" ] 
then
    RUST_LOG=WARN cargo run --all-features --bin test_disambiguation -- --tokenizer storage/$1_tokenizer.bin
fi

if [ "${flags:2:1}" == "x" ] 
then
    RUST_LOG=WARN cargo run --all-features --bin test -- --tokenizer storage/$1_tokenizer.bin --rules storage/$1_rules.bin
fi