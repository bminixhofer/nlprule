if [ "$1" = "" ]
then
  echo "language code must be set as first argument"
  exit
fi

# this script assumes the build directories are in data/
# only for convenience
mkdir -p nlprule/src/storage

# x- => only compile
# -x => only test
# xx or flags not set => everything
flags=${2:-"xx"}

if [ "${flags:0:1}" == "x" ] 
then
    cd nlprule
    RUST_LOG=INFO cargo run --features "compile bin" --bin compile -- --build-dir ../data/$1 --out-dir storage/$1
    cd ..
fi

if [ "${flags:1:1}" == "x" ] 
then
    cd nlprule
    RUST_LOG=INFO cargo run --all-features --bin test -- --lang $1
    cd ..
fi