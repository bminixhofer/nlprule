if [ "$1" = "" ]
then
  echo "language code must be set as first argument"
  exit
fi

mkdir -p nlprule/src/storage

cd data

# download + extract the build directory from backblaze if we don't have it yet
if [ ! -f $1.zip ]; then
  wget https://f000.backblazeb2.com/file/nlprule/$$1.zip
  unzip -o $1.zip
fi

cd ..

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
    RUST_LOG=INFO cargo run --features "bin binaries-$1" --bin test_$1
    cd ..
fi