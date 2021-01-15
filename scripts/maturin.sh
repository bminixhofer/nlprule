if [ `uname` = "Darwin" ]
then
    SED=gsed
else
    SED=sed
fi

function build_change {
    FILE=$1
    $SED -i '/.*# BUILD_BINDINGS_COMMENT/s/^/# /g' $FILE
    $SED -i '/.*# BUILD_BINDINGS_UNCOMMENT/s/^# //g' $FILE
}

cp bindings/python/Cargo.toml bindings/python/.Cargo.toml.bak
cp nlprule/Cargo.toml nlprule/.Cargo.toml.bak
cp Cargo.toml .Cargo.toml.bak

build_change bindings/python/Cargo.toml
build_change nlprule/Cargo.toml
build_change Cargo.toml

cd bindings/python
maturin $@
exit_code=$?
cd ../..

mv bindings/python/.Cargo.toml.bak bindings/python/Cargo.toml
mv nlprule/.Cargo.toml.bak nlprule/Cargo.toml
mv .Cargo.toml.bak Cargo.toml

exit $exit_code