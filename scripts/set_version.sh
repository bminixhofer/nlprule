if [ `uname` = "Darwin" ]
then
    SED=gsed
else
    SED=sed
fi

function set_cargo_toml_version {
    VERSION=$1
    FILE=$2

    $SED -i "0,/^version/s/^version *= *\".*\"/version = \"$VERSION\"/" $FILE
}

set_cargo_toml_version $1 core/Cargo.toml
set_cargo_toml_version $1 nlprule/Cargo.toml
set_cargo_toml_version $1 request/Cargo.toml
set_cargo_toml_version $1 bindings/python/Cargo.toml