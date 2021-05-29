mkdir -p data

cd data

for lang in "en" "de" "es"
do
    if [ ! -f $lang.zip ]; then
        wget https://f000.backblazeb2.com/file/nlprule/$lang.zip
        unzip -o $lang.zip
    fi
done