# Building the NLPRule binaries

Building the NLPRule binaries requires the *build directory* for the corresponding language. The latest build directories are stored on Backblaze B2. Download them from https://f000.backblazeb2.com/file/nlprule/en.zip (adjusting the two-letter language code accordingly for other languages).

See [Making the build directory](#making-the-build-directory) for information on how to create a 
build directory yourself.

Besides the build directory, a rule config file and tokenizer config file are required. These are stored in-tree at [configs/](configs/). The binaries can then be built with the `compile` target e. g.:

```
RUST_LOG=INFO cargo run --all-features --release --bin compile -- \
    --build-dir data/en \
    --tokenizer-config configs/en/tokenizer.json \
    --rules-config configs/en/rules.json \
    --tokenizer-out storage/en_tokenizer.bin \
    --rules-out storage/en_rules.bin
```

This is expected to warn about errors in the `Rules` since not all grammar rules are supported but should *not* report any errors in the `Tokenizer`.

# Making the build directory

NLPRule needs *build files* to build the rule and tokenizer binaries. These build files contain e. g. the XML files for grammar and disambiguation rules, a dictionary with words and their associated part-of-speech tags / lemmas and some data used for optimizations. Collectively, they form the *build directory*. Each language has a separate build directory.

The build directory for a language can be generated with `make_build_dir.py`. Run `python make_build_dir.py --help` (or take a look at the source code) for more information. 

Below are the commands used to make the build directories for NLPRule's supported languages (of course, the paths need to be adjusted depending on your setup):

### English

```bash
python build/make_build_dir.py \
    --lt_dir=$LT_PATH \
    --lang_code=en \
    --tag_dict_path=$LT_PATH/org/languagetool/resource/en/english.dict \
    --tag_info_path=$LT_PATH/org/languagetool/resource/en/english.info \
    --chunker_token_model=$HOME/Downloads/nlprule/en-token.bin \
    --chunker_pos_model=$HOME/Downloads/nlprule/en-pos-maxent.bin \
    --chunker_chunk_model=$HOME/Downloads/nlprule/en-chunker.bin \
    --out_dir=data/en
```

Chunker binaries can be downloaded from http://opennlp.sourceforge.net/models-1.5/.

### German

```bash
python build/make_build_dir.py \
    --lt_dir=$LT_PATH \
    --lang_code=de \
    --tag_dict_path=$HOME/Downloads/nlprule/german-pos-dict/src/main/resources/org/languagetool/resource/de/german.dict \
    --tag_info_path=$HOME/Downloads/nlprule/german-pos-dict/src/main/resources/org/languagetool/resource/de/german.info \
    --out_dir=data/de
```

The POS dict can be downloaded from https://github.com/languagetool-org/german-pos-dict.

### Spanish

```bash
python build/make_build_dir.py \
    --lt_dir=$LT_PATH \
    --lang_code=es \
    --tag_dict_path=$HOME/Downloads/nlprule/spanish-pos-dict/org/languagetool/resource/es/es-ES.dict \
    --tag_info_path=$HOME/Downloads/nlprule/spanish-pos-dict/org/languagetool/resource/es/es-ES.info \
    --out_dir=data/es
```

Note for Spanish: `disambiguation.xml` is currently manually postprocessed by removing an invalid `<marker>` in `POS_N`, `grammar.xml` is manually postprocessed by fixing the match reference for `EN_TORNO`. Both issues will be fixed in the next LanguageTool release.

The POS dict can be downloaded from https://mvnrepository.com/artifact/org.softcatala/spanish-pos-dict (download the latest version and unzip the `.jar`).
