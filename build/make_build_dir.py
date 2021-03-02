from argparse import ArgumentParser, RawTextHelpFormatter
from pathlib import Path
from shutil import copyfile
import os
import logging
from zipfile import ZipFile
import lxml.etree as ET
import wordfreq
from glob import glob
from chardet.universaldetector import UniversalDetector

from chunker import write_chunker  # type: ignore


def write_freqlist(f, lang_code, top_n=1000):
    # wordfreq lists are lowercase so we add title case manually
    for word in wordfreq.top_n_list(lang_code, top_n):
        f.write(word + "\n")
        f.write(word.title() + "\n")


def canonicalize(path):
    et = ET.parse(str(path))
    et.write_c14n(open(path, "wb"))


def copy_lt_files(out_dir, lt_dir, lang_code):
    tag_dir = out_dir / "tags"
    tag_dir.mkdir()

    lt_resource_dir = lt_dir / "org" / "languagetool" / "resource" / lang_code
    lt_rule_dir = lt_dir / "org" / "languagetool" / "rules" / lang_code

    for source, dest in [
        (lt_resource_dir / "added.txt", tag_dir / "added.txt"),
        (lt_resource_dir / "removed.txt", tag_dir / "removed.txt"),
        (lt_resource_dir / "disambiguation.xml", out_dir / "disambiguation.xml"),
        (lt_resource_dir / "multiwords.txt", tag_dir / "multiwords.txt",),
        (lt_rule_dir / "grammar.xml", out_dir / "grammar.xml"),
    ]:
        if source.exists():
            copyfile(source, dest)
        else:
            logging.warning(f"{source} does not exist.")

    # copy from zipfiles
    for zipfile, source, dest in [
        (
            lt_dir / "libs" / "languagetool-core.jar",
            Path("org") / "languagetool" / "resource" / "segment.srx",
            out_dir / "segment.srx",
        )
    ]:
        file = ZipFile(zipfile)
        with open(dest, "wb") as f:
            f.write(file.read(str(source)))

    # canonicalize XML
    for xmlfile in ["grammar.xml", "disambiguation.xml"]:
        canonicalize(out_dir / xmlfile)


def dump_dict(out_path, lt_dir, tag_dict_path, tag_info_path):
    # dump dictionary, see https://dev.languagetool.org/developing-a-tagger-dictionary
    os.system(
        f"java -cp {lt_dir / 'languagetool.jar'} org.languagetool.tools.DictionaryExporter "
        f"-i {tag_dict_path} -info {tag_info_path} -o {out_path}"
    )

    # the dumped dictionary is sometimes not in utf-8
    detector = UniversalDetector()
    for i, line in enumerate(open(out_path, "rb")):
        detector.feed(line)

        if i > 10_000:
            detector.close()
            break

    result = detector.result

    print(
        f"Dump was encoded as {result['encoding']} with confidence {result['confidence']}."
    )
    dump_bytes = open(out_path, "rb").read()

    with open(out_path, "w") as f:
        f.write(dump_bytes.decode(result["encoding"] or "utf-8"))


if __name__ == "__main__":
    parser = ArgumentParser(
        description="""
Script to generate the build files for nlprule binaries.
See the accompanying README.md for example usages.

Requirements:
- Python >= 3.6
- a version of Java compatible with the used LanguageTool version
- Python packages from `requirements.txt`. Install with `pip install -r requirements.txt`
""",
        formatter_class=RawTextHelpFormatter,
    )
    parser.add_argument(
        "--lt_dir",
        type=lambda p: Path(p).absolute(),
        help="Directory the LanguageTool Desktop version is in. Download instructions: https://dev.languagetool.org/http-server#getting-the-server.",
    )
    parser.add_argument(
        "--lang_code",
        type=str,
        help="Language code in ISO_639-1 (two letter) format e. g. 'en'.",
    )
    parser.add_argument(
        "--tag_dict_path",
        type=lambda p: Path(p).absolute(),
        help="Path to a tagger dictionary .dict file.",
    )
    parser.add_argument(
        "--tag_info_path",
        type=lambda p: Path(p).absolute(),
        help="Path to the accompanying tagger dictionary .info file.",
    )
    parser.add_argument(
        "--chunker_token_model",
        default=None,
        help="""
Path to the OpenNLP tokenizer binary. Binaries can be downloaded from here: http://opennlp.sourceforge.net/models-1.5/

Only needed if the language requires a chunker (e. g. English).
""",
    )
    parser.add_argument(
        "--chunker_pos_model",
        default=None,
        help="Path to the OpenNLP POS tagger binary. See token model message for details.",
    )
    parser.add_argument(
        "--chunker_chunk_model",
        default=None,
        help="Path to the OpenNLP chunker binary. See token model message for details.",
    )
    parser.add_argument(
        "--out_dir",
        type=lambda p: Path(p).absolute(),
        help="Directory to store the build files in.",
    )

    args = parser.parse_args()
    args.out_dir.mkdir(parents=True)

    write_freqlist(open(args.out_dir / "common.txt", "w"), args.lang_code)
    copy_lt_files(args.out_dir, args.lt_dir, args.lang_code)

    # tagger dictionary
    dump_dict(
        args.out_dir / "tags" / "output.dump",
        args.lt_dir,
        args.tag_dict_path,
        args.tag_info_path,
    )

    # spell dictionaries
    (args.out_dir / "spell").mkdir()
    for dic in glob(
        str(
            args.lt_dir
            / "org"
            / "languagetool"
            / "resource"
            / args.lang_code
            / "hunspell"
            / "*.dict"
        )
    ):
        dic = Path(dic)
        info = Path(dic).with_suffix(".info")

        variant_name = dic.stem

        dump_dict(
            args.out_dir / "spell" / f"{variant_name}.dump", args.lt_dir, dic, info,
        )

    if (
        args.chunker_token_model is not None
        and args.chunker_pos_model is not None
        and args.chunker_chunk_model is not None
    ):
        write_chunker(
            args.out_dir / "chunker.json",
            args.chunker_token_model,
            args.chunker_pos_model,
            args.chunker_chunk_model,
        )

    open(args.out_dir / "lang_code.txt", "w").write(args.lang_code)

    print("Success!")
