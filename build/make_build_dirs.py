"""Generate nlprule build directories for all LanguageTool languages.

Works from a LanguageTool desktop distribution (unzipped) which contains:
- loose resources under org/languagetool/{rules,resource}/
- POS dictionary jars under libs/

Requires Java (11+) on PATH or via --java to run the DictionaryExporter.

Usage:
    python make_build_dirs.py --lt_dir /path/to/LanguageTool-6.5 --out_root ../data
"""

from argparse import ArgumentParser
from pathlib import Path
from shutil import copyfile
from subprocess import run
from tempfile import TemporaryDirectory
from urllib.request import urlopen
from zipfile import ZipFile
import json
import os
import re
import logging

import lxml.etree as ET
import wordfreq

MAX_TABLE_LINES = 10000

LT_MASTER = "https://raw.githubusercontent.com/languagetool-org/languagetool/master"

# analyzer + synth dict locations: ('loose', filename_stem) or ('jar', jar, entry)
DICT_SOURCES = {
    "ar": ("loose", "arabic"),
    "ast": ("jar", "asturian-pos-dict.jar", "asturian"),
    "br": ("loose", "breton"),
    "ca": ("jar", "catalan-pos-dict.jar", "ca-ES"),
    "crh": ("jar", "morfologik-crh-lt.jar", "crimean_tatar"),
    "da": ("loose", "danish"),
    "de": ("jar", "german-pos-dict.jar", "german"),
    "de-DE-x-simple-language": ("jar", "german-pos-dict.jar", "german", "de"),
    "el": ("loose", "greek"),
    "en": ("jar", "english-pos-dict.jar", "english"),
    "es": ("jar", "spanish-pos-dict.jar", "es-ES"),
    "fr": ("jar", "french-pos-dict.jar", "french"),
    "ga": ("jar", "languagetool-ga-dicts.jar", "irish"),
    "gl": ("loose", "galician"),
    "it": ("loose", "italian"),
    "km": ("loose", "khmer"),
    "nl": ("jar", "dutch-pos-dict.jar", "dutch"),
    "pl": ("loose", "polish"),
    "pt": ("jar", "portuguese-pos-dict.jar", "portuguese"),
    "ro": ("loose", "romanian"),
    "ru": ("loose", "russian"),
    "sk": ("loose", "slovak"),
    "sv": ("loose", "swedish"),
    "ta": ("loose", "tamil"),
    "tl": ("loose", "tagalog"),
    "uk": ("jar", "morfologik-ukrainian-lt.jar", "ukrainian"),
}

# languages whose grammar/disambiguation XML must be fetched from LT master
GITHUB_ONLY = {"lt", "ml", "sr"}

# wordfreq language codes where they differ from the LT code
WORDFREQ_CODES = {
    "de-DE-x-simple-language": "de",
    "tl": "fil",
}

URL_JOIN_REGEX = (
    r"(https?:\\/\\/(?:www\\.|(?!www))[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]"
    r"\\.[^\\s]{2,}|www\\.[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\\.[^\\s]{2,}|"
    r"https?:\\/\\/(?:www\\.|(?!www))[a-zA-Z0-9]+\\.[^\\s]{2,}|"
    r"www\\.[a-zA-Z0-9]+\\.[^\\s]{2,})"
)

def sh(cmd, **kwargs):
    result = run(cmd, capture_output=True, text=True, **kwargs)
    if result.returncode != 0:
        logging.error("Command failed: %s\n%s", cmd, result.stderr[:2000])
    return result


def fetch_github(path: str, dest: Path):
    try:
        content = urlopen(f"{LT_MASTER}/{path}", timeout=60).read()
        dest.write_bytes(content)
        return True
    except Exception as e:  # noqa: BLE001
        logging.warning("Could not fetch %s: %s", path, e)
        return False


def canonicalize(src: Path, dest: Path):
    """Parse `src` (resolving external DTD entities like &foo; / %entities;) and
    write the canonicalized XML to `dest`."""
    parser = ET.XMLParser(load_dtd=True, resolve_entities=True, no_network=True, huge_tree=True)
    try:
        et = ET.parse(str(src), parser)
    except ET.XMLSyntaxError:
        et = ET.parse(str(src))
    et.write_c14n(open(dest, "wb"))


def write_freqlist(f, lang_code, top_n=1000):
    wf_code = WORDFREQ_CODES.get(lang_code, lang_code)
    try:
        words = wordfreq.top_n_list(wf_code, top_n)
    except Exception:  # noqa: BLE001
        words = []
    if not words:
        logging.warning("no wordfreq data for %s; common.txt will be minimal", lang_code)
    for word in words:
        f.write(word + "\n")
        f.write(word.title() + "\n")


def dump_dictionary(java, classpath, dict_path, info_path, out_path):
    result = sh(
        [
            java,
            "-cp",
            classpath,
            "org.languagetool.tools.DictionaryExporter",
            "-i",
            str(dict_path),
            "-info",
            str(info_path),
            "-o",
            str(out_path),
        ]
    )
    if result.returncode != 0:
        logging.error("DictionaryExporter failed for %s", dict_path)
        return False

    # dumped dictionaries are sometimes not in utf-8; re-encode defensively
    try:
        content = out_path.read_bytes().decode("utf-8")
    except UnicodeDecodeError:
        import chardet

        detection = chardet.detect(out_path.read_bytes()[:2_000_000])
        encoding = detection.get("encoding") or "latin-1"
        content = out_path.read_bytes().decode(encoding, errors="replace")
    out_path.write_text(content)
    return True


def extract_dict(jar_path, stem, resource_dir, workdir):
    """Extract {stem}.dict/.info for a language from its jar (or copy if loose)."""
    extracted = {}
    for suffix in (".dict", ".info"):
        name = f"{stem}{suffix}"
        loose = resource_dir / name
        target = workdir / name
        if loose.exists():
            copyfile(loose, target)
        else:
            with ZipFile(jar_path) as zf:
                entry = f"org/languagetool/resource/{resource_dir.name}/{name}"
                try:
                    target.write_bytes(zf.read(entry))
                except KeyError:
                    logging.warning("%s not found in %s", entry, jar_path)
                    continue
        extracted[suffix] = target
    return extracted


def literal_postags(xml_path):
    """All literal POS tags referenced via postag= attributes (ignoring regexes)."""
    if not xml_path.exists():
        return set()
    content = xml_path.read_text(encoding="utf-8", errors="replace")
    tags = set()
    for match in re.finditer(r'postag="([^"]*)"', content):
        tag = match.group(1)
        if tag and not re.search(r"[|?*+()\[\]{}.\\^$]", tag):
            tags.add(tag)
    return tags


def dump_tags(dump_path):
    """All POS tags present in an analyzer dump (3rd column)."""
    tags = set()
    if not dump_path.exists():
        return tags
    with open(dump_path, encoding="utf-8", errors="replace") as f:
        for line in f:
            parts = line.rstrip("\n").split("\t")
            if len(parts) >= 3 and parts[2]:
                tags.add(parts[2])
    return tags



def escape_xml(text: str) -> str:
    return (
        text.replace("&", "&amp;")
        .replace("<", "&lt;")
        .replace(">", "&gt;")
        .replace('"', "&quot;")
    )


def escape_regex(text: str) -> str:
    out = []
    for c in text:
        if c in "\\.^$|?*+()[]{}":
            out.append("\\")
        out.append(c)
    return "".join(out)


def _simple_word_id(w):
    return re.sub(r"[^\w]", "", w.upper().replace("'", "Q").replace(" ", "_")) or "X"


def _simple_pattern(wrong):
    if " " in wrong:
        return "".join(f"<token>{escape_xml(t)}</token>" for t in wrong.split(" "))
    return f'<token regexp="yes">{escape_xml(re.escape(wrong))}</token>'


# Esquema de ids empiricamente verificado contra LT 6.5 (ver delta_log):
# - be/ru/uk/fa/pl replace.txt -> id único <L2>_SIMPLE_REPLACE
# - es/fr/ca replace.txt -> <L2>_SIMPLE_REPLACE + subregla SIMPLE_<W>
# - de/en/km replace.txt -> <L2>_SIMPLE_REPLACE + subregla <W> (sin infijo)
# - uk variantes -> UK_SIMPLE_REPLACE_RENAMED/_SOFT/_SPELLING_1992 (únicos)
# - ca tablas -> CA_SIMPLE_REPLACE_<SUFFIX>_<W>; operationnames -> NOMS_OPERACIONS_<W>
SIMPLE_REPLACE_SINGLE = {"be", "ru", "uk", "fa", "pl"}
SIMPLE_REPLACE_INFIX = {"es": "SIMPLE_", "fr": "SIMPLE_", "ca": "SIMPLE_"}
UK_TABLE_IDS = {"renamed": "UK_SIMPLE_REPLACE_RENAMED",
                "soft": "UK_SIMPLE_REPLACE_SOFT",
                "spelling_2019": "UK_SIMPLE_REPLACE_SPELLING_1992"}


def simple_replace_table_ids(lang, stem):
    """(group_id, per_word, infix) para una tabla replace* de un idioma."""
    if stem == "replace":
        if lang in SIMPLE_REPLACE_SINGLE:
            return f"{lang.upper()}_SIMPLE_REPLACE", False, ""
        infix = SIMPLE_REPLACE_INFIX.get(lang, "")
        return f"{lang.upper()}_SIMPLE_REPLACE", True, infix
    sfx = stem.replace("replace_", "")
    if lang == "uk":
        return UK_TABLE_IDS[sfx], False, ""
    group = "NOMS_OPERACIONS" if sfx == "operationnames" else f"{lang.upper()}_SIMPLE_REPLACE_{sfx.upper()}"
    return group, True, ""


def simple_replace_rules(tables, lang=None):
    """Convierte las tablas SimpleReplaceRule de LT (wrong=correct) en XML:
    una categoría por tabla, ids idénticos a los que LT reporta (ver
    simple_replace_table_ids). Una regla por variante incorrecta (las
    alternativas multi-token no se pueden expresar como secuencia)."""
    parts = []
    for stem, path in tables:
        group, per_word, infix = simple_replace_table_ids(lang, stem)
        rules = []
        try:
            content = Path(path).read_text(encoding="utf-8", errors="replace")
        except OSError:
            continue
        # nl (66k líneas) usa la vía del filter table, no XML
        if content.count("\n") > MAX_TABLE_LINES:
            logging.warning(
                "%s: table %s has more than %d lines, skipping",
                stem, MAX_TABLE_LINES,
            )
            continue
        for line in content.splitlines():
            line = line.split("#")[0].strip()
            if not line or "=" not in line:
                continue
            wrong, correct = line.split("=", 1)
            correct = correct.split("\t")[0]
            wrongs = [w.strip() for w in wrong.split("|") if w.strip()]
            corrects = [c.strip() for c in correct.split("|") if c.strip()]
            if not wrongs or not corrects:
                continue
            suggestions = "|".join(escape_xml(c) for c in corrects)
            for w in wrongs:
                rid = f' id="{group}_{infix}{_simple_word_id(w)}"' if per_word else ""
                rules.append(
                    f"<rule{rid}>\n"
                    f"<pattern>{_simple_pattern(w)}</pattern>\n"
                    f"<message>Did you mean <suggestion>{suggestions}</suggestion>?</message>\n"
                    "</rule>"
                )
        if rules:
            parts.append(
                f'<category id="{group}" name="Simple replacements ({escape_xml(stem)})">\n'
                f'<rulegroup id="{group}" name="Simple replacements ({escape_xml(stem)})">\n'
                + "\n".join(rules)
                + "\n</rulegroup>\n</category>"
            )
    return "\n".join(parts)

SPELLER_DICTS = {
    "ca": [("catalan-pos-dict.jar", "ca/ca-ES_spelling")],
    "de": [(None, "de/hunspell/de_DE")],  # loose in the dist
    "nl": [("dutch-pos-dict.jar", "nl/spelling/nl_NL")],
    "pt": [("portuguese-pos-dict.jar", "pt/spelling/pt-PT-90")],
    "ru": [(None, "ru/hunspell/ru_RU")],
    "pl": [(None, "pl/hunspell/pl_PL")],
    "it": [(None, "it/hunspell/it_IT")],
    "br": [(None, "br/hunspell/br_FR")],
    "gl": [(None, "gl/galician")],
    "ga": [("languagetool-ga-dicts.jar", "ga/hunspell/ga_IE")],
}

# plain-text word lists added to the speller vocabulary per language
SPELLER_LISTS = {
    "en": ["en/hunspell/spelling.txt", "en/hunspell/ignore.txt"],
    # German: LT's ignore/spelling/added lists plus the hunspell .dic word
    # column (the morfologik de_DE.dict dump misses plain-form words like
    # "baff", "online", "US"). spelling_merged.txt = accepted-but-recommend.
    "de": [
        "de/hunspell/spelling.txt",
        "de/hunspell/ignore.txt",
        "de/hunspell/spelling_merged.txt",
        "de/hunspell/spelling_custom.txt",
        "de/added.txt",
        "de/hunspell/de_DE.dic",
    ],
    "fr": ["fr/added.txt"],
    "es": ["es/hunspell/spelling.txt", "es/hunspell/ignore.txt"],
    "ca": ["ca/spelling.txt", "ca/added.txt", "ca/hunspell/ignore.txt"],
    "nl": ["nl/added.txt"],
    "ru": ["ru/hunspell/spelling.txt", "ru/hunspell/ignore.txt", "ru/added.txt"],
    "pt": [],
    "pl": ["pl/hunspell/ignore.txt"],
    "it": ["it/hunspell/spelling.txt", "it/hunspell/ignore.txt"],
    "gl": ["gl/added.txt"],
}


def _words_from_lines(text):
    words = set()
    for line in text.splitlines():
        line = line.split("#")[0].strip()
        if not line:
            continue
        # added.txt/removed.txt style "form\tlemma\tpos" or plain word lists;
        # hunspell .dic lines carry affix flags after an unescaped slash
        # ("Fahrrad/ABX") — strip them (flags are all-uppercase).
        first = line.split("\t")[0].strip()
        if "/" in first:
            stem, _, flags = first.partition("/")
            if flags and flags.strip("\\/") == flags.upper() and flags.isalpha():
                first = stem.strip()
        if first:
            words.add(first)
    return words


def make_filter_data(lang, lt_dir, resource_dir, dist_rules, out_dir, java, classpath):
    """Create the `filters/` directory with data for the runtime Java filters."""
    filters = out_dir / "filters"
    filters.mkdir(exist_ok=True)
    work = out_dir / ".filter_work"
    work.mkdir(exist_ok=True)

    spelling_global = work / "spelling_global.txt"
    if not spelling_global.exists():
        with ZipFile(lt_dir / "libs" / "languagetool-core.jar") as zf:
            spelling_global.write_bytes(
                zf.read("org/languagetool/resource/spelling_global.txt")
            )

    # multitoken suggestion list
    if lang in MULTITOKEN_LISTS:
        lines = [spelling_global.read_text(encoding="utf-8", errors="replace")]
        for rel in MULTITOKEN_LISTS[lang]:
            path = lt_dir / "org" / "languagetool" / "resource" / rel
            if path.exists():
                lines.append(path.read_text(encoding="utf-8", errors="replace"))
            else:
                logging.warning("multitoken list missing: %s", path)
        (filters / "multitoken.txt").write_text("\n".join(lines), encoding="utf-8")

    # speller vocabulary: tagger dump forms + dumped speller dicts + plain lists
    words = set()
    tagger_dump = out_dir / "tags" / "output.dump"
    if tagger_dump.exists():
        with tagger_dump.open(encoding="utf-8", errors="replace") as f:
            for line in f:
                form = line.split("\t")[0]
                if form:
                    words.add(form)
    for jar, stem in SPELLER_DICTS.get(lang, []):
        parts = stem.split("/")
        sub = lt_dir / "org" / "languagetool" / "resource" / "/".join(parts[:-1])
        name = parts[-1]
        extracted = {}
        for suffix in (".dict", ".info"):
            loose = sub / f"{name}{suffix}"
            target = work / f"{lang}_{name}{suffix}"
            if loose.exists():
                copyfile(loose, target)
                extracted[suffix] = target
            elif jar:
                with ZipFile(lt_dir / "libs" / jar) as zf:
                    entry = (
                        "org/languagetool/resource/"
                        + "/".join(parts)
                        + suffix
                    )
                    try:
                        target.write_bytes(zf.read(entry))
                        extracted[suffix] = target
                    except KeyError:
                        logging.warning("%s not found in %s", entry, jar)
        if ".dict" in extracted and ".info" in extracted:
            dump = work / f"{lang}_{name}_speller.dump"
            if dump_dictionary(java, classpath, extracted[".dict"], extracted[".info"], dump):
                words |= _words_from_lines(dump.read_text(encoding="utf-8", errors="replace"))
    for rel in SPELLER_LISTS.get(lang, []):
        path = lt_dir / "org" / "languagetool" / "resource" / rel
        if path.exists():
            words |= _words_from_lines(path.read_text(encoding="utf-8", errors="replace"))
    if lang in MULTITOKEN_LISTS and (filters / "multitoken.txt").exists():
        pass  # multitoken entries are covered by the lists above via the tagger dump
    if words:
        (filters / "speller.txt").write_text(
            "\n".join(sorted(words)) + "\n", encoding="utf-8"
        )

    # confusion pairs
    pair_sources = []
    if lang in ("es", "ca"):
        pair_sources = [dist_rules / "confusion_pairs.txt"]
    elif lang == "pt":
        pair_sources = [
            dist_rules / "confusion_pairs.txt",
            dist_rules / "pt-PT" / "confusion_pairs.txt",
        ]
    pair_text = []
    for src in pair_sources:
        if src.exists():
            pair_text.append(src.read_text(encoding="utf-8", errors="replace"))
    if pair_text:
        (filters / "confusion_pairs.txt").write_text("\n".join(pair_text), encoding="utf-8")

    # German compound parts
    if lang == "de":
        src = dist_rules / "addedCompound.txt"
        if src.exists():
            copyfile(src, filters / "added_compound.txt")

    logging.info("%s: filter data written to %s", lang, filters)


def make_build_dir(lang, lt_dir, out_root, java, classpath, keep_going):
    dist_rules = lt_dir / "org" / "languagetool" / "rules" / lang
    resource_dir = lt_dir / "org" / "languagetool" / "resource" / lang

    out_dir = out_root / lang
    if out_dir.exists() and (out_dir / "lang_code.txt").exists() and not keep_going:
        logging.info("%s: build dir exists, skipping", lang)
        return True
    out_dir.mkdir(parents=True, exist_ok=True)
    tag_dir = out_dir / "tags"
    tag_dir.mkdir(exist_ok=True)

    # grammar.xml
    grammar = out_dir / "grammar.xml"
    merged_grammar = False
    if dist_rules.exists() and (dist_rules / "grammar.xml").exists():
        # some languages (uk, sk) split rules over several grammar-*.xml files
        # which LT loads by convention; merge them into one file
        extra_grammars = sorted(
            g
            for g in dist_rules.glob("grammar-*.xml")
            if "-l2-" not in g.name  # L2 variants (e.g. English for German speakers) are separate languages
        )

        if extra_grammars:
            merged = [
                '<?xml version="1.0" encoding="UTF-8"?>',
                '<rules lang="%s">' % lang,
            ]
            dtd_parser = ET.XMLParser(
                load_dtd=True, resolve_entities=True, no_network=True, huge_tree=True
            )

            def children_of(path):
                try:
                    root = ET.parse(str(path), dtd_parser).getroot()
                except ET.XMLSyntaxError:
                    content = path.read_bytes()
                    try:
                        root = ET.fromstring(content)
                    except ET.XMLSyntaxError:
                        # fragment without a root element: wrap it
                        root = ET.fromstring(b"<rules>" + content + b"</rules>")
                for child in root:
                    if isinstance(child.tag, str):
                        yield ET.tostring(child, encoding="unicode")

            merged.extend(children_of(dist_rules / "grammar.xml"))
            for extra in extra_grammars:
                merged.extend(children_of(extra))
            merged.append("</rules>")
            grammar.write_text("\n".join(merged), encoding="utf-8")
            merged_grammar = True
        else:
            copyfile(dist_rules / "grammar.xml", grammar)
            merged_grammar = False
    elif lang in GITHUB_ONLY:
        base = (
            f"languagetool-language-modules/{lang}/src/main/resources/"
            f"org/languagetool/rules/{lang}"
        )
        shell_ok = fetch_github(f"{base}/grammar.xml", grammar)

        # languages like sr split rules over grammar-*.xml files like uk
        sub_files = []
        for candidate in [
            "grammar-barbarism.xml",
            "grammar-grammar.xml",
            "grammar-logical.xml",
            "grammar-punctuation.xml",
            "grammar-spelling.xml",
            "grammar-style.xml",
            "grammar-typography.xml",
            "grammar-nezaradene.xml",
        ]:
            target = out_dir / candidate
            if fetch_github(f"{base}/{candidate}", target):
                sub_files.append(target)

        if shell_ok and sub_files:
            merged = [
                '<?xml version="1.0" encoding="UTF-8"?>',
                '<rules lang="%s">' % lang,
            ]
            dtd_parser = ET.XMLParser(
                load_dtd=True, resolve_entities=True, no_network=True, huge_tree=True
            )

            def children_of(path):
                try:
                    root = ET.parse(str(path), dtd_parser).getroot()
                except ET.XMLSyntaxError:
                    content = path.read_bytes()
                    try:
                        root = ET.fromstring(content)
                    except ET.XMLSyntaxError:
                        root = ET.fromstring(b"<rules>" + content + b"</rules>")
                for child in root:
                    if isinstance(child.tag, str):
                        yield ET.tostring(child, encoding="unicode")

            merged.extend(children_of(grammar))
            for sub in sub_files:
                merged.extend(children_of(sub))
            merged.append("</rules>")
            grammar.write_text("\n".join(merged), encoding="utf-8")
            merged_grammar = True
        elif shell_ok:
            merged_grammar = False
        else:
            logging.warning("%s: no grammar.xml", lang)
            merged_grammar = False
    else:
        logging.warning("%s: no grammar.xml", lang)

    # disambiguation.xml
    disambig = out_dir / "disambiguation.xml"
    if (resource_dir / "disambiguation.xml").exists():
        copyfile(resource_dir / "disambiguation.xml", disambig)
    elif lang in GITHUB_ONLY:
        fetch_github(
            f"languagetool-language-modules/{lang}/src/main/resources/"
            f"org/languagetool/resource/{lang}/disambiguation.xml",
            disambig,
        )
    if not disambig.exists():
        disambig.write_text("<?xml version='1.0' encoding='UTF-8'?>\n<rules/>\n")

    for name, dest in [
        ("added.txt", "added.txt"),
        ("removed.txt", "removed.txt"),
        ("multiwords.txt", "multiwords.txt"),
        ("do-not-synthesize.txt", "do-not-synthesize.txt"),
    ]:
        if (resource_dir / name).exists():
            copyfile(resource_dir / name, tag_dir / dest)

    # segment.srx from the core jar
    srx = out_dir / "segment.srx"
    if not srx.exists():
        with ZipFile(lt_dir / "libs" / "languagetool-core.jar") as zf:
            srx.write_bytes(zf.read("org/languagetool/resource/segment.srx"))

    with open(out_dir / "common.txt", "w", encoding="utf-8") as f:
        write_freqlist(f, lang)

    # dictionaries
    output_dump = tag_dir / "output.dump"
    synth_dump = tag_dir / "synth.dump"
    source = DICT_SOURCES.get(lang)

    with TemporaryDirectory() as tmp:
        tmp = Path(tmp)
        if source is None:
            logging.warning("%s: no POS dictionary known; using empty dump", lang)
            output_dump.write_text("")
        else:
            if source[0] == "loose":
                extracted = extract_dict(None, source[1], resource_dir, tmp)
            else:
                jar_path = lt_dir / "libs" / source[1]
                # shared dictionaries (e.g. de-DE-x-simple uses German) live
                # under the donor language's resource dir
                res_dir = (
                    lt_dir / "org" / "languagetool" / "resource" / source[3]
                    if len(source) > 3
                    else resource_dir
                )
                extracted = extract_dict(jar_path, source[2], res_dir, tmp)

            if ".dict" in extracted and ".info" in extracted:
                if not output_dump.exists():
                    ok = dump_dictionary(java, classpath, extracted[".dict"], extracted[".info"], output_dump)
                    if not ok and not output_dump.exists():
                        output_dump.write_text("")
                synth_dict, synth_info = extracted.get(".dict"), extracted.get(".info")
                # synth dict is a separate {stem}_synth.dict
                synth_pair = {}
                for suffix in (".dict", ".info"):
                    name = f"{source[1] if source[0] == 'loose' else source[2]}_synth{suffix}"
                    loose = resource_dir / name
                    target = tmp / name
                    if loose.exists():
                        copyfile(loose, target)
                        synth_pair[suffix] = target
                    elif source[0] == "jar":
                        with ZipFile(lt_dir / "libs" / source[1]) as zf:
                            entry = f"org/languagetool/resource/{resource_dir.name}/{name}"
                            try:
                                target.write_bytes(zf.read(entry))
                                synth_pair[suffix] = target
                            except KeyError:
                                pass
                if ".dict" in synth_pair and ".info" in synth_pair and not synth_dump.exists():
                    ok = dump_dictionary(java, classpath, synth_pair[".dict"], synth_pair[".info"], synth_dump)
                    if ok:
                        # sort by key (column 1) so the FST can be built streaming;
                        # large dumps must not be loaded into memory at once
                        sh(
                            [
                                "sort",
                                "-t",
                                "\t",
                                "-k1,1",
                                "-o",
                                str(synth_dump),
                                str(synth_dump),
                            ],
                            env={**os.environ, "LC_ALL": "C"},
                        )
            else:
                output_dump.write_text("")

    if grammar.exists() and not merged_grammar:
        dist_grammar = dist_rules / "grammar.xml"
        canonicalize(dist_grammar if dist_grammar.exists() else grammar, grammar)

    # expand <phraseref idref="..."/> with the matching <phrase> body
    if grammar.exists():
        content = grammar.read_text(encoding="utf-8", errors="replace")
        m = re.search(r"<phrases>(.*?)</phrases>", content, re.S)
        if m:
            defs = dict(re.findall(r'<phrase id="([^"]+)">(.*?)</phrase>', m.group(1), re.S))
            content, n = re.subn(
                r'<phraseref idref="([^"]+)"\s*/>\s*|<phraseref idref="([^"]+)">\s*</phraseref>',
                lambda mm: defs.get(mm.group(1) or mm.group(2), "").strip(),
                content,
            )
            if n:
                grammar.write_text(content, encoding="utf-8")
                logging.info("%s: expanded %d phraserefs", lang, n)

    # convert LT SimpleReplaceRule data tables into XML rules
    if grammar.exists():
        tables = []
        if dist_rules.exists():
            for table in sorted(dist_rules.glob("replace*.txt")):
                tables.append((table.stem, table))
        if lang == "sr":
            for stem in ["replace-grammar", "replace-soft", "replace-style"]:
                target = out_dir / f"sr_{stem}.txt"
                fetch_github(
                    f"languagetool-language-modules/sr/src/main/resources/"
                    f"org/languagetool/rules/sr/ekavian/{stem}.txt",
                    target,
                )
                if target.exists():
                    tables.append((stem, target))

        extra = simple_replace_rules(tables, lang=lang)
        if extra:
            content = grammar.read_text(encoding="utf-8", errors="replace")
            content = content.replace("</rules>", extra + "\n</rules>")
            grammar.write_text(content, encoding="utf-8")
            logging.info("%s: appended simple-replace rules from %d tables", lang, len(tables))

    dist_disambig = resource_dir / "disambiguation.xml"
    canonicalize(
        dist_disambig if dist_disambig.exists() else disambig,
        disambig,
    )

    make_filter_data(lang, lt_dir, resource_dir, dist_rules, out_dir, java, classpath)

    (out_dir / "lang_code.txt").write_text(lang)
    logging.info("%s: done", lang)
    return True


def main():
    logging.basicConfig(level=logging.INFO, format="%(levelname)s %(message)s")
    parser = ArgumentParser()
    parser.add_argument("--lt_dir", type=Path, required=True)
    parser.add_argument("--out_root", type=Path, required=True)
    parser.add_argument("--java", default="java")
    parser.add_argument("--langs", nargs="*", default=None)
    parser.add_argument("--keep-going", action="store_true")
    args = parser.parse_args()

    lt_dir = args.lt_dir.resolve()
    out_root = args.out_root.resolve()
    out_root.mkdir(parents=True, exist_ok=True)

    classpath = os.pathsep.join([str(lt_dir / "libs" / "*")])

    all_langs = sorted(
        [p.name for p in (lt_dir / "org" / "languagetool" / "rules").iterdir() if p.is_dir()]
    )
    # languages that only exist in the LT source repo
    for extra in GITHUB_ONLY:
        if extra not in all_langs:
            all_langs.append(extra)
    all_langs.sort()

    langs = args.langs if args.langs else all_langs
    failures = []
    for lang in langs:
        try:
            make_build_dir(lang, lt_dir, out_root, args.java, classpath, args.keep_going)
        except Exception as e:  # noqa: BLE001
            logging.exception("%s failed: %s", lang, e)
            failures.append(lang)

    if failures:
        logging.error("failed languages: %s", failures)
    print(f"Done. {len(langs) - len(failures)}/{len(langs)} languages.")


if __name__ == "__main__":
    main()
