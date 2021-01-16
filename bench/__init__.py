import argparse
import bz2
import time
from dataclasses import dataclass
from pathlib import Path
from typing import List, Set, Tuple

import language_tool_python
import nlprule
import requests
from tqdm.auto import tqdm

STORE_DIR = (Path(__file__) / ".." / "data").resolve()


@dataclass(eq=True, frozen=True)
class Suggestion:
    start: int
    end: int
    source: str
    replacements: Tuple[str]
    # message: str


# The ids in NLPRule have e.g ".4" to indicate the index in a rule group so we have to strip that
def strip_index(identifier):
    return identifier if "." not in identifier else identifier.split(".")[0]


class LanguageTool:
    def __init__(self, lang_code: str, ids: Set[str]):
        lt_code = {"en": "en_US", "de": "de_DE"}[lang_code]
        self.tool = language_tool_python.LanguageTool(
            lt_code, remote_server="http://localhost:8081/"
        )
        self.tool.disabled_rules = {
            "MORFOLOGIK_RULE_EN_US",
            "GERMAN_SPELLER_RULE",
            "COMMA_PARENTHESIS_WHITESPACE",
            "DOUBLE_PUNCTUATION",
            "UPPERCASE_SENTENCE_START",
            "WHITESPACE_RULE",
            "SENTENCE_WHITESPACE",
            "WHITESPACE_PARAGRAPH",
            "WHITESPACE_PARAGRAPH_BEGIN",
            "EMPTY_LINE",
            "TOO_LONG_SENTENCE",
            "TOO_LONG_PARAGRAPH",
            "PARAGRAPH_REPEAT_BEGINNING_RULE",
            "PUNCTUATION_PARAGRAPH_END",
            "PUNCTUATION_PARAGRAPH_END2",
            "EN_SPECIFIC_CASE",
            "EN_UNPAIRED_BRACKETS",
            "ENGLISH_WORD_REPEAT_RULE",
            "EN_A_VS_AN",
            "ENGLISH_WORD_REPEAT_BEGINNING_RULE",
            "EN_COMPOUNDS",
            "EN_CONTRACTION_SPELLING",
            "ENGLISH_WRONG_WORD_IN_CONTEXT",
            "EN_DASH_RULE",
            "EN_WORD_COHERENCY",
            "EN_DIACRITICS_REPLACE",
            "EN_PLAIN_ENGLISH_REPLACE",
            "EN_REDUNDANCY_REPLACE",
            "EN_SIMPLE_REPLACE",
            "READABILITY_RULE_SIMPLE",
            "READABILITY_RULE_DIFFICULT",
            "DE_SIMPLE_REPLACE",
            "OLD_SPELLING",
            "DE_SENTENCE_WHITESPACE",
            "DE_DOUBLE_PUNCTUATION",
            "MISSING_VERB",
            "GERMAN_WORD_REPEAT_RULE",
            "GERMAN_WORD_REPEAT_BEGINNING_RULE",
            "GERMAN_WRONG_WORD_IN_CONTEXT",
            "DE_AGREEMENT",
            "DE_AGREEMENT2",
            "DE_CASE",
            "DE_DASH",
            "DE_VERBAGREEMENT",
            "DE_SUBJECT_VERB_AGREEMENT",
            "DE_WORD_COHERENCY",
            "DE_SIMILAR_NAMES",
            "DE_WIEDER_VS_WIDER",
            "STYLE_REPEATED_WORD_RULE_DE",
            "DE_COMPOUND_COHERENCY",
            "TOO_LONG_SENTENCE_DE",
            "FILLER_WORDS_DE",
            "GERMAN_PARAGRAPH_REPEAT_BEGINNING_RULE",
            "DE_DU_UPPER_LOWER",
            "EINHEITEN_METRISCH",
            "COMMA_BEHIND_RELATIVE_CLAUSE",
            "COMMA_IN_FRONT_RELATIVE_CLAUSE",
            "READABILITY_RULE_SIMPLE_DE",
            "READABILITY_RULE_DIFFICULT_DE",
            "COMPOUND_INFINITIV_RULE",
            "STYLE_REPEATED_SHORT_SENTENCES",
            "STYLE_REPEATED_SENTENCE_BEGINNING",
        }

    def suggest(self, sentence: str) -> Set[Suggestion]:
        suggestions = {
            Suggestion(
                start=s.offset,
                end=s.offset + s.errorLength,
                source=s.ruleId,
                replacements=tuple(s.replacements),
                # there's subtle differences in messages (e. g. in quotation marks) for some reason
                # message=s.message,
            )
            for s in self.tool.check(sentence)
        }
        return suggestions


class NLPRule:
    def __init__(self, lang_code: str):
        self.tokenizer = nlprule.Tokenizer(f"storage/{lang_code}_tokenizer.bin")
        self.rules = nlprule.Rules(f"storage/{lang_code}_rules.bin", self.tokenizer)

    def suggest(self, sentence: str) -> Set[Suggestion]:
        suggestions = {
            Suggestion(
                start=s.start,
                end=s.end,
                source=strip_index(s.source),
                replacements=tuple(s.replacements),
                # message=s.message,
            )
            for s in self.rules.suggest_sentence(sentence)
        }
        return suggestions


def load_texts(lang_code: str) -> List[str]:
    tatoeba_code = {"en": "eng", "de": "deu"}[lang_code]

    base = "https://downloads.tatoeba.org/exports/per_language"
    url = f"{base}/{tatoeba_code}/{tatoeba_code}_sentences.tsv.bz2"

    path = STORE_DIR / Path(f"{tatoeba_code}_sentences.tsv.bz2")
    path.parents[0].mkdir(parents=True, exist_ok=True)

    if not path.exists():
        open(path, "wb").write(requests.get(url).content)

    texts = []
    for line in bz2.open(path):
        _id, _lang, text = line.decode("utf-8").strip().split("\t")
        texts.append(text)

    return texts


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--lang", choices={"de", "en"})
    parser.add_argument("--n_texts", default=10_000, type=int)

    args = parser.parse_args()

    texts = load_texts(args.lang)

    nlprule_instance = NLPRule(args.lang)
    lt_instance = LanguageTool(
        args.lang, {strip_index(rule.id) for rule in nlprule_instance.rules.rules}
    )

    same_suggestions = 0
    total_lt_suggestions = 0
    total_nlprule_suggestions = 0

    lt_time = 0.0
    nlprule_time = 0.0

    for i, text in enumerate(tqdm(texts[: args.n_texts])):
        start = time.time()
        lt_suggestions = lt_instance.suggest(text)
        lt_end = time.time()
        nlprule_suggestions = nlprule_instance.suggest(text)
        nlprule_end = time.time()

        same_suggestions += len(lt_suggestions & nlprule_suggestions)
        total_lt_suggestions += len(lt_suggestions)
        total_nlprule_suggestions += len(nlprule_suggestions)

        # skip the first 100 measurements to give the JVM time to warm up
        if i >= 100:
            lt_time += lt_end - start
            nlprule_time += nlprule_end - lt_end

    print(f"LanguageTool time: {lt_time:.3f}s")
    print(f"NLPRule time: {nlprule_time:.3f}s")
    print()
    print(f"n LanguageTool suggestions: {total_lt_suggestions}")
    print(f"n NLPRule suggestions: {total_nlprule_suggestions}")
    print(f"n same suggestions: {same_suggestions}")
