# type: ignore

import timeit

from nlprule import Rules, SplitOn, Tokenizer

tokenizer = Tokenizer("../../storage/en/tokenizer.bin", SplitOn([".", "?", "!"]))
rules = Rules("../../storage/en/rules.bin", tokenizer, SplitOn([".", "?", "!"]))


def foo():
    tokenizer.tokenize(
        "NLPRule can correct grammatical errors through a large number of rules ported from LanguageTool. "
        "Their are many rules which are too obscure for all intensive purposes, but the bulk of them cold be useful."
    )


def foo2():
    rules.correct(
        "NLPRule can correct grammatical errors through a large number of rules ported from LanguageTool. "
        "Their are many rules which are too obscure for all intensive purposes, but the bulk of them cold be useful."
    )


print(timeit.timeit("foo2()", setup="from __main__ import foo, foo2", number=100))


# %timeit rules.correct(
#     "NLPRule can correct grammatical errors through a large number of rules ported from LanguageTool. "
#     "Their are many rules which are too obscure for all intensive purposes, but the bulk of them cold be useful."
