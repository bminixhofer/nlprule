# type: ignore

import nlprule

tokenizer = nlprule.Tokenizer.load("en")
rules = nlprule.Rules.load("en", tokenizer, nlprule.SplitOn([".", "?", "!"]))

tokens = tokenizer.tokenize_sentence("She was not been here since Monday.")
for token in tokens:
    print(token.text, token.span, token.tags, token.lemmas, token.chunks)
# prints:
#  (0, 0) ['SENT_START'] [] []
# She (0, 3) ['PRP'] ['She', 'she'] ['B-NP-singular', 'E-NP-singular']
# was (4, 7) ['VBD'] ['be', 'was'] ['B-VP']
# not (8, 11) ['RB'] ['not'] ['I-VP']
# been (12, 16) ['VBN'] ['be', 'been'] ['I-VP']
# here (17, 21) ['RB'] ['here'] ['B-ADVP']
# since (22, 27) ['CC', 'IN', 'RB'] ['since'] ['B-PP']
# Monday (28, 34) ['NNP'] ['Monday'] ['B-NP-singular', 'E-NP-singular']
# . (34, 35) ['.', 'PCT', 'SENT_END'] ['.'] ['O']
