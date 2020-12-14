# type: ignore

import pynlprule

tagger = pynlprule.Tagger(
    ["../../data/dumps/de/output.dump", "../../data/dumps/de/output.dump"],
    ["../../data/dumps/de/added.txt"],
)
tokenizer = pynlprule.Tokenizer("../../data/disambiguation.de.canonic.xml", tagger)
tokens = tokenizer.apply("Das ist ein Test.")

print(tokens)
print([x.text for x in tokens])
