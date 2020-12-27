# type: ignore

import nlprule

tokenizer = nlprule.Tokenizer("../../tokenizer.bin")
rules = nlprule.Rules("../../rules.bin", tokenizer)

print(rules.correct("He wants that you send him an email."))
