# type: ignore

import nlprule

tokenizer = nlprule.Tokenizer.load("en")
rules = nlprule.Rules.load("en", tokenizer)

print(rules.correct("He wants that you send him an email."))
