# type: ignore

import nlprule

tokenizer = nlprule.Tokenizer.load("en")
rules = nlprule.Rules.load("en", tokenizer, nlprule.SplitOn([".", "?", "!"]))

print(
    rules.correct(
        ["He wants that you send him an email. He wants that you send him an email."]
    )
)
