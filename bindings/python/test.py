import nlprule
import nnsplit

tokenizer = nlprule.Tokenizer.load("en")
splitter = nnsplit.NNSplit.load("en")
rules = nlprule.Rules.load("en", tokenizer, nlprule.SplitOn(["!", ".", "?"]),)

print(
    rules.correct(
        "He wants that you send him an email. He wants that you send him an email. "
    )
)

print(tokenizer.tokenize_sentence("This is an test.")[5].chunks)
