# nlprule

NLPRule is a library for rule-based grammatical error correction written in pure Rust with bindings for Python. Rules are sourced from [LanguageTool](https://github.com/languagetool-org/languagetool). 

```python
from nlprule import Tokenizer, Rules, SplitOn

tokenizer = Tokenizer.load("en")
rules = Rules.load("en", tokenizer, SplitOn([".", "?", "!"]))

rules.correct(
    "NLPRule can correct grammatical errors through a large number of rules ported from LanguageTool. "
    "Their are many rules which are too obscure for all intensive purposes, but the bulk of them cold be useful."
)
# returns: 
# 'NLPRule can correct grammatical errors through many rules ported from LanguageTool. '
# 'There are many rules which are too obscure for all intents and purposes, but the bulk of them could be useful.'
```

My goal with this library was creating a fast, lightweight engine to run natural language rules without having to rely on the JVM (and its speed implications) and without all the extra stuff LanguageTool does such as spellchecking, n-gram based error detection, etc.

NLPRule currently supports English and German.

|         | \|Disambiguation rules\| | \|Grammar rules\| | LT version   |
|---------|--------------------------|-------------------|--------------|
| English | 801 (100%)               | 3083 (~ 82%)      | 5.1          |
| German  | 464 (100%)               | 2526 (~ 81%)      | 5.1          |

## Usage

1. Install: `pip install nlprule`

<details><summary>2. Create a `tokenizer` and `rules` object</summary>
<p>

```python
from nlprule import Tokenizer, Rules

tokenizer = Tokenizer.load("en") # or 'de'
rules = Rules.load("en", tokenizer) # or 'de'
```

The objects will be downloaded the first time, then cached.

</p>
</details>

<details><summary>3a. Correct your text</summary>
<p>
    
```python
rules.correct_sentence("He wants that you send him an email.")
# returns: 'He wants you to send him an email.'
```

`correct_sentence` expects a single sentence as input. 

If you want to correct an arbitrary text, pass a `sentence_splitter` at initialization. A sentence splitter can be any function that takes a list of texts as input and returns a list of lists of sentences. A splitter that splits on fixed characters is included in NLPRule for convenience:

```python
from nlprule import SplitOn

rules = Rules.load("en", tokenizer, SplitOn([".", "?", "!"]))
```

Pro tip: You can use [NNSplit](https://github.com/bminixhofer/nnsplit) for more robust sentence segmentation:

```python
from nnsplit import NNSplit

splitter = NNSplit.load("en")
rules = Rules.load(
    "en",
    tokenizer,
    lambda texts: [[str(s) for s in text] for text in splitter.split(texts)],
)
```

If a sentence splitter is set, you can call `.correct`:

```python
rules.correct("He wants that you send him an email. She was not been here since Monday.")
# returns: 'He wants you to send him an email. She was not here since Monday.'
```

</p>
</details>

<details><summary>3b. Get suggestions</summary>
<p>
    

```python
suggestions = rules.suggest_sentence("She was not been here since Monday.")
for s in suggestions:
  print(s.start, s.end, s.text)
  
# prints:
# 4 16 ['was not', 'has not been']
```

`.suggest_sentence` also has a multi-sentence counterpart in `.suggest`.
    
</p>
</details>

<details><summary>Bonus: Analyze text with the `tokenizer`</summary>
<p>

NLPRule does rule + dictionary-based part-of-speech tagging and lemmatization as well as chunking with a model ported from [OpenNLP](https://opennlp.apache.org/). It's not as fancy as spaCy but could be faster and had to be done anyway to apply the rules so I thought I might as well add a public API:

```python
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
```

</p>
</details>

## Maintenance disclaimer

NLPRule is currently pretty bare bones in terms of API and documentation. I will definitely fix bugs, but adding new functionality (especially new languages) and improving API / docs will depend on interest by the community.

Fixing discrepancies between NLPRule and LanguageTool behaviour will have high priority if any are found.

## Acknowledgements

All credit for the rule content goes to [LanguageTool](https://github.com/languagetool-org/languagetool) who have made a Herculean effort to create high-quality grammar correction rules. This library is just a parser and reimplementation of the rule logic.

## License

NLPRule is licensed under the MIT license.
