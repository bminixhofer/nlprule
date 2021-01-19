# nlprule

[![PyPI](https://img.shields.io/pypi/v/nlprule)](https://pypi.org/project/nlprule)
[![Crates.io](https://img.shields.io/crates/v/nlprule)](https://crates.io/crates/nlprule)
[![Docs.rs](https://docs.rs/nlprule/badge.svg)](https://docs.rs/nlprule)
![CI](https://github.com/bminixhofer/nlprule/workflows/CI/badge.svg)
[![Downloads](https://pepy.tech/badge/nlprule/month)](https://pepy.tech/project/nlprule)

NLPRule is a library for rule-based grammatical error correction written in pure Rust with bindings for Python. Rules are sourced from [LanguageTool](https://github.com/languagetool-org/languagetool). 

```python
from nlprule import Tokenizer, Rules, SplitOn

tokenizer = Tokenizer.load("en")
rules = Rules.load("en", tokenizer, SplitOn([".", "?", "!"]))

rules.correct("He wants that you send him an email.")
# returns: 'He wants you to send him an email.'

rules.correct("Thanks for your’s and Lucy’s help.")
# returns: 'Thanks for yours and Lucy’s help.'

rules.correct("I can due his homework.")
# returns: 'I can do his homework.'

suggestions = rules.suggest("She was not been here since Monday.")
for s in suggestions:
  print(s.start, s.end, s.replacements, s.source, s.message)
# prints:
# 4 16 ['was not', 'has not been'] WAS_BEEN.1 Did you mean was not or has not been?
```

My goal with this library was creating a fast, lightweight engine to run natural language rules without having to rely on the JVM (and its speed / memory implications) and without all the extra stuff LanguageTool does such as spellchecking, n-gram based error detection, etc.

NLPRule currently supports English and German.

|         | \|Disambiguation rules\| | \|Grammar rules\| | LT version   |
|---------|--------------------------|-------------------|--------------|
| English | 843 (100%)               | 3725 (~ 85%)      | 5.2          |
| German  | 486 (100%)               | 2970 (~ 90%)      | 5.2          |

NLPRule is focused on speed.

```python
In [1]: from nlprule import Tokenizer, Rules, SplitOn
   ...: 
   ...: tokenizer = Tokenizer.load("en")
   ...: rules = Rules.load("en", tokenizer, SplitOn([".", "?", "!"]))

In [2]: %timeit rules.correct("He wants that you send him an email.")
783 µs ± 6.18 µs per loop (mean ± std. dev. of 7 runs, 1000 loops each)
```

<sub>Using Intel(R) Core(TM) i5-8600K CPU @ 3.60GHz</sub>

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
  print(s.start, s.end, s.replacements, s.source, s.message)
# prints:
# 4 16 ['was not', 'has not been'] WAS_BEEN.1 Did you mean was not or has not been?
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

## Benchmark

NLPRule is approximately 1.7x - 2.8x faster than LanguageTool. See the [benchmark issue](https://github.com/bminixhofer/nlprule/issues/6) for details.

|         | NLPRule time | LanguageTool time |
|---------|--------------|-------------------|
| English | 1            | 1.7 - 2.0         | 
| German  | 1            | 2.4 - 2.8         |

## Maintenance disclaimer

NLPRule is currently pretty bare bones in terms of API and documentation. I will definitely fix bugs, but adding new functionality (especially new languages) and improving API / docs will depend on interest by the community.

Fixing discrepancies between NLPRule and LanguageTool behaviour will have high priority if any are found.

## Acknowledgements

All credit for the rule content goes to [LanguageTool](https://github.com/languagetool-org/languagetool) who have made a Herculean effort to create high-quality grammar correction rules. This library is just a parser and reimplementation of the rule logic.

## License

NLPRule is licensed under the MIT license or Apache-2.0 license, at your option.
