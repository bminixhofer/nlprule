<h1 align='center'>
  nlprule
</h1>

<p align='center'>
    <a href="https://pypi.org/project/nlprule">
        <img src="https://img.shields.io/pypi/v/nlprule" alt="PyPI">
    </a>
    <a href="https://crates.io/crates/nlprule">
        <img src="https://img.shields.io/crates/v/nlprule" alt="Crates.io">
    </a>
    <a href="https://docs.rs/nlprule">
        <img src="https://docs.rs/nlprule/badge.svg" alt="Docs.rs">
    </a>
    <a href="https://pepy.tech/project/nlprule">
        <img src="https://pepy.tech/badge/nlprule/month" alt="PyPI Downloads">
    </a>
    <a href="">
        <img src="https://img.shields.io/crates/l/nlprule" alt="License">
    </a>
</p>

A fast, low-resource Natural Language Processing and Error Correction library written in Rust. nlprule implements a rule- and lookup-based approach to NLP using resources from [LanguageTool](github.com/languagetool-org/languagetool).

```python
from nlprule import Tokenizer, Rules

tokenizer = Tokenizer.load("en")
rules = Rules.load("en", tokenizer)

rules.correct("He wants that you send him an email.")
# returns: 'He wants you to send him an email.'

rules.correct("I can due his homework.")
# returns: 'I can do his homework.'

for s in rules.suggest("She was not been here since Monday."):
  print(s.start, s.end, s.replacements, s.source, s.message)
# prints:
# 4 16 ['was not', 'has not been'] WAS_BEEN.1 Did you mean was not or has not been?

for sentence in tokenizer.pipe("A brief example is shown."):
    for token in sentence:
        print(
            repr(token.text).ljust(10),
            repr(token.span).ljust(10),
            repr(token.tags).ljust(25),
            repr(token.lemmas).ljust(25),
            repr(token.chunks).ljust(25),
        )
# prints:
# ''         (0, 0)     ['SENT_START']           []                       []                      
# 'A'        (0, 1)     ['DT']                   ['A', 'a']               ['B-NP-singular']       
# 'brief'    (2, 7)     ['JJ']                   ['brief']                ['I-NP-singular']       
# 'example'  (8, 15)    ['NN:UN']                ['example']              ['E-NP-singular']       
# 'is'       (16, 18)   ['VBZ']                  ['be', 'is']             ['B-VP']                
# 'shown'    (19, 24)   ['VBN']                  ['show', 'shown']        ['I-VP']                
# '.'        (24, 25)   ['.', 'PCT', 'SENT_END'] ['.']                    ['O']

# and every call here takes less than 1ms! (on an i5 8600k)
```

## Main features

- Rule-based Grammatical Error Correction through multiple thousand rules.
- A text processing pipeline doing sentencization, part-of-speech tagging, lemmatization, chunking and disambiguation.
- Support for English, German and Spanish.
- Spellchecking. (*in progress*)

## Goals

- Fast, low-resource NLP suited for running:
    1. as a pre- / postprocessing step for more sophisticated (i. e. ML) approaches.
    2. in the background of another application with low overhead.
    3. client-side in the browser via WebAssembly. (*in progress*)
- 100% Rust code and dependencies. (*in progress*)

## non-Goals

- Being user-centric. nlprule is a library. It is not a correction tool on its own and does not aim to be.
- Creating and maintaining NLP resources. nlprule is a parser and executor of external resources mainly from [LanguageTool](github.com/languagetool-org/languagetool). nlprule does not aim to create and maintain such resources.

## Comparison to LanguageTool

|         | \|Disambiguation rules\| | \|Grammar rules\| | LT version   | nlprule time | LanguageTool time |
|---------|--------------------------|-------------------|--------------|--------------|-------------------|
| English | 843 (100%)               | 3725 (~ 85%)      | 5.2          | 1            | 1.7 - 2.0         |
| German  | 486 (100%)               | 2970 (~ 90%)      | 5.2          | 1            | 2.4 - 2.8         |
| Spanish | *Experimental support. Not fully tested yet.*

See the [benchmark issue](https://github.com/bminixhofer/nlprule/issues/6) for details.

## Acknowledgements

All credit for the resources used in nlprule goes to [LanguageTool](https://github.com/languagetool-org/languagetool) who have made a Herculean effort to create high-quality resources for Grammatical Error Correction and broader NLP.

## License

nlprule is licensed under the MIT license or Apache-2.0 license, at your option.

The nlprule binaries (`*.bin`) are derived from LanguageTool v5.2 and licensed under the LGPLv2.1 license. nlprule statically and dynamically links to these binaries. Under LGPLv2.1 §6(a) this does not have any implications on the license of nlprule itself.