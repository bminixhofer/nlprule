# 0.5.0

## Breaking changes

- Changes the focus from `Vec<Token>` to `Sentence` (#54). `pipe` and `sentencize` return iterators over `Sentence` / `IncompleteSentence` now.
- Removes the special `SENT_START` token (now only used internally). Each token corresponds to at least one character in the input text now.
- Makes the fields of `Token` and `IncompleteToken` private and adds getter methods (#54).
- `char_span` and `byte_span` are replaced by a `Span` struct which keeps track of char and byte indices at the same time (#54). To e.g. get the byte range, use `token.span().byte()`.
- Spans are relative to the input text now, not anymore to sentence boundaries (#53, thanks @drahnr!).

## New features

- The regex backend can now be chosen from Oniguruma or fancy-regex with the features `regex-onig` and `regex-fancy`. `regex-onig` is the default.
- nlprule now compiles to WebAssembly. WebAssembly support is guaranteed for future versions and tested in CI.
- A new selector API to select individual rules (details documented in `nlprule::rule::id`). For example:

```rust
use nlprule::{Tokenizer, Rules, rule::id::Category};
use std::convert::TryInto;

let mut rules = Rules::new("path/to/en_rules.bin")?;

// disable rules named "confusion_due_do" in category "confused_words"
rules
   .select_mut(
       &Category::new("confused_words")
           .join("confusion_due_do")
           .into(),
   )
   .for_each(|rule| rule.disable());

// disable all grammar rules
rules
   .select_mut(&Category::new("grammar").into())
   .for_each(|rule| rule.disable());

// a string syntax where slashes are the separator is also supported
rules
    .select_mut(&"confused_words/confusion_due_do".try_into()?)
   .for_each(|rule| rule.enable());
```

# 0.4.6

## Breaking changes

- `.validate()` in `nlprule-build` now returns a `Result<()>` to encourage calling it after `.postprocess()`.

## Fixes

- Fixes an error where `Cursor` position in `nlprule-build` was not reset appropriately.
- Use `fs_err` everywhere for better error messages.

# 0.4.5

## New features
- A `transform` function in `nlprule-build` to transform binaries immediately after acquiring them. Suited for e. g. compressing the binaries before caching them.

## Fixes
- Require `srx=^0.1.2` to include a patch for out of bounds access.

# 0.4.4

## Breaking changes

This is a patch release but there are some small breaking changes to the public API:
- `from_reader` and `new` methods of the `Tokenizer` and `Rules` now return an `nlprule::Error` instead of `bincode:Error`.
- `tag_store` and `word_store` methods of the `Tagger` are now private.

## New features

- The `nlprule-build` crate now has a `postprocess` method to allow e.g. compression of the produced binaries (#32, thanks @drahnr!).

## Internal improvements

- Newtypes for `PosIdInt` and `WordIdInt` to clarify use of ids in the tagger (#31).
- Newtype for indices into the match graph (`GraphId`). All graph ids are validated at build-time now (also fixed an error where invalid graph ids in the XML files were ignored through this) (#31).
- Reduced size of the English tokenizer through better serialization of the chunker. From 15MB (7.7MB gzipped) to 11MB (6.9MB gzipped).
- Reduce allocations through making more use of iterators internally (#30). Improves speed but there is no significant benchmark improvement on my machine.
- Improve error handling by propagating more errors in the `compile` module instead of panicking and better build-time validation. Reduces `unwrap`s from ~80 to ~40.

# 0.4.3

## Breaking changes

- `nlprule` does sentence segmentation internally now using [srx](https://github.com/bminixhofer/srx). The Python API has changed, removing the `SplitOn` class and the `*_sentence` methods:

```python
tokenizer = Tokenizer.load("en")
rules = Rules.load("en", tokenizer)

rules.correct("He wants that you send him an email.") # this takes an arbitrary text
```

- `new_from` is now called `from_reader` in the Rust API (thanks @drahnr!)
- `Token.text` and `IncompleteToken.text` are now called `Token.sentence` / `IncompleteToken.sentence` to avoid confusion with `Token.word.text`.
- `Tokenizer.tokenize` is now private. Use `Tokenizer.pipe` instead (also does sentence segmentation).

## New features
- Support for Spanish (*experimental*).
- A new multiword tagger improves tagging of e. g. named entities for English and Spanish.
- Adds the `nlprule-build` crate which makes using the correct binaries in Rust easier (thanks @drahnr for the suggestion and discussion!)
- Scripts and docs in `build/README.md` to make creating the nlprule build directories easier and more reproducible.
- Full support for LanguageTool *unifications*.
- Binary size of the `Tokenizer` improved a lot. Now roughly x6 smaller for German and x2 smaller for English.
- New iterator helpers for `Rules` (thanks @drahnr!)
- A method `.sentencize` on the `Tokenizer` which does only sentence segmentation and nothing else.

# 0.3.0

__BREAKING: `suggestion.text` is now more accurately called `suggestion.replacements`__

- Lots of speed improvements: NLPRule is now roughly 2.5x to 5x faster for German and English, respectively.

- Rules have more information in the public API now: See #5 

# 0.2.2

- Python 3.9 support (fixes #7)

# 0.2.1

- Fix precedence of Rule IDs over Rule Group IDs.

# 0.2.0

- Updated to LT version 5.2.
- Suggestions now have a `message` and `source` attribute (#5):

```python
suggestions = rules.suggest_sentence("She was not been here since Monday.")
for s in suggestions:
  print(s.start, s.end, s.text, s.source, s.message)

# prints:
# 4 16 ['was not', 'has not been'] WAS_BEEN.1 Did you mean was not or has not been?
```

- NLPRule is parallelized by default now. Parallelism can be turned off by setting the `NLPRULE_PARALLELISM` environment variable to false.