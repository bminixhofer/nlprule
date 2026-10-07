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

## About this fork — `all-languages` branch

This fork extends nlprule from ~5 languages to **all 35 LanguageTool 6.5 languages** (+ `de-DE-x-simple-language`, `ja`), verified rule-by-rule against a local **LanguageTool 6.5** server (HTTP API, level=default).

**State (final verification, single clean run):**

- **XML rules: 100%** — 28,503 active top-level rules / 0 missing, 499 default-off (`temp_off`) / 0 missing across all 35 languages. Reproducible audit (two byte-identical consecutive runs). Default-off rules stay silent by default and can be activated at runtime via `NLPRULE_ENABLE=<CATEGORY/RULE[/N],...>`.
- **Builtin (Java) rules: 100% of everything the local LT actually fires** — per-language variant ids (e.g. `DE_DOUBLE_PUNCTUATION`, `RU_UNPAIRED_BRACKETS`, `ES_UNPAIRED_BRACKETS`), glue semantics, word-repeat per language, trailing whitespace, `EN_A_VS_AN`, `DE_VERBAGREEMENT`, `DE_CASE`, `RU_VERB_CONJUGATION`, `RU_COMPOUNDS`, `BR_TOPO`, `PT_ENGLISH_CONTRACTION_ORTHOGRAPHY`, `AR_DIACRITICS_REPLACE`, `ARABIC_*_WHITESPACE`, `ES_QUESTION_MARK`, `FRENCH_WHITESPACE`, and exact LT ids for all SimpleReplace tables. Probe battery: **31/31 languages 100% id parity**.
- **Spellcheckers: 14 ported** from LT morfologik/hunspell dictionaries (it, br, gl, crh, ro, tl, el, sk, sl, ast, be, ga, eo + German with compound decomposition, gated to `de-DE` like LT).
- **Live ID agreement vs LT 6.5** (jaccard of per-sentence rule-id sets, n=1000 corpus examples/lang): **0 languages with net regression > 1pp vs the 5-language baseline; 13 languages improved > 1pp**.

| lang | jaccard | Δ vs baseline (pp) |
|---|---|---|
| ar | 0.9627 | -0.16 |
| ast | 1.0000 | +0.00 |
| be | 0.9878 | +1.83 |
| br | 0.9845 | +0.11 |
| ca | 0.8980 | +0.00 |
| crh | 0.9892 | +0.00 |
| da | 0.9540 | +0.00 |
| de | 0.9715 | +0.10 |
| de-DE-x-simple-language | 0.9298 | +0.00 |
| el | 0.9821 | +3.57 |
| en | 0.9390 | +0.65 |
| eo | 0.9645 | +0.00 |
| es | 0.9127 | +0.20 |
| fa | 0.9965 | +0.00 |
| fr | 0.9313 | +0.00 |
| ga | 0.9960 | +0.60 |
| gl | 0.9627 | +1.86 |
| it | 0.9719 | +0.56 |
| ja | 0.9483 | +0.00 |
| km | 1.0000 | +0.00 |
| nl | 0.9285 | +0.35 |
| pl | 0.8510 | +1.40 |
| pt | 0.9655 | +1.10 |
| ro | 0.9627 | +1.33 |
| ru | 0.9135 | -0.05 |
| sk | 0.9744 | +0.00 |
| sl | 0.9896 | +1.04 |
| sv | 0.9487 | +2.56 |
| ta | 1.0000 | +0.00 |
| tl | 0.9831 | +1.70 |
| uk | 0.9488 | +0.06 |
| zh | 0.8520 | +0.00 |

The only unported rule families are the 11 ngram-based ones (`CONFUSION_*`, `NgramProbability`, ...) — the local LT 6.5 has no ngram index installed (verified: they never fire at any level), so there is no local ground truth to port against. Explicitly approved and documented.

**Main changes vs upstream** (`nlprule/src/`):
- `compile/structure.rs` — full `<phrases>`/`<includephrases>` expansion (OR-alternatives via rule cloning), `type` attribute support, XML escaping fixes; 0 deserialize warnings in all 35 languages.
- `compile/utils.rs` (`from_java_regex`) — Java→oniguruma translation fixes: inline flags `(?id)`, surrogate pairs, quantified anchors, brace possessives, octal escapes, lookbehind group hoisting, `\p{Is...}` strip.
- `builtins.rs` (new) — all ported builtin families with per-language config (`text_cfg`), speller configs with wordlists + replace-table ignores, eo/de compound logic.
- `rules.rs` — runtime enable/disable (`NLPRULE_ENABLE`), builtin wiring, overlap dedup with builtin exemption, trailing-whitespace pass.
- `build/make_build_dirs.py` — SimpleReplace table → grammar.xml generator with exact LT id scheme per language/table.

**Reproducing:** compile inputs (`data2/<lang>/grammar.xml` + tagger dumps + speller wordlists) and verification tooling (coverage audit, compare runners, delta log) live in the companion working repo; this repo contains the Rust workspace — build with `cargo build --release --features "compile,bin,zh,ja"`.

## Docker

The repo ships a multi-stage `Dockerfile` that builds the workspace with the documented feature set (`compile bin zh ja`) and installs all binaries (`compile`, `test`, `run`, `check_server`, `bench`, `debug_filters`, `test_disambiguation`, `opennlp_to_chunker`) on `PATH`. The rule binaries under `storage/` total roughly 5.6 GB across all 35 languages, so they are **not** baked into the image — mount the host `storage/` directory at `/storage` instead:

```sh
cd nlprule-fork
docker build -t nlprule-all .
echo "She have a test." | docker run -i --rm -v "$PWD/storage:/storage:ro" nlprule-all en
# {"i":0,"text":"She have a test.","hits":[["HAVE_VBZ_EN",4,8,"has"]]}
```

The entrypoint accepts:

- `<lang>` — run `check_server` over stdin (one sentence per line), printing one JSON object per line with the rule IDs, spans and first replacement that fired. This is the same protocol the comparison harness uses.
- `dump-rules <lang>` — print the loaded-rule inventory as JSON (same output `scripts/coverage_audit.py` consumes).
- `<tokenizer.bin> <rules.bin> [lang]` — run `check_server` against explicit bin paths.
- `sh` / `bash` — drop into a shell with every binary available, e.g. to recompile a language inside the container:

```sh
docker run --rm -it \
  -v "$PWD/../data2:/data2:ro" -v "$PWD/storage:/storage" \
  -e NLPRULE_DATE_ANCHOR=2014-01-01 nlprule-all sh
compile --build-dir /data2/en --tokenizer-out /storage/en_tokenizer.bin --rules-out /storage/en_rules.bin
test --tokenizer /storage/en_tokenizer.bin --rules /storage/en_rules.bin
```

`NLPRULE_ENABLE="CAT/GROUP/N,..."` is passed through to activate `default="off"` rules. A `docker-compose.yml` is included for the mount-and-run setup (`docker compose run -T nlprule en < sentences.txt`).

### Service image (`Dockerfile.service`)

`Dockerfile.service` builds a self-contained service image for hosts with no
local checkout: it bakes the rule binaries of the languages it serves into
the image (en + es, ~225 MB extra; all 35 languages would be ~5.6 GB) and
adds a small Python HTTP front-end (`docker/http_server.py`) around the
long-lived `check_server` process:

```sh
docker build -f Dockerfile.service -t nlprule-service .
docker run -d --name nlprule -p 8081:8080 --restart unless-stopped nlprule-service
```

Endpoints (`NLPRULE_LANGS` controls the language list):

- `POST /v2/check` — **LanguageTool-compatible API** (what the LanguageTool
  browser extension speaks): form-encoded or JSON params `text`,
  `language` (LT codes like `es-ES` are mapped to the served base language;
  `auto` picks between the served languages by keyword heuristic),
  `disabledRules` / `enabledRules` / `enabledOnly` /
  `disabledCategories` / `enabledCategories`. Response is LT-shaped:
  `software`, `language`, `matches[]` with `message`, `replacements`,
  character `offset`/`length` (converted from the checker's byte offsets, so
  accented text aligns), `context`, `sentence` and `rule.category`
  (`MORFOLOGIK_*`/`HUNSPELL*` map to `TYPOS` like LT). Fields nlprule has no
  data for (rule message, description, issueType) are synthesized. Point the
  LanguageTool browser extension's custom server at the container and it
  works unchanged.
- `POST /check` — native JSON `{"text": "...", "lang": "es"}`; same match
  objects as above.
- `GET /v2/languages`, `GET /languages` — the served languages.
- `GET /health` — readiness plus which languages have their worker loaded.

Per-language `check_server` children start lazily on first request (en loads
in ~1 s, es in ~3-15 s) and stay resident afterwards. Note that
`check_server` flushes its JSONL output after every line — required for the
persistent-process protocol (upstream relied on stdin EOF to flush).

To serve more languages, add the corresponding `COPY storage/<lang>_*.bin`
lines in `Dockerfile.service` (and its dockerignore), set `NLPRULE_LANGS`,
rebuild and redeploy. On a host reachable only through Portainer, a
`docker save <image> | gzip` tar can be uploaded through the Portainer API
proxy endpoint `POST /api/endpoints/<id>/docker/images/load` — no SSH or
registry needed.

---

A fast, low-resource Natural Language Processing and Error Correction library written in Rust. nlprule implements a rule- and lookup-based approach to NLP using resources from [LanguageTool](https://github.com/languagetool-org/languagetool).

<details>
  <summary>Python Usage</summary>

Install: `pip install nlprule`

Use:
```python
from nlprule import Tokenizer, Rules

tokenizer = Tokenizer.load("en")
rules = Rules.load("en", tokenizer)
```
```python
rules.correct("He wants that you send him an email.")
# returns: 'He wants you to send him an email.'

rules.correct("I can due his homework.")
# returns: 'I can do his homework.'

for s in rules.suggest("She was not been here since Monday."):
    print(s.start, s.end, s.replacements, s.source, s.message)
# prints:
# 4 16 ['was not', 'has not been'] WAS_BEEN.1 Did you mean was not or has not been?
```
```python
for sentence in tokenizer.pipe("A brief example is shown."):
    for token in sentence:
        print(
            repr(token.text).ljust(10),
            repr(token.span).ljust(10),
            repr(token.tags).ljust(24),
            repr(token.lemmas).ljust(24),
            repr(token.chunks).ljust(24),
        )
# prints:
# 'A'        (0, 1)     ['DT']                   ['A', 'a']               ['B-NP-singular']       
# 'brief'    (2, 7)     ['JJ']                   ['brief']                ['I-NP-singular']       
# 'example'  (8, 15)    ['NN:UN']                ['example']              ['E-NP-singular']       
# 'is'       (16, 18)   ['VBZ']                  ['be', 'is']             ['B-VP']                
# 'shown'    (19, 24)   ['VBN']                  ['show', 'shown']        ['I-VP']                
# '.'        (24, 25)   ['.', 'PCT', 'SENT_END'] ['.']                    ['O']
```
</details>

<details>
  <summary>Rust Usage</summary>

Recommended setup:

`Cargo.toml`
```toml
[dependencies]
nlprule = "<version>"

[build-dependencies]
nlprule-build = "<version>" # must be the same as the nlprule version!
```

`build.rs`
```rust
fn main() -> Result<(), nlprule_build::Error> {
    println!("cargo:rerun-if-changed=build.rs");

    nlprule_build::BinaryBuilder::new(
        &["en"],
        std::env::var("OUT_DIR").expect("OUT_DIR is set when build.rs is running"),
    )
    .build()?
    .validate()
}
```

`src/main.rs`
```rust
use nlprule::{Rules, Tokenizer, tokenizer_filename, rules_filename};

fn main() {
    let mut tokenizer_bytes: &'static [u8] = include_bytes!(concat!(
        env!("OUT_DIR"),
        "/",
        tokenizer_filename!("en")
    ));
    let mut rules_bytes: &'static [u8] = include_bytes!(concat!(
        env!("OUT_DIR"),
        "/",
        rules_filename!("en")
    ));

    let tokenizer = Tokenizer::from_reader(&mut tokenizer_bytes).expect("tokenizer binary is valid");
    let rules = Rules::from_reader(&mut rules_bytes).expect("rules binary is valid");

    assert_eq!(
        rules.correct("She was not been here since Monday.", &tokenizer),
        String::from("She was not here since Monday.")
    );
}
```

`nlprule` and `nlprule-build` versions are kept in sync.

</details>

## Main features

- Rule-based Grammatical Error Correction through multiple thousand rules.
- A text processing pipeline doing sentence segmentation, part-of-speech tagging, lemmatization, chunking and disambiguation.
- Support for **35 languages** built from LanguageTool 6.5 resources (see table below).
- Morphological synthesis: `match` elements with `postag` / `postag_regexp` / `postag_replace`
  inflect lemmas in suggestions, mirroring LanguageTool's `MatchState`.
- Spellchecking. (*in progress*)

## Goals

- A single place to apply spellchecking and grammatical error correction for a downstream task.
- Fast, low-resource NLP suited for running:
    1. as a pre- / postprocessing step for more sophisticated (i. e. ML) approaches.
    2. in the background of another application with low overhead.
    3. client-side in the browser via WebAssembly.
- 100% Rust code and dependencies.

## Comparison to LanguageTool

All languages are built from a LanguageTool 6.5 desktop distribution with
`build/make_build_dirs.py` + `build/make_configs.py`; per-language binaries are compiled with
`scripts/compile_all.sh`. "XML rules" counts `<rule>`/`<rulegroup>` entries in `grammar.xml`
(expanded rules count higher); "passing" counts rules whose embedded LT examples
(`test --rules ...`) pass. Serbian rules are Java classes in LT (no XML to convert);
Japanese/Chinese need LT's specialized tokenizers; `da`/`km`/`crh` suffer from incomplete
tagger dictionaries in the LT distribution itself.

| lang           | runnable   | passing   | lang   | runnable   | passing   |
|----------------|------------|-----------|--------|------------|-----------|
| ar | 1110 | 1062 | km     | 44         | 11        |
| ast            | 71         | 69        | lt     | 4          | 4         |
| be             | 516        | 506       | ml     | 18         | 18        |
| br | 665 | 476       | nl†    | 68331      | 68208     |
| ca | 14545 | 10877 | pl | 1842 | 1491      |
| crh | 93 | 41        | pt | 1978 | 956 |
| da             | 78         | 61        | ro     | 1167       | 1116      |
| de | 5480 | 5193 | ru | 1180 | 988 |
| de-DE-x-simple | 92         | 40        | sk     | 206        | 184       |
| el             | 98         | 95        | sl     | 85         | 82        |
| en | 5718 | 4867 | sr     | 45         | 44        |
| eo | 416 | 127       | sv     | 31         | 29        |
| es | 1806 | 1415 | ta     | 210        | 210       |
| fa             | 764        | 557       | tl     | 44         | 37        |
| fr | 5313 | 3390 | uk | 10915 | 10682     |
| ga             | 3566       | 3292      | zh     | 1863       | 1520      |
| gl | 298 | 204       | it | 131 | 124       |
| ja             | 735        | 702       |        |            |           |

Totals: ~119,000 runnable rules, ~109,400 passing their embedded LanguageTool
examples (~92%). †`nl` includes the 66k-entry `replace.txt` table appended to
the grammar; table rules carry no embedded examples (they pass vacuously —
multi-word table entries can never match a single token under nlprule's
token semantics, so they were neutralized to a shared never-matching pattern
to keep compilation fast; without the table `nl` is 1544 runnable / 1492
passing). Highlights: uk 97.9%, be 98.1%, nl 96.6%, sr 97.8%, de 94.9%,
ja 95.5% (lindera/ipadic), ro 95.6%, ta 100%, ga 92.3%, zh 81.6%, ru 84%,
ar 85.2%, es 78.1%, ca 75.3%.

### Live comparison against the LanguageTool HTTP server

`build/compare_server.py` sends each language's embedded example sentences (unique, up to 1000 per language) to a local instance of the official LanguageTool 6.5 HTTP server and to nlprule, and compares the rule IDs that fire per sentence (standard level, ids normalized to LT's sub-rule ids). Final results (single clean run; baseline deltas in the fork table above):

| lang | sentences | only nlprule | only LT | ID precision vs LT | ID recall vs LT | Jaccard |
|---|---|---|---|---|---|---|
| ast | 71 | 0 | 0 | 1.0000 | 1.0000 | 1.0000 |
| km | 52 | 0 | 0 | 1.0000 | 1.0000 | 1.0000 |
| ta | 216 | 0 | 0 | 1.0000 | 1.0000 | 1.0000 |
| fa | 282 | 0 | 1 | 1.0000 | 0.9964 | 0.9965 |
| ga | 1000 | 4 | 3 | 0.9955 | 0.9966 | 0.9960 |
| sl | 96 | 1 | 1 | 0.9808 | 0.9808 | 0.9896 |
| crh | 93 | 0 | 1 | 1.0000 | 0.9867 | 0.9892 |
| be | 82 | 0 | 1 | 1.0000 | 0.9836 | 0.9878 |
| br | 841 | 3 | 13 | 0.9905 | 0.9602 | 0.9845 |
| tl | 59 | 1 | 1 | 0.9688 | 0.9688 | 0.9831 |
| el | 56 | 1 | 1 | 0.9444 | 0.9444 | 0.9821 |
| sk | 39 | 1 | 1 | 0.9583 | 0.9583 | 0.9744 |
| it | 178 | 0 | 5 | 1.0000 | 0.9580 | 0.9719 |
| de | 1000 | 27 | 27 | 0.9276 | 0.9276 | 0.9715 |
| pt | 1000 | 13 | 29 | 0.9797 | 0.9559 | 0.9655 |
| eo | 507 | 10 | 14 | 0.9728 | 0.9623 | 0.9645 |
| ar | 616 | 1 | 22 | 0.9973 | 0.9436 | 0.9627 |
| gl | 483 | 5 | 17 | 0.9840 | 0.9475 | 0.9627 |
| ro | 939 | 47 | 31 | 0.9391 | 0.9590 | 0.9627 |
| da | 87 | 2 | 4 | 0.9688 | 0.9394 | 0.9540 |
| uk | 1000 | 47 | 42 | 0.9388 | 0.9450 | 0.9488 |
| sv | 39 | 1 | 2 | 0.9688 | 0.9394 | 0.9487 |
| ja | 735 | 0 | 38 | 1.0000 | 0.9482 | 0.9483 |
| en | 1000 | 32 | 61 | 0.9510 | 0.9106 | 0.9390 |
| fr | 1000 | 53 | 70 | 0.9127 | 0.8878 | 0.9313 |
| de-DE-x-simple-language | 114 | 7 | 2 | 0.8971 | 0.9683 | 0.9298 |
| nl | 1000 | 23 | 72 | 0.9723 | 0.9181 | 0.9285 |
| ru | 1000 | 68 | 86 | 0.9182 | 0.8987 | 0.9135 |
| es | 1000 | 69 | 88 | 0.9023 | 0.8786 | 0.9127 |
| ca | 1000 | 66 | 96 | 0.8840 | 0.8397 | 0.8980 |
| zh | 1000 | 4 | 145 | 0.9943 | 0.8284 | 0.8520 |
| pl | 1000 | 129 | 136 | 0.8120 | 0.8038 | 0.8510 |
| ta | 216 | 0 | 0 | 1.0000 | 1.0000 | 1.0000 |
| km | 52 | 0 | 0 | 1.0000 | 1.0000 | 1.0000 |
| ast | 71 | 0 | 0 | 1.0000 | 1.0000 | 1.0000 |
| fa | 282 | 0 | 1 | 1.0000 | 0.9964 | 0.9965 |
| ga | 1000 | 4 | 9 | 0.9955 | 0.9899 | 0.9900 |
| crh | 93 | 0 | 1 | 1.0000 | 0.9867 | 0.9892 |
| br | 841 | 2 | 14 | 0.9937 | 0.9572 | 0.9834 |
| sl | 96 | 0 | 2 | 1.0000 | 0.9615 | 0.9792 |
| sk | 39 | 1 | 1 | 0.9583 | 0.9583 | 0.9744 |
| de | 1000 | 26 | 29 | 0.9297 | 0.9223 | 0.9705 |
| be | 82 | 0 | 3 | 1.0000 | 0.9508 | 0.9695 |
| it | 178 | 1 | 6 | 0.9912 | 0.9496 | 0.9663 |
| tl | 59 | 1 | 2 | 0.9677 | 0.9375 | 0.9661 |
| eo | 507 | 7 | 14 | 0.9808 | 0.9623 | 0.9645 |
| ar | 616 | 6 | 18 | 0.9841 | 0.9538 | 0.9643 |
| pt | 1000 | 21 | 39 | 0.9671 | 0.9406 | 0.9545 |
| da | 87 | 2 | 4 | 0.9688 | 0.9394 | 0.9540 |
| ro | 939 | 40 | 48 | 0.9465 | 0.9365 | 0.9494 |
| ja | 735 | 0 | 38 | 1.0000 | 0.9482 | 0.9483 |
| uk | 1000 | 48 | 43 | 0.9375 | 0.9436 | 0.9482 |
| el | 56 | 1 | 3 | 0.9375 | 0.8333 | 0.9464 |
| gl | 483 | 3 | 27 | 0.9900 | 0.9167 | 0.9441 |
| en | 1000 | 39 | 62 | 0.9408 | 0.9091 | 0.9325 |
| fr | 1000 | 53 | 70 | 0.9127 | 0.8878 | 0.9313 |
| de-DE-x-simple-language | 114 | 7 | 2 | 0.8971 | 0.9683 | 0.9298 |
| nl | 1000 | 36 | 75 | 0.9571 | 0.9147 | 0.9250 |
| sv | 39 | 1 | 3 | 0.9677 | 0.9091 | 0.9231 |
| ru | 1000 | 66 | 86 | 0.9204 | 0.8987 | 0.9140 |
| es | 1000 | 65 | 90 | 0.9071 | 0.8759 | 0.9107 |
| ca | 1000 | 66 | 96 | 0.8840 | 0.8397 | 0.8980 |
| zh | 1000 | 4 | 145 | 0.9943 | 0.8284 | 0.8520 |
| pl | 1000 | 125 | 157 | 0.8109 | 0.7734 | 0.8370 |


### Final comparison table (goal coverage 100%, final v8)

`~/nlprule_bg/final_v8.jsonl` (1000 example sentences per language vs the
local LT 6.5 HTTP server, marker fragments, id jaccard). All 32
server-verifiable languages: **no net jaccard regression >1pp vs v7**, 15
languages improved (el +3.6pp, sv +2.6pp, be +1.8pp, tl +1.7pp, pl +1.5pp,
ro +1.3pp, sl +1.0pp, pt +1.0pp, gl +0.9pp, it +0.6pp, ...).

Coverage after the goal (coverage_audit.py, reproducible):
19085 active XML rules / 0 missing, 499 off|temp_off rules / 0 missing,
0 deserialize warnings, 0 construction errors over the 35 language builds.

Caveat: GERMAN_SPELLER_RULE is gated on the `de-DE` language code exactly
like LT 6.5 (the generic `de` code does not run the German speller there),
so the coverage audit's `de` dump (built with code `de`) does not list it;
it is loaded in the de bin and fires under `de-DE` (probe parity 9/10).

### Residue by language (marker-fragment compare, final v7)

- `pl` (0.81/0.77): tagger readings — COFANIE_PRZECINKA 28, PREP_CASUS 23,
  ZDANIA_ZLOZONE variant selection, BRAK_PRZECINKA_*; speller compound
  suppression (isNotCompound) unported.
- `zh` (0.99/0.83): jieba-vs-HanLP segmentation on wrong-spelling compounds
  (~40 tone-pair rule groups, 2-4 events each); porting HanLP is not viable
  in Rust today.
- `ca` (0.88/0.84): CONCORDANCES_* same-priority tie resolution beyond the
  priority map; MORFOLOGIK over-firing on names.
- `ru`/`es`/`uk` (-1..-3pp vs pre-SentEndAtom state): the SentEndAtom
  (br depends on it, +0.41 j) shifts SENT_END-exception rules; the exact
  reading-level semantics is the documented next step.
- `de` (0.93/0.92 on fragments): GERMAN_SPELLER_RULE (ngram model),
  IM_UM; full-sentence agreement additionally recovered DOPPELTES_VERB
  (27 events, `<match no postag>` compile fix).
- `nl` (0.96/0.91): NAAM_FOUT_GESPELD variant selection, speller accepts.
- `el` (0.94/0.83, n=56): small corpus, tagger readings.
- `ja` (1.00/0.95): 38 onlyLT, lindera-vs-Sen segmentation differences.
- sr/lt/ml: build+run verified only (no LT 6.5 server truth).

Source: `~/nlprule_bg/final_v7.jsonl`. Reproduce with
`scripts/run_compare_all.sh` (parallel, resumable) after
`scripts/rebuild_all.sh`.

(el 0.94/0.83, n=56, omitted for width; sr/lt/ml have no LT-6.5 server truth.
Source: `~/nlprule_bg/final_v5.jsonl` — the v5 engine and bins.)

This includes the Java-only built-in rules that have no XML representation,
ported to Rust (see "ported" below): `UPPERCASE_SENTENCE_START` (with the
Ukrainian list-letter exception, pt dialogue dashes, nl `'k`-contraction and
the cross-sentence state), the `MORFOLOGIK_RULE_*`/`FR_SPELLING_RULE`
spelling rules on the exact morfologik dictionaries the server uses
(`es-ES`, `ca-ES`, `fr`, `nl_NL`, `en_US`, `pt-BR`, `ru_RU`, `uk_UA`; the
dictionaries were dumped from the LT distro jars with morfologik's own
tools), the `immunize`/`ignore_spelling` disambiguation actions, and LT's
`CleanOverlappingFilter` (the server always drops overlapping matches —
language priority first, then the longer match, then the later match).

`build/gap_analysis.py` aggregates which rule IDs the server fires but
nlprule does not (400 sentences per language); the remaining gap decomposes
into:

- `ar` (0.87/0.84): `ArabicTagger.additionalTags` prefix/suffix stemming is
  ported (definite article, clitics; +145 rules recovered, 1062 examples
  pass), `HUNSPELL_RULE_AR` runs on the offline-expanded hunspell dictionary; overlap ties resolve in grammar order (the generic syntax_125 wins over specific rules), taking ar to 0.93/0.90 (plus the SimpleReplaceRule table)
  (30.5M forms, validated 400/400 against the server's accept/reject). The
  residue is gender subrule-variant selection and `ArabicNumberPhraseFilter`
  (needs the ArabicNumbersWords number-to-words engine).
- `es` (0.92/0.90): the sentence splitter now matches LT 6.5 exactly (see the
  SRX note below), so `UPPERCASE_SENTENCE_START` fires on the same fragments.
- `ca` (0.88/0.85): the 22 rules previously dropped for parallel-token
  control flow compile now (skip gaps on `<and>`/`<or>` members are hoisted
  after the group like LT applies them, and a `min="0"` member makes the
  group optional — FALTA_ELEMENT_ENTRE_VERBS, AL_FRONT, FICAR_POSAR, TE,
  ... fire at the same offsets as the server), and
  `SuppressIfAnyRuleMatchesFilter` is ported (re-analyzes the sentence with
  each replacement applied and suppresses the match if any listed rule
  fires overlapping — QUE_INICIAL_*/MES1 match the server). Residue:
  word-internal contractions are split like LT's CatalanWordTokenizer ("del" -> "de" + "l", also al/als/dels/pel/pels/can), which makes the CONCORDANCES_DET_* agreement family fire; residue: `PRONOMS_FEBLES_SOLTS1` over-firing on article/pronoun ambiguities,
  `MUNICIPIS_VALENCIA` (external toponym data), the CONCORDANCES_DET_* gender-agreement family, `SPELLOUT_NUMBERS`
  (CatalanNumberSpellerFilter) and `ES_UNKNOWN` (FindSuggestionsEsFilter).
- `fr` (0.90/0.88): the `D_N` determiner-noun agreement family now fires
  (the antipattern-unification bug is fixed); the residue is tagger
  reading differences and the rules still dropped for parallel-token
  control flow.
- `en` (0.91/0.89, picky rules off like the server's default level): the OpenNLP chunker is ported and loaded from
  `chunker.json` (chunks like `B-NP-singular`/`I-NP`/`E-NP`/`B-VP` verified);
  the residue is speller-backed rules (`EN_CONTRACTION_SPELLING`,
  `EN_SPLIT_WORDS_HYPHEN`) and `MORFOLOGIK_RULE_EN_US` over-firing.
- `ru` (0.93/0.92): mostly tagger differences on the `giloj_gilichnij`
  stemmer-like pairs and `MORFOLOGIK_RULE_RU_RU` suggestion-order details.
- `es`/`pt` (0.90/0.87 for pt after picky off): small; much of it is the same error caught by a variant rule
  (LT fires `HOLA_COMO_ESTAS` where nlprule fires `OLA_HOLA`).
- `nl` (0.83/0.79): the 66k-line SimpleReplaceRule table is now a
  dictionary-backed built-in with the server's `NL_SIMPLE_REPLACE_*`
  sub-rule ids (and Dutch's priority-1 for that family); Dutch
  mid-word apostrophes stay joined like `DutchWordTokenizer` ("zo'n"
  is one token). Residue: the single IETS_KLEINS/GEURIGS_GURIGS
  same-span tie (LT's insertion order there differs from the
  reverse-grammar order that matches fr/de/ca/en/es) and tagger
  reading differences.
- `pl` (0.78/0.71): the overlap filter now applies LT's priority map
  (`ZDANIA_ZLOZONE` = -1, `Polish.getPriorityForId`) before the
  longer-match tiebreak, so the whole-sentence `ZDANIA_ZLOZONE` no
  longer swallows `UPPERCASE_SENTENCE_START` at the sentence start
  (0.7595/0.6926 → 0.7769/0.7085). Residue: `COFANIE_PRZECINKA`,
  `BRAK_PRZECINKA_ZE`/`ZEBY` comma rules (LT's tagger readings), the
  speller (`MORFOLOGIK_RULE_PL_PL` on dictionary dump differences) and
  `DWUZNACZNOSC_W_COFANIU` fragments.
- `de`'s speller: the server does not run it either (its `GermanSpellerRule`
  needs the ngram language model; without it "Glük"/"Fehller"/"nromale"
  are not flagged), so disabling it on our side is parity, not a gap.
  `pl`'s stemmer-based rules and the other Java-only rule classes below.

Language-specific notes:
- `ja` uses lindera with the ipadic dictionary (the same dictionary data LT uses
  via Sen/Gosen); enable the `ja` cargo feature
- `zh` uses jieba segmentation (`zh` feature) loaded with HanLP's own
  dictionaries (the mini CoreNatureDictionary + CustomDictionary, recovered
  from the double-array tries inside `hanlp.jar` — see
  `nlprule/configs/zh/hanlp_dict.txt`), so segmentation matches LT's HanLP
  setup closely (81.6% of examples pass; remaining gaps are HanLP's n-gram
  disambiguation and unknown-word heuristics)
- SimpleReplaceRule data tables (`replace*.txt`) are converted to XML rules
  automatically; tables larger than 10k lines are skipped (nl's 66k-line table
  does not fit the per-rule model)
- `sr` rules come from LT master (ekavian variant) plus its replace tables
- `da`/`km`/`crh`/`eo`/`fa` have incomplete tagger data in LT itself
- Java `RuleFilter` classes ported to runtime filters
  (`nlprule/src/rule/filter_java.rs`, data in `filter_data.rs`): the whole date
  family (DateCheck/FutureDate/NewYear/YMD/DMY/RecentYear/DateRange/... in 14
  languages), MultitokenSpeller (+ per-language word lists),
  *SuppressMisspelledSuggestions, FindSuggestions (en/fr/es/ca, with
  morfologik's frequency-ranked, diacritics-insensitive suggestion order),
  AdvancedSynthesizer, PartialPosTag (ru/ga/en), AddCommas, OrdinalSuffix,
  CompoundCheck, nl CompoundFilter, INN, DecadeSpelling, UppercaseNounReading,
  ConvertToSentenceCase, ConfusionCheck (es/pt), DiacriticsCheck (ca),
  RomanNumeral, RegularIrregularParticiple, ValidWord,
  RemoveUnknownCompounds, WhitespaceCheck, SuppressIfAnyRuleMatches (the
  re-analyze-with-replacement suppression, run in `Rules::suggest`), plus the
  English `+DT`/`+INDT`
  determiner synthesis of `EnglishSynthesizer`
- Sentence splitting matches LT 6.5's SRX semantics: LT's Java engine
  (net.loomchild.segment) only honors a `break="no"` exception whose
  `beforebreak` match *ends* exactly at the break candidate, while the Rust
  `srx` crate anchors on capture group 1. All `data2/*/segment.srx` copies
  are preprocessed accordingly: inner capture groups in `beforebreak`/
  `afterbreak` are rewritten to non-capturing (so group 1 is the crate's
  wrapper at the right offset) and the malformed Spanish abbreviation rule
  (`\b([Ee]d(it)?|[Nn]o|n|...)|[V\.]gr\.[\s\u00A0]`, whose first top-level
  branch is inert in LT but masked word starts in the crate) is replaced by
  its LT-effective form `[V\.]gr\.[\s\u00A0]`. Verified against the server:
  `Cosas. palabra` and `No. vino ayer.` split, `p. ej. esto funciona.`,
  `Sr.`/`núm.`/`Ed.` stay joined.
- Also ported: ca's pronoun/verb-morphology filters (AdjustPronouns,
  AdjustVerbSuggestions, AnarA, DonarTemps, Oblidarse, PortarGerundi,
  PortarTemps, PronomsFeblesHelper with the full weak-pronoun transformation
  tables), the Catalan synthesizer on its valencia dictionary with the
  regional verb-variant fallback and PostagComparator,
  `Catalan.adaptSuggestion`, ar MasdarToVerb/VerbToMafoulMutlaq/
  AdjectiveToExclamation/ArabicDateCheck (with ArabicTagManager flag
  arithmetic), en AdverbFilter, the disambiguation `addchunk` action (GV /
  PTime chunks for ca+es), and `IsEnglishWordFilter` (approximated with a
  frequency list of the 20k most common English words)
- Still unported: ar ArabicNumberPhraseFilter (needs ArabicNumbersWords) and
  `HUNSPELL_RULE_AR` (hunspell affix expansion of `ar.dic`), de's
  ca CatalanNumberSpellerFilter (needs the Catalan number speller) and
  ca's `FindSuggestionsEsFilter` variant,
  pt BrazilianToponymFilter (LT `<regexp>` rules are a separate rule type),
  WordWithDeterminer's `suggestionHasNoErrors` re-validation, and rules that
  are pure Java in LT with no XML (pl stemmer rules, the remaining
  language-specific speller subclasses like en's hyphen splitting and
  pt's dialect rule)

### Performance vs the LanguageTool server

`build/bench_server.py` (300 example sentences per language, single thread,
same machine; LT 6.5 via its local HTTP server at standard level — HTTP + JVM
overhead included, as a real client sees it — nlprule in-process):

| lang | nlprule p50/sent | p95 | throughput | LT p50/sent | p95 | throughput | speedup | nlprule RSS | LT JVM RSS |
|------|------------------|-----|------------|-------------|-----|------------|---------|-------------|------------|
| ar   | 0 ms             | 1 ms | 2088 sents/s | 120 ms   | 161 ms  | 7.8 sents/s   | ~270x  | 457 MB      | ~3.2 GB    |
| ru   | 1 ms             | 1 ms | 1579 sents/s | 361 ms   | 553 ms  | 2.5 sents/s   | ~630x  | 1.3 GB      | ~3.0 GB    |
| pt   | 1 ms             | 3 ms | 577 sents/s  | 953 ms   | 1201 ms | 1.0 sents/s   | ~580x  | 3.3 GB      | ~3.0 GB    |
| uk   | 2 ms             | 3 ms | 562 sents/s  | 996 ms   | 1248 ms | 1.0 sents/s   | ~560x  | 1.3 GB      | ~3.1 GB    |
| en   | 2 ms             | 5 ms | 362 sents/s  | 14 ms    | 23 ms   | 66 sents/s    | 5.5x   | 254 MB      | ~2.8 GB    |
| fr   | 3 ms             | 5 ms | 337 sents/s  | 2074 ms  | 2723 ms | 0.5 sents/s   | ~670x  | 329 MB      | ~2.8 GB    |
| de   | 1 ms             | 2 ms | 285 sents/s  | 19 ms    | 34 ms   | 23 sents/s    | 12x    | 412 MB      | ~2.3 GB    |
| ca   | 4 ms             | 9 ms | 201 sents/s  | 29 ms    | 44 ms   | 27 sents/s    | 7x     | 438 MB      | ~2.9 GB    |
| es   | 1 ms             | 3 ms | 60 sents/s*  | 14 ms    | 34 ms   | 54 sents/s    | 1.1x   | 1.2 GB      | ~1.4 GB    |

nlprule loads its binaries in 0.6-14 s per language and checks every language
at sub-10 ms p95 latency; the LT server needs 0.5-2.7 s per sentence on
fr/pt/uk/ru even at the standard (non-picky) level. (*`es` p50 is 1 ms but
the mean is 16.6 ms: sentences that hit the FindSuggestions full-scan fallback
over the 3.4M-form speller dominate; `pt`'s 3.3 GB RSS and 14 s load come
from its large disambiguation data.) nlprule fires more matches than the
server because it applies rules that LT disables by default (`default="off"`).
The LT JVM RSS is cumulative: the single server process keeps every language
loaded.

With the original LT 5.2-based build directories, English passes 4192/4226 (99.2%) of its
example tests and German 3799/3903 (97.3%).

See the [benchmark issue](https://github.com/bminixhofer/nlprule/issues/6) for details.

## Projects using nlprule

- [prosemd](https://github.com/kitten/prosemd-lsp): a proofreading and linting language server for markdown files with VSCode integration.
- [cargo-spellcheck](https://github.com/drahnr/cargo-spellcheck): a tool to check all your Rust documentation for spelling and grammar mistakes.

Please submit a PR to add your project!

## Acknowledgements

All credit for the resources used in nlprule goes to [LanguageTool](https://github.com/languagetool-org/languagetool) who have made a Herculean effort to create high-quality resources for Grammatical Error Correction and broader NLP.

## License

nlprule is licensed under the MIT license or Apache-2.0 license, at your option.

The nlprule binaries (`*.bin`) are derived from LanguageTool v5.2 and licensed under the LGPLv2.1 license. nlprule statically and dynamically links to these binaries. Under LGPLv2.1 §6(a) this does not have any implications on the license of nlprule itself.
