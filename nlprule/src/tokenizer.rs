//! A tokenizer to split raw text into tokens.
//! Tokens are assigned lemmas and part-of-speech tags by lookup from a [Tagger][tag::Tagger] and chunks containing
//! information about noun / verb and grammatical case by a statistical [Chunker][chunk::Chunker].
//! Tokens are *disambiguated* (i. e. information from the initial assignment is changed) in a rule-based way by
//! [DisambiguationRule][crate::rule::DisambiguationRule]s.

use crate::{
    rule::id::{Index, Selector},
    rule::MatchSentence,
    types::*,
    utils::{parallelism::MaybeParallelRefIterator, regex::Regex},
    Error,
};
use fs_err::File;
use serde::{Deserialize, Serialize};
use std::{
    io::{BufReader, Read, Write},
    ops::Range,
    path::Path,
    sync::Arc,
};

pub mod ar_stem;
pub mod chunk;
pub mod cjk;
pub mod multiword;
pub mod tag;

use chunk::Chunker;
use multiword::MultiwordTagger;
use tag::Tagger;

use crate::rule::DisambiguationRule;

/// Split a text at the points where the given function is true.
/// Keeps the separators. See https://stackoverflow.com/a/40296745.
fn split<F>(text: &str, split_func: F) -> Vec<&str>
where
    F: Fn(char) -> bool,
{
    let mut result = Vec::new();
    let mut last = 0;
    for (index, matched) in text.match_indices(split_func) {
        if last != index {
            result.push(&text[last..index]);
        }
        result.push(matched);
        last = index + matched.len();
    }
    if last < text.len() {
        result.push(&text[last..]);
    }

    result
}

/// Options for a tokenizer.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct TokenizerLangOptions {
    /// Whether to allow errors while constructing the tokenizer.
    pub allow_errors: bool,
    /// Disambiguation Rule selectors to use in this tokenizer.
    #[serde(default)]
    pub ids: Vec<Selector>,
    /// Disambiguation Rule selectors to ignore in this tokenizer.
    #[serde(default)]
    pub ignore_ids: Vec<Selector>,
    /// Specific examples in the notation `{id}:{example_index}` which are known to fail.
    #[serde(default)]
    pub known_failures: Vec<String>,
    /// Extra language-specific characters to split text on.
    #[serde(default)]
    pub extra_split_chars: Vec<char>,
    /// Extra language-specific Regexes of which the matches will *not* be split into multiple tokens.
    #[serde(default)]
    pub extra_join_regexes: Vec<Regex>,
    /// CJK dictionary-based word segmentation to use ("jieba" for Chinese).
    #[serde(default)]
    pub cjk_segmentation: Option<String>,
    /// Split word-internal contractions the way the language's LT word
    /// tokenizer does ("ca": al/als/del/dels/pel/pels/can -> preposition +
    /// article, e.g. "del" -> "de" + "l").
    #[serde(default)]
    pub split_contractions: Option<String>,
    /// LT's English tokenizer splits a leading or trailing hyphen off an
    /// unknown whitespace chunk ("seco- ndary" -> [seco][-][ndary]), while
    /// intra-word hyphens stay part of the token ("well-suiting").
    #[serde(default)]
    pub split_edge_hyphens: bool,
    /// LT's English tokenizer on an unknown chunk containing an apostrophe:
    /// if the part before the first apostrophe is a known word, the
    /// apostrophe is glued to the remainder ("We'RE" -> [We]['RE]);
    /// otherwise the chunk splits into [left][apos][right]
    /// ("didnt't" -> [didnt]['][t]).
    #[serde(default)]
    pub apostrophe_glue_after_known: bool,
    /// LT's Breton tokenizer: "c'h" is a letter (trigraph) and never split;
    /// any other apostrophe is glued to the preceding letter and the word
    /// splits after it ("n'eo" -> ["n'"][eo], "c'havotenn" -> one token).
    /// NOT serialized (bincode has no field versioning; a serialized field
    /// would break every older tokenizer binary) — binaries enable it from
    /// the language code via `Tokenizer::set_breton_apostrophes`.
    #[serde(skip)]
    pub breton_apostrophes: bool,
}

impl Default for TokenizerLangOptions {
    fn default() -> Self {
        TokenizerLangOptions {
            allow_errors: false,
            ids: Vec::new(),
            ignore_ids: Vec::new(),
            known_failures: Vec::new(),
            extra_split_chars: Vec::new(),
            extra_join_regexes: Vec::new(),
            cjk_segmentation: None,
            split_contractions: None,
            split_edge_hyphens: false,
            apostrophe_glue_after_known: false,
            breton_apostrophes: false,
        }
    }
}

/// An iterator over [IncompleteSentence]s. Has the same properties as [SentenceIter].
pub struct IncompleteSentenceIter<'t> {
    text: &'t str,
    splits: Vec<Range<usize>>,
    tokenizer: &'t Tokenizer,
    index: usize,
    position: Position,
}

impl<'t> Iterator for IncompleteSentenceIter<'t> {
    type Item = IncompleteSentence<'t>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index == self.splits.len() {
            return None;
        }

        let mut range = self.splits[self.index].clone();
        self.index += 1;

        // as long as the current sentence contains only whitespace, add the next sentence
        // in practice, this might never happen, but we can not make any assumption about
        // SRX rule behavior here.
        while self.text[range.clone()].trim().is_empty() && self.index < self.splits.len() {
            range.end = self.splits[self.index].end;
            self.index += 1;
        }

        let sentence = self
            .tokenizer
            .tokenize(&self.text[range.clone()])
            .map(|x| x.rshift(self.position));

        self.position += Position {
            char: self.text[range.clone()].chars().count(),
            byte: range.len(),
        };

        sentence
    }
}

/// An iterator over [Sentence]s. Has some key properties:
/// - Preceding whitespace is always included so the first sentence always starts at byte and char index zero.
/// - There are no gaps between sentences i.e. `sentence[i - 1].span().end() == sentence[i].span().start()`.
/// - Behavior for trailing whitespace is not defined. Can be included in the last sentence or not be part of any sentence.
pub struct SentenceIter<'t> {
    inner: IncompleteSentenceIter<'t>,
    tokenizer: &'t Tokenizer,
}

impl<'t> Iterator for SentenceIter<'t> {
    type Item = Sentence<'t>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner
            .next()
            .map(|sentence| self.tokenizer.disambiguate(sentence).into_sentence())
    }
}

/// The complete Tokenizer doing tagging, chunking and disambiguation.
#[derive(Serialize, Deserialize, Default, Clone)]
pub struct Tokenizer {
    pub(crate) rules: Vec<DisambiguationRule>,
    pub(crate) chunker: Option<Chunker>,
    pub(crate) sentencizer: srx::Rules,
    pub(crate) multiword_tagger: Option<MultiwordTagger>,
    pub(crate) tagger: Arc<Tagger>,
    pub(crate) lang_options: TokenizerLangOptions,
    /// CJK segmenter used to split whitespace-less text into words.
    pub(crate) cjk: Option<cjk::CjkSegmenter>,
}

impl Tokenizer {
    /// Creates a new tokenizer from a path to a binary.
    ///
    /// # Errors
    /// - If the file can not be opened.
    /// - If the file content can not be deserialized to a rules set.
    pub fn new<P: AsRef<Path>>(p: P) -> Result<Self, Error> {
        let reader = BufReader::new(File::open(p.as_ref())?);
        Ok(bincode::deserialize_from(reader)?)
    }

    /// Creates a new tokenizer from a reader.
    pub fn from_reader<R: Read>(reader: R) -> Result<Self, Error> {
        Ok(bincode::deserialize_from(reader)?)
    }

    /// Enable LT BretonWordTokenizer apostrophe handling for this tokenizer.
    /// The flag is not serialized with the binary; callers that know the
    /// language (check_server, test) set it explicitly.
    pub fn set_breton_apostrophes(&mut self, enabled: bool) {
        self.lang_options.breton_apostrophes = enabled;
    }

    /// Serializes this rules set to a writer.
    pub fn to_writer<W: Write>(&self, writer: W) -> Result<(), Error> {
        Ok(bincode::serialize_into(writer, &self)?)
    }

    /// Gets all disambigation rules in the order they are applied.
    pub fn rules(&self) -> &[DisambiguationRule] {
        &self.rules
    }

    /// Gets the lexical tagger.
    pub fn tagger(&self) -> &Arc<Tagger> {
        &self.tagger
    }

    /// Gets the chunker if one exists.
    pub fn chunker(&self) -> &Option<Chunker> {
        &self.chunker
    }

    pub(crate) fn lang_options(&self) -> &TokenizerLangOptions {
        &self.lang_options
    }

    /// The CJK segmenter configured for this language, if any.
    pub fn cjk_segmenter(&self) -> Option<cjk::CjkSegmenter> {
        self.cjk
    }

    pub(crate) fn disambiguate_up_to_id<'t>(
        &'t self,
        mut sentence: IncompleteSentence<'t>,
        id: Option<&Index>,
    ) -> IncompleteSentence<'t> {
        let n = id.map_or(self.rules.len(), |id| {
            self.rules.iter().position(|x| x.id == *id).unwrap()
        });
        let mut i = 0;

        while i < n {
            let complete_sentence = sentence.clone().into_sentence();
            let match_sentence = MatchSentence::new(&complete_sentence);

            let result = self.rules[i..n]
                .maybe_par_iter()
                .enumerate()
                .filter_map(|(j, rule)| {
                    let changes = rule.apply(&match_sentence);
                    if changes.is_empty() {
                        None
                    } else {
                        Some((j + i, changes))
                    }
                })
                .find_first(|_| true);

            if let Some((index, changes)) = result {
                self.rules[index].change(&mut sentence, changes);
                i = index + 1;
            } else {
                i = n;
            }
        }

        sentence
    }

    /// Apply rule-based disambiguation to the tokens.
    /// This does not change the number of tokens, but can change the content arbitrarily.
    pub fn disambiguate<'t>(&'t self, sentence: IncompleteSentence<'t>) -> IncompleteSentence<'t> {
        self.disambiguate_up_to_id(sentence, None)
    }

    fn get_token_ranges<'t>(
        &self,
        text: &'t str,
    ) -> impl ExactSizeIterator<Item = Range<usize>> + 't {
        let mut tokens = Vec::new();

        const APOS_CHARS: [char; 4] = ['\'', '\u{2019}', '\u{2018}', '\u{02BC}'];

        let glue_mode = self.lang_options.apostrophe_glue_after_known;
        let br_mode = self.lang_options.breton_apostrophes;
        let split_char = |c: char| {
            c.is_whitespace()
                || (crate::utils::splitting_chars().contains(c)
                    && !(glue_mode && (c == '\'' || c == '\u{2019}'))
                    && !(br_mode && APOS_CHARS.contains(&c)))
        };
        let split_text = |text: &'t str| {
            let mut tokens = Vec::new();
            for pretoken in split(text, split_char) {
                // if the token is in the dictionary, we add it right away
                if self.tagger.id_word(pretoken.into()).1.is_some() {
                    tokens.push(pretoken);
                } else if self.lang_options.split_edge_hyphens
                    && pretoken.chars().count() > 2
                    && (pretoken.starts_with('-') || pretoken.ends_with('-'))
                {
                    // LT's English tokenizer: an unknown chunk with an edge
                    // hyphen gets the hyphen as its own token
                    // ("seco- ndary" -> [seco][-][ndary])
                    if let Some(stripped) = pretoken.strip_suffix('-') {
                        tokens.push(stripped);
                        tokens.push(&pretoken[pretoken.len() - 1..]);
                    } else if let Some(stripped) = pretoken.strip_prefix('-') {
                        tokens.push(&pretoken[..1]);
                        tokens.push(stripped);
                    }
                } else if self.lang_options.breton_apostrophes
                    && pretoken.chars().any(|c| APOS_CHARS.contains(&c))
                {
                    // LT BretonWordTokenizer: "c'h" (any apostrophe variant
                    // between c/C and h/H) is a letter of the word; any other
                    // apostrophe is glued to the preceding letter and the word
                    // splits right after it ("n'eo" -> [n'][eo],
                    // "c'havotenn" -> one token)
                    let chars: Vec<char> = pretoken.chars().collect();
                    let mut start = 0usize;
                    let mut byte_i = 0usize;
                    for (idx, &c) in chars.iter().enumerate() {
                        if APOS_CHARS.contains(&c) {
                            let is_ch = idx > 0
                                && idx + 1 < chars.len()
                                && matches!(chars[idx - 1], 'c' | 'C')
                                && matches!(chars[idx + 1], 'h' | 'H');
                            let apos_end = byte_i + c.len_utf8();
                            if !is_ch && apos_end > start {
                                tokens.push(&pretoken[start..apos_end]);
                                start = apos_end;
                            }
                        }
                        byte_i += c.len_utf8();
                    }
                    if start < pretoken.len() {
                        tokens.push(&pretoken[start..]);
                    }
                } else if self.lang_options.apostrophe_glue_after_known
                    && pretoken.chars().any(|c| c == '\'' || c == '\u{2019}')
                {
                    let apostrophe_idx = pretoken
                        .char_indices()
                        .find(|(_, c)| *c == '\'' || *c == '\u{2019}')
                        .map(|(i, _)| i)
                        .unwrap_or(pretoken.len());
                    let (left, rest) = pretoken.split_at(apostrophe_idx);
                    let apos_len = rest.chars().next().map_or(1, char::len_utf8);
                    let rest_upper = rest[apos_len..].chars().any(char::is_uppercase);
                    if !left.is_empty()
                        && (rest_upper || rest[apos_len..].chars().count() > 1)
                        && self.tagger.id_word(left.into()).1.is_some()
                    {
                        // known left part + uppercase remainder (We'RE):
                        // glue the apostrophe to the rest, like LT
                        tokens.push(left);
                        tokens.push(rest);
                    } else {
                        // unknown left part: split at every apostrophe,
                        // keeping each apostrophe as its own token
                        let mut start = 0usize;
                        for (i, c) in pretoken.char_indices() {
                            if c == '\'' || c == '\u{2019}' {
                                if i > start {
                                    tokens.push(&pretoken[start..i]);
                                }
                                tokens.push(&pretoken[i..i + c.len_utf8()]);
                                start = i + c.len_utf8();
                            }
                        }
                        if start < pretoken.len() {
                            tokens.push(&pretoken[start..]);
                        }
                    }
                } else {
                    // otherwise, potentially split it again with `extra_split_chars` e. g. "-"
                    tokens.extend(split(pretoken, |c| {
                        split_char(c) || self.lang_options.extra_split_chars.contains(&c)
                    }));
                }
            }

            // LT CatalanWordTokenizer patterns[9]/[10]: contractions split
            // into preposition + article as two tokens ("del" -> "de" + "l");
            // the second part has no space before it because its span is
            // inside the original word
            if self.lang_options.split_contractions.as_deref() == Some("ca") {
                let mut split2 = Vec::with_capacity(tokens.len());
                for token in tokens {
                    // case-insensitive so sentence-initial "Als" also splits
                    // (LT rebuilds the parts preserving case, e.g. "A" + "ls")
                    let lower = token.to_lowercase();
                    let second_len = match lower.as_str() {
                        "al" | "del" | "pel" | "can" => 1,
                        "als" | "dels" | "pels" => 2,
                        _ => {
                            split2.push(token);
                            continue;
                        }
                    };
                    let split_at = token.len() - second_len;
                    split2.push(&token[..split_at]);
                    split2.push(&token[split_at..]);
                }
                tokens = split2;
            }

            // CJK text has no whitespace word boundaries: segment it with a
            // dictionary-based segmenter (mirrors LT's HanLP / Sen tokenizers)
            if let Some(segmenter) = self.cjk {
                let mut segmented = Vec::new();
                for token in tokens {
                    if cjk::needs_segmentation(token) {
                        let offset = token.as_ptr() as usize - text.as_ptr() as usize;
                        segmented.extend(
                            cjk::segment(token, segmenter)
                                .into_iter()
                                .map(|(start, end)| {
                                    &text[offset + start..offset + end]
                                }),
                        );
                    } else {
                        segmented.push(token);
                    }
                }
                tokens = segmented;
            }

            // LT's km tokenizer emits \u200b as its own token, but LT pattern
            // matching runs over getTokensWithoutWhitespace which drops it —
            // ZWSP is a word boundary, not a word. Drop ZWSP-only tokens so
            // patterns match across the boundary while the preceding char
            // stays non-whitespace ("no space before" semantics preserved).
            tokens.retain(|t| !t.chars().all(|c| c == '\u{200b}'));

            tokens
        };

        let mut joined_mask = vec![false; text.len()];
        let mut joins = Vec::new();

        for regex in self.lang_options.extra_join_regexes.iter() {
            for mat in regex.find_iter(text) {
                if !joined_mask[mat.start()..mat.end()].iter().any(|x| *x) {
                    joins.push(mat.start()..mat.end());
                    joined_mask[mat.start()..mat.end()]
                        .iter_mut()
                        .for_each(|x| *x = true);
                }
            }
        }

        joins.sort_by(|a, b| a.start.cmp(&b.start));

        let mut prev = 0;
        for range in joins {
            tokens.extend(split_text(&text[prev..range.start]));
            prev = range.end;
            tokens.push(&text[range]);
        }

        tokens.extend(split_text(&text[prev..text.len()]));
        tokens.into_iter().map(move |token| {
            let byte_start = (token.as_ptr() as usize)
                .checked_sub(text.as_ptr() as usize)
                .expect("Each token str is a slice of the text str.");

            byte_start..byte_start + token.len()
        })
    }

    /// Tokenize the given sentence. This applies chunking and tagging, but does not do disambiguation.
    // NB: this is not public because it could be easily misused by passing a text instead of one sentence.
    pub(crate) fn tokenize<'t>(&'t self, sentence: &'t str) -> Option<IncompleteSentence<'t>> {
        if sentence.trim().is_empty() {
            return None;
        }

        let token_strs = self.get_token_ranges(sentence);
        let n_token_strs = token_strs.len();

        let mut tokens: Vec<_> = token_strs
            .enumerate()
            .filter(|(_, range)| !sentence[range.clone()].trim().is_empty())
            .map(|(i, range)| {
                let byte_start = range.start;
                let char_start = sentence[..byte_start].chars().count();

                let token_text = sentence[range].trim();

                let is_sentence_start = i == 0;
                // LT JLanguageTool.analyzeText: `tokenArray[lastToken].setSentEnd()`
                // — UNCONDITIONAL on the last non-whitespace token, punctuated
                // or not (only trailing whitespace is skipped)
                let is_sentence_end = i == n_token_strs - 1;

                let mut tags: Vec<_> = self
                    .tagger
                    .get_tags_with_options(
                        token_text,
                        if is_sentence_start { Some(true) } else { None },
                        None,
                    )
                    .collect();

                if self.cjk.is_some() && cjk::needs_segmentation(token_text) {
                    tags.extend(cjk::word_data(token_text, self.cjk.unwrap(), &self.tagger));
                }

                // Arabic prefix/suffix stemming (LT ArabicTagger.additionalTags):
                // derive readings of e.g. "السلامة" from the dictionary entry
                // "سلامة" with merged procletic flags
                if self.tagger.lang_options().arabic_stemming {
                    let base: Vec<_> = tags
                        .iter()
                        .map(|t| {
                            (
                                t.lemma().as_str().to_string(),
                                t.pos().as_str().to_string(),
                            )
                        })
                        .collect();
                    if !ar_stem::is_stopword_reading(&base) {
                        let tagger = &self.tagger;
                        let additional = ar_stem::additional_tags(token_text, |stem| {
                            tagger
                                .get_tags(stem)
                                .map(|d| {
                                    (
                                        d.lemma().as_str().to_string(),
                                        d.pos().as_str().to_string(),
                                    )
                                })
                                .collect()
                        });
                        for (lemma, pos) in additional {
                            tags.push(WordData::new(
                                tagger.id_word(lemma.into()),
                                tagger.id_tag_stored(&pos),
                            ));
                        }
                    }
                }

                IncompleteToken::new(
                    Word::new(self.tagger.id_word(token_text.into()), tags),
                    Span::new(
                        byte_start..byte_start + token_text.len(),
                        char_start..char_start + token_text.chars().count(),
                    ),
                    is_sentence_end,
                    sentence[..byte_start].ends_with(char::is_whitespace),
                    Vec::new(),
                )
            })
            .collect();

        let mut sentence = IncompleteSentence::new(tokens, sentence, &self.tagger);

        if let Some(chunker) = &self.chunker {
            chunker.apply(&mut sentence);
        }

        if let Some(multiword_tagger) = &self.multiword_tagger {
            multiword_tagger.apply(&mut sentence);
        }

        Some(sentence)
    }

    /// Splits the text into sentences and tokenizes each sentence.
    pub fn sentencize<'t>(&'t self, text: &'t str) -> IncompleteSentenceIter<'t> {
        IncompleteSentenceIter {
            text,
            splits: self.sentencizer.split_ranges(text),
            tokenizer: &self,
            index: 0,
            position: Position::default(),
        }
    }

    /// Applies the entire tokenization pipeline including sentencization, tagging, chunking and disambiguation.
    pub fn pipe<'t>(&'t self, text: &'t str) -> SentenceIter<'t> {
        SentenceIter {
            inner: self.sentencize(text),
            tokenizer: &self,
        }
    }
}
