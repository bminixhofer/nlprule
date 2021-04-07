//! Fundamental types used by this crate.

use crate::tokenizer::tag::Tagger;
pub use crate::tokenizer::tag::{PosId, WordId};
pub(crate) use crate::tokenizer::tag::{PosIdInt, SpecialPos, WordIdInt};
use derivative::Derivative;
use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};
use std::{
    cmp::Ordering,
    collections::{hash_map, HashMap, HashSet},
    ops::{Add, AddAssign, Range, Sub},
};
use bimap::BiHashMap;


pub(crate) type DefaultHashMap<K, V> = HashMap<K, V>;
pub(crate) type DefaultHashSet<T> = HashSet<T>;
pub(crate) type DefaultHasher = hash_map::DefaultHasher;

pub(crate) type FastBiMap<L,R> = BiHashMap<L, R, hashbrown::hash_map::DefaultHashBuilder, hashbrown::hash_map::DefaultHashBuilder>;
pub(crate) type FastHashSet<I> = hashbrown::HashSet<I>;
pub(crate) type FastHashMap<K,V> = hashbrown::HashMap<K,V>;

#[derive(Debug, Copy, Clone, Serialize, Deserialize, Hash, Eq, PartialEq, Ord, PartialOrd)]
#[serde(transparent)]
pub(crate) struct WordIdInt(pub u32);
#[derive(Debug, Copy, Clone, Serialize, Deserialize, Hash, Eq, PartialEq, Ord, PartialOrd)]
#[serde(transparent)]
pub(crate) struct PosIdInt(pub u16);

/// Owned versions of the types for use in longer-living structures not bound to the `'t` lifetime e.g. rule tests.
pub mod owned {
    use super::*;
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Serialize, Deserialize, Hash, Eq, PartialEq, Clone)]
    /// See [super::WordId].
    pub struct WordId(pub(crate) String, pub(crate) Option<WordIdInt>);

    impl WordId {
        /// Gets this ID as a reference ID.
        pub fn as_ref_id(&self) -> super::WordId {
            super::WordId(self.0.as_str().into(), self.1)
        }
    }

    impl AsRef<str> for WordId {
        fn as_ref(&self) -> &str {
            self.0.as_ref()
        }
    }

    /// See [super::PosId].
    #[derive(Debug, Serialize, Deserialize, Hash, Eq, PartialEq, Clone)]
    pub struct PosId(pub(crate) String, pub(crate) PosIdInt);

    impl PosId {
        /// Gets this ID as a reference ID.
        pub fn as_ref_id(&self) -> super::PosId {
            super::PosId::regular(self.0.as_str(), self.1)
        }
    }

    impl AsRef<str> for PosId {
        fn as_ref(&self) -> &str {
            self.0.as_ref()
        }
    }

    /// See [super::WordData].
    #[derive(Debug, Serialize, Deserialize, Hash, Eq, PartialEq, Clone)]
    #[allow(missing_docs)]
    pub struct WordData {
        pub lemma: WordId,
        pub pos: PosId,
    }

    impl WordData {
        /// Creates a new owned Word ID.
        pub fn new(lemma: WordId, pos_id: PosId) -> Self {
            WordData { lemma, pos: pos_id }
        }
    }

    /// See [super::Word].
    #[derive(Debug, Serialize, Deserialize, Hash, Eq, PartialEq, Clone)]
    #[allow(missing_docs)]
    pub struct Word {
        pub text: WordId,
        pub tags: Vec<WordData>,
    }

    /// See [super::Token].
    #[derive(Debug, Serialize, Deserialize, Hash, Eq, PartialEq, Clone)]
    #[allow(missing_docs)]
    pub struct Token {
        pub word: Word,
        pub span: Span,
        pub has_space_before: bool,
        pub chunks: Vec<String>,
    }
}

/// A incomplete sentence containing partially set information about the tokens.
/// Can be converted to a complete sentence with [into_sentence][IncompleteSentence::into_sentence].
#[derive(Derivative, Clone)]
#[derivative(Debug, PartialEq)]
pub struct IncompleteSentence<'t> {
    text: &'t str,
    tokens: Vec<IncompleteToken<'t>>,
    #[derivative(Debug = "ignore", PartialEq = "ignore")]
    tagger: &'t Tagger,
    span: Span,
}

impl<'t> IntoIterator for IncompleteSentence<'t> {
    type Item = IncompleteToken<'t>;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.tokens.into_iter()
    }
}

// is_empty does not make sense because there is always at least one token
#[allow(clippy::clippy::len_without_is_empty)]
impl<'t> IncompleteSentence<'t> {
    /// Creates a new incomplete sentence.
    pub(crate) fn new(tokens: Vec<IncompleteToken<'t>>, text: &'t str, tagger: &'t Tagger) -> Self {
        IncompleteSentence {
            text,
            tokens,
            tagger,
            span: Span::new(0..text.len(), 0..text.chars().count()),
        }
    }

    /// Gets the text of this sentence.
    pub fn text(&self) -> &'t str {
        self.text
    }

    /// Returns an iterator over tokens by mutable reference.
    pub fn iter_mut(&mut self) -> impl DoubleEndedIterator<Item = &mut IncompleteToken<'t>> {
        self.tokens.iter_mut()
    }

    /// Returns an iterator over tokens by reference.
    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &IncompleteToken> {
        self.tokens.iter()
    }

    /// Gets the amount of tokens in this sentence.
    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    /// Gets the tagger associated with this sentence.
    pub fn tagger(&self) -> &'t Tagger {
        self.tagger
    }

    /// Converts this incomplete sentence into a [Sentence].
    pub fn into_sentence(self) -> Sentence<'t> {
        let tagger = self.tagger();

        Sentence {
            text: self.text(),
            tagger,
            tokens: self
                .tokens
                .into_iter()
                .map(|token| token.into_token())
                .collect(),
            span: self.span,
        }
    }

    /// Returns the span of this sentence.
    pub fn span(&self) -> &Span {
        &self.span
    }

    /// Shift the span of this sentence right by the specified amount.
    pub fn rshift(mut self, position: Position) -> Self {
        self.span = self.span.rshift(position);
        self.tokens = self
            .tokens
            .into_iter()
            .map(|x| x.rshift(position))
            .collect();
        self
    }
}

/// A Sentence. As opposed to [IncompleteSentence], all information is set and frozen.
/// Always contains at least one token.
#[derive(Derivative, Clone)]
#[derivative(Debug, PartialEq)]
pub struct Sentence<'t> {
    text: &'t str,
    tokens: Vec<Token<'t>>,
    #[derivative(Debug = "ignore", PartialEq = "ignore")]
    tagger: &'t Tagger,
    span: Span,
}

impl<'t> IntoIterator for Sentence<'t> {
    type Item = Token<'t>;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.tokens.into_iter()
    }
}

// is_empty does not make sense because there is always at least one token
#[allow(clippy::clippy::len_without_is_empty)]
impl<'t> Sentence<'t> {
    /// Gets the tokens in this sentence.
    pub fn tokens(&self) -> &[Token<'t>] {
        &self.tokens
    }

    /// Returns an iterator over tokens by reference.
    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &Token> {
        self.tokens.iter()
    }

    /// Gets the text of this sentence.
    pub fn text(&self) -> &'t str {
        self.text
    }

    /// Gets the amount of tokens in this sentence.
    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    /// Gets the tagger associated with this sentence.
    pub fn tagger(&self) -> &'t Tagger {
        self.tagger
    }

    /// Returns the span of this sentence.
    pub fn span(&self) -> &Span {
        &self.span
    }
}

/// Lemma and part-of-speech tag associated with a word.
#[derive(Debug, Clone, PartialEq)]
pub struct WordData<'t> {
    lemma: WordId<'t>,
    pos: PosId<'t>,
    frozen: bool,
}

impl<'t> WordData<'t> {
    /// The lemma word ID.
    pub fn lemma(&self) -> &WordId<'t> {
        &self.lemma
    }

    /// The part-of-speech ID.
    pub fn pos(&self) -> &PosId<'t> {
        &self.pos
    }

    /// Creates a new referential word data.
    pub fn new(lemma: WordId<'t>, pos: PosId<'t>) -> Self {
        WordData {
            lemma,
            pos,
            frozen: false,
        }
    }

    /// Freezes the data hinting that it should never be removed from a word once added.
    pub fn freeze(&mut self) {
        self.frozen = true;
    }

    /// Checks whether the data is frozen i.e. whether it can never be removed from a word once added.
    pub fn frozen(&self) -> bool {
        self.frozen
    }

    /// Converts to owned word data.
    pub fn to_owned_word_data(&self) -> owned::WordData {
        owned::WordData {
            lemma: self.lemma.to_owned_id(),
            pos: self.pos.to_owned_id(),
        }
    }
}

/// Contains all the local information about a token i. e.
/// the text itself and the [WordData]s associated with the word.
#[derive(Debug, Clone, PartialEq)]
pub struct Word<'t> {
    text: WordId<'t>,
    tags: Vec<WordData<'t>>,
}

impl<'t> Word<'t> {
    /// Creates a new Word.
    pub fn new(text: WordId<'t>, tags: Vec<WordData<'t>>) -> Self {
        Word { text, tags }
    }

    /// The text ID of this token.
    pub fn text(&self) -> &WordId<'t> {
        &self.text
    }

    /// Multiple pairs of (lemma, part-of-speech) associated with this token.
    /// Order is in general not significant.
    pub fn tags(&self) -> &[WordData<'t>] {
        &self.tags
    }

    /// Gets the word text as string.
    pub fn as_str(&'t self) -> &'t str {
        self.text.as_str()
    }

    /// Removes all non-frozen tags.
    pub fn clear(&mut self) {
        self.retain(|_| false);
    }

    /// Equivalent to [Vec::retain][std::vec::Vec::retain] on the tags but makes sure frozen tags are ignored.
    pub fn retain<F: FnMut(&WordData<'t>) -> bool>(&mut self, mut f: F) {
        self.tags.retain(|data| data.frozen() || f(data));
    }

    /// Adds a new tag to the word.
    pub fn push(&mut self, data: WordData<'t>) {
        self.tags.push(data);
    }

    /// Converts to an owned word.
    pub fn to_owned_word(&self) -> owned::Word {
        owned::Word {
            text: self.text.to_owned_id(),
            tags: self.tags.iter().map(|x| x.to_owned_word_data()).collect(),
        }
    }
}

/// A token where varying levels of information are set.
#[derive(Debug, Clone, PartialEq)]
pub struct IncompleteToken<'t> {
    word: Word<'t>,
    span: Span,
    is_sentence_end: bool,
    has_space_before: bool,
    chunks: Vec<String>,
}

impl<'t> IncompleteToken<'t> {
    pub(crate) fn new(
        word: Word<'t>,
        span: Span,
        is_sentence_end: bool,
        has_space_before: bool,
        chunks: Vec<String>,
    ) -> Self {
        IncompleteToken {
            word,
            span,
            is_sentence_end,
            has_space_before,
            chunks,
        }
    }

    /// Converts this incomplete token to a complete token.
    pub fn into_token(self) -> Token<'t> {
        let mut word = self.word.clone();

        word.tags.push(WordData::new(
            self.word.text.clone(),
            PosId::special(SpecialPos::None),
        ));

        if word.tags.iter().all(|x| x.pos.as_str().is_empty()) {
            word.tags.push(WordData::new(
                self.word.text.clone(),
                PosId::special(SpecialPos::Unknown),
            ));
        }

        if self.is_sentence_end {
            word.tags.push(WordData::new(
                self.word.text,
                PosId::special(SpecialPos::SentEnd),
            ));
        }

        Token {
            word,
            span: self.span,
            has_space_before: self.has_space_before,
            chunks: self.chunks,
        }
    }

    /// The word of this token. Contains information about the actual text and part-of-speech tags + lemmas.
    pub fn word(&self) -> &Word<'t> {
        &self.word
    }

    #[allow(missing_docs)]
    pub fn word_mut(&mut self) -> &mut Word<'t> {
        &mut self.word
    }

    /// The span of this sentence.
    pub fn span(&self) -> &Span {
        &self.span
    }

    /// Whether this token is the last token in the sentence-
    pub fn is_sentence_end(&self) -> bool {
        self.is_sentence_end
    }

    #[allow(missing_docs)]
    pub fn is_sentence_end_mut(&mut self) -> &mut bool {
        &mut self.is_sentence_end
    }

    /// Whether this token has one or more whitespace characters before.
    pub fn has_space_before(&self) -> bool {
        self.has_space_before
    }

    /// Chunks associated with this token.
    pub fn chunks(&self) -> &[String] {
        &self.chunks
    }

    #[allow(missing_docs)]
    pub fn chunks_mut(&mut self) -> &mut Vec<String> {
        &mut self.chunks
    }

    /// Shift the span of this token right by the specified amount.
    pub fn rshift(mut self, position: Position) -> Self {
        self.span = self.span.rshift(position);
        self
    }
}

/// A finished token with all information set.
/// The main difference to [IncompleteToken] is that all the information is frozen.
#[derive(Clone, Debug, PartialEq)]
#[allow(missing_docs)]
pub struct Token<'t> {
    word: Word<'t>,
    span: Span,
    has_space_before: bool,
    chunks: Vec<String>,
}

lazy_static! {
    static ref SENT_START: Token<'static> = {
        Token {
            word: Word::new(
                WordId::empty(),
                vec![WordData::new(
                    WordId::empty(),
                    PosId::special(SpecialPos::SentStart),
                )]
                .into_iter()
                .collect(),
            ),
            span: Span::default(),
            has_space_before: false,
            chunks: Vec::new(),
        }
    };
}

impl<'t> Token<'t> {
    /// Converts this token to an owned equivalent.
    pub fn to_owned_token(&self) -> owned::Token {
        owned::Token {
            word: self.word.to_owned_word(),
            span: self.span.clone(),
            has_space_before: self.has_space_before,
            chunks: self.chunks.clone(),
        }
    }

    /// The word of this token. Contains information about the actual text and part-of-speech tags + lemmas.
    pub fn word(&self) -> &Word<'t> {
        &self.word
    }

    /// The span of this sentence.
    pub fn span(&self) -> &Span {
        &self.span
    }

    /// Whether this token has one or more whitespace characters before.
    pub fn has_space_before(&self) -> bool {
        self.has_space_before
    }

    /// Chunks associated with this token.
    pub fn chunks(&self) -> &[String] {
        &self.chunks
    }

    pub(crate) fn sent_start() -> &'static Token<'static> {
        &*SENT_START
    }

    /// Shift the span of this sentence right by the specified amount.
    pub fn rshift(mut self, position: Position) -> Self {
        self.span = self.span.rshift(position);
        self
    }
}

/// A position in a text. Determined by a byte and char index.
/// Can be an absolute position (offset relative to zero) or a position delta (offset relative to some other position).
#[derive(Debug, Clone, Copy, PartialEq, Default, Serialize, Deserialize)]
pub struct Position {
    /// The byte offset.
    pub byte: usize,
    /// The char offset.
    pub char: usize,
}

impl PartialOrd for Position {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        let byte_order = self.byte.cmp(&other.byte);
        // for positions to be comparable byte and char must both be smaller / larger / equal
        if byte_order == self.char.cmp(&other.char) {
            Some(byte_order)
        } else {
            None
        }
    }
}

impl AddAssign for Position {
    fn add_assign(&mut self, other: Self) {
        *self = *self + other;
    }
}

impl Add for Position {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            byte: self.byte + other.byte,
            char: self.char + other.char,
        }
    }
}

impl Sub for Position {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        Self {
            byte: self.byte.saturating_sub(other.byte),
            char: self.char.saturating_sub(other.char),
        }
    }
}

/// A span in a text determined by start (inclusive) and end (exclusive) position.
/// The start must always be greater than or equal to the end.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
pub struct Span {
    byte: Range<usize>,
    char: Range<usize>,
}

impl Span {
    /// Creates a new span from start and end [Position].
    /// # Panics
    /// - If the end is smaller than the start.
    pub fn from_positions(start: Position, end: Position) -> Self {
        assert!(
            end >= start,
            "end position must be greater than or equal to the start."
        );

        Span {
            byte: start.byte..end.byte,
            char: start.char..end.char,
        }
    }

    /// Creates a new span from a byte and char range.
    pub fn new(byte: Range<usize>, char: Range<usize>) -> Self {
        Span { byte, char }
    }

    /// Gets the start position.
    pub fn start(&self) -> Position {
        Position {
            byte: self.byte.start,
            char: self.char.start,
        }
    }

    /// Gets the end position.
    pub fn end(&self) -> Position {
        Position {
            byte: self.byte.end,
            char: self.char.end,
        }
    }

    /// Gets the byte range.
    pub fn byte(&self) -> &Range<usize> {
        &self.byte
    }

    /// Gets the char range.
    pub fn char(&self) -> &Range<usize> {
        &self.char
    }

    /// Checks whether this span is empty.y
    pub fn is_empty(&self) -> bool {
        self.end() == self.start()
    }

    /// Gets the length of this span.
    pub fn len(&self) -> Position {
        self.end() - self.start()
    }

    /// Sets the start position.
    pub fn set_start(&mut self, start: Position) {
        self.byte.start = start.byte;
        self.char.start = start.char;
    }

    /// Sets the end position.
    pub fn set_end(&mut self, end: Position) {
        self.byte.end = end.byte;
        self.char.end = end.char;
    }

    /// Shift the span right by the specified amount.
    pub fn rshift(mut self, position: Position) -> Self {
        self.byte.start += position.byte;
        self.byte.end += position.byte;

        self.char.start += position.char;
        self.char.end += position.char;
        self
    }

    /// Shift the span left by the specified amount.
    /// Clips at zero if the resulting span would have a negative component.
    pub fn lshift(mut self, position: Position) -> Self {
        self.byte.start = self.byte.start.saturating_sub(position.byte);
        self.byte.end = self.byte.end.saturating_sub(position.byte);

        self.char.start = self.char.start.saturating_sub(position.char);
        self.char.end = self.char.end.saturating_sub(position.char);
        self
    }
}

/// Suggestion for change in a text.
#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Suggestion {
    source: String,
    message: String,
    span: Span,
    replacements: Vec<String>,
}

impl std::cmp::PartialEq for Suggestion {
    fn eq(&self, other: &Suggestion) -> bool {
        let a: HashSet<&String> = self.replacements().iter().collect();
        let b: HashSet<&String> = other.replacements().iter().collect();

        a.intersection(&b).count() > 0 && other.span() == self.span()
    }
}

impl Suggestion {
    pub(crate) fn new(
        source: String,
        message: String,
        span: Span,
        replacements: Vec<String>,
    ) -> Self {
        Suggestion {
            source,
            message,
            span,
            replacements,
        }
    }

    /// Returns an identifier of the source of this suggestion.
    pub fn source(&self) -> &str {
        self.source.as_str()
    }

    /// Returns a human-readable message.
    pub fn message(&self) -> &str {
        self.message.as_str()
    }

    /// Returns the suggested replacement options for the text.
    pub fn replacements(&self) -> &[String] {
        &self.replacements
    }

    /// Gets the span of this suggestion.
    pub fn span(&self) -> &Span {
        &self.span
    }

    /// Shift the span right by the specified amount.
    pub fn rshift(mut self, position: Position) -> Self {
        self.span = self.span.rshift(position);
        self
    }
}
