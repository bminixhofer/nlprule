//! Fundamental types used by this crate.

use crate::components::tagger::Tagger;
pub(crate) use crate::components::tagger::{PosId, PosIdInt, SpecialPos, WordId, WordIdInt};
use derivative::Derivative;
use lazy_static::lazy_static;
use serde::{Deserialize, Serialize};
use std::{
    cmp::Ordering,
    collections::{hash_map, HashMap, HashSet},
    ops::{Add, AddAssign, Range, Sub},
};

pub(crate) type DefaultHashMap<K, V> = HashMap<K, V>;
pub(crate) type DefaultHashSet<T> = HashSet<T>;
pub(crate) type DefaultHasher = hash_map::DefaultHasher;

/// A sentence containing one or more [Token]s.
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
    /// Creates a new sentence.
    pub(crate) fn new(tokens: Vec<Token<'t>>, text: &'t str, tagger: &'t Tagger) -> Self {
        Sentence {
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
    pub fn iter_mut(&mut self) -> impl DoubleEndedIterator<Item = &mut Token<'t>> {
        self.tokens.iter_mut()
    }

    /// Returns an iterator over tokens by reference.
    pub fn iter(&self) -> impl DoubleEndedIterator<Item = &Token> {
        self.tokens.iter()
    }

    /// Gets the tokens in this sentence.
    pub fn tokens(&self) -> &[Token<'t>] {
        &self.tokens
    }

    /// Gets the first token in this sentence. There is always at least one token in the sentence
    /// so this will never panic.
    pub fn first(&self) -> &Token<'t> {
        &self.tokens[0]
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

/// Lemma and part-of-speech tag associated with a word.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
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
    pub fn freeze(mut self) -> Self {
        self.frozen = true;
        self
    }

    /// Checks whether the data is frozen i.e. whether it can never be removed from a word once added.
    pub fn frozen(&self) -> bool {
        self.frozen
    }

    /// Converts this struct to a struct with `'static` lifetime by cloning borrowed data.
    pub fn into_static(self) -> WordData<'static> {
        WordData {
            lemma: self.lemma.into_static(),
            pos: self.pos.into_static(),
            frozen: self.frozen,
        }
    }
}

lazy_static! {
    static ref UNKNOWN_DATA: WordData<'static> =
        WordData::new(WordId::empty(), PosId::special(SpecialPos::Unknown));
}

struct TagIter<'a, 't> {
    iter: std::slice::Iter<'a, WordData<'t>>,
    is_empty: Option<bool>,
}

impl<'a, 't> Iterator for TagIter<'a, 't> {
    type Item = &'a WordData<'t>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(x) = self.iter.next() {
            if *x.pos() == PosId::special(SpecialPos::SentStart) {
                self.is_empty = None;
            }

            return Some(x);
        }

        if let Some(true) = self.is_empty.take() {
            return Some(&*UNKNOWN_DATA);
        }

        None
    }
}

/// Contains all the local information about a token i. e.
/// the text itself and the [WordData]s associated with the word.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct Tags<'t> {
    id: WordId<'t>,
    tags: Vec<WordData<'t>>,
}

impl<'t> Tags<'t> {
    /// Creates new [Tags].
    pub fn new(id: WordId<'t>, tags: Vec<WordData<'t>>) -> Self {
        Tags { id, tags }
    }

    pub fn id(&self) -> &WordId<'t> {
        &self.id
    }

    /// Multiple pairs of (lemma, part-of-speech) associated with this token.
    /// Order is in general not significant.
    pub fn iter(&self) -> impl Iterator<Item = &WordData<'t>> {
        TagIter {
            iter: self.tags.iter(),
            is_empty: Some(self.is_empty()),
        }
    }

    /// Checks if there are no ordinary part-of-speech tags. If this is true, this does not imply that `.iter()` will return
    /// an empty iterator as special tags (e.g. `UNKNOWN`) are not considered in `is_empty()`.
    pub fn is_empty(&self) -> bool {
        self.tags.iter().all(|x| x.pos().is_special())
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

    /// Converts this struct to a struct with `'static` lifetime by cloning borrowed data.
    pub fn into_static(self) -> Tags<'static> {
        Tags {
            id: self.id.into_static(),
            tags: self.tags.into_iter().map(|x| x.into_static()).collect(),
        }
    }
}

lazy_static! {
    pub(crate) static ref SENT_START: Token<'static> = Token {
        text: "",
        span: Span::default(),
        is_sentence_start: false, // `is_sentence_start` marks the first *real* token in the sentence.
        is_sentence_end: false,
        has_space_before: false,
        tags: Some(Tags::new(
            WordId::empty(),
            vec![WordData::new(
                WordId::empty(),
                PosId::special(SpecialPos::SentStart),
            )],
        )),
        chunks: Some(Vec::new()),
    };
}

/// A token where varying levels of information are set.
#[derive(Debug, Clone, PartialEq)]
pub struct Token<'t> {
    text: &'t str,
    span: Span,
    is_sentence_start: bool,
    is_sentence_end: bool,
    has_space_before: bool,
    pub tags: Option<Tags<'t>>,
    pub chunks: Option<Vec<String>>,
}

impl<'t> Token<'t> {
    pub(crate) fn new(
        text: &'t str,
        span: Span,
        is_sentence_start: bool,
        is_sentence_end: bool,
        has_space_before: bool,
    ) -> Self {
        Token {
            text,
            span,
            is_sentence_start,
            is_sentence_end,
            has_space_before,
            tags: None,
            chunks: None,
        }
    }

    /// Gets the token as string.
    pub fn as_str(&self) -> &'t str {
        self.text
    }

    /// The span of this sentence.
    pub fn span(&self) -> &Span {
        &self.span
    }

    /// Whether this token is the first token in the sentence.
    pub fn is_sentence_start(&self) -> bool {
        self.is_sentence_start
    }

    /// Whether this token is the last token in the sentence.
    pub fn is_sentence_end(&self) -> bool {
        self.is_sentence_end
    }

    /// Whether this token has one or more whitespace characters before.
    pub fn has_space_before(&self) -> bool {
        self.has_space_before
    }

    /// Shift the span of this token right by the specified amount.
    pub fn rshift(mut self, position: Position) -> Self {
        self.span = self.span.rshift(position);
        self
    }
}

impl<'t> Token<'t> {
    /// The tags of this token. Contain information about the part-of-speech tags and lemmas.
    pub fn tags(&self) -> Result<&Tags<'t>, crate::Error> {
        self.tags.as_ref().ok_or(crate::Error::Unset("tags"))
    }

    pub fn tags_mut(&mut self) -> Result<&mut Tags<'t>, crate::Error> {
        self.tags.as_mut().ok_or(crate::Error::Unset("tags"))
    }

    pub fn chunks(&self) -> Result<&[String], crate::Error> {
        self.chunks.as_deref().ok_or(crate::Error::Unset("chunks"))
    }

    pub fn chunks_mut(&mut self) -> Result<&mut Vec<String>, crate::Error> {
        self.chunks.as_mut().ok_or(crate::Error::Unset("chunks"))
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

    /// Shift the span left by the specified amount.
    pub fn lshift(mut self, position: Position) -> Self {
        self.span = self.span.lshift(position);
        self
    }
}
