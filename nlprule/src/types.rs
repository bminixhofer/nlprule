//! Fundamental types used by this crate.

use derivative::Derivative;
use serde::{Deserialize, Serialize};
use std::{
    borrow::Cow,
    collections::{hash_map, HashMap, HashSet},
};

use crate::tokenizer::tag::Tagger;

pub(crate) type DefaultHashMap<K, V> = HashMap<K, V>;
pub(crate) type DefaultHashSet<T> = HashSet<T>;
pub(crate) type DefaultHasher = hash_map::DefaultHasher;

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

    #[derive(Debug, Serialize, Deserialize, Hash, Eq, PartialEq)]
    pub struct WordId(pub(crate) String, pub(crate) Option<WordIdInt>);

    impl WordId {
        pub fn as_ref_id(&self) -> super::WordId {
            super::WordId(self.0.as_str().into(), self.1)
        }
    }

    impl AsRef<str> for WordId {
        fn as_ref(&self) -> &str {
            self.0.as_ref()
        }
    }

    #[derive(Debug, Serialize, Deserialize, Hash, Eq, PartialEq)]
    pub struct PosId(pub(crate) String, pub(crate) PosIdInt);

    impl PosId {
        pub fn as_ref_id(&self) -> super::PosId {
            super::PosId(self.0.as_str(), self.1)
        }
    }

    impl AsRef<str> for PosId {
        fn as_ref(&self) -> &str {
            self.0.as_ref()
        }
    }

    #[derive(Debug, Serialize, Deserialize, Hash, Eq, PartialEq)]
    pub struct WordData {
        pub lemma: WordId,
        pub pos: PosId,
    }

    impl WordData {
        pub fn new(lemma: WordId, pos_id: PosId) -> Self {
            WordData { lemma, pos: pos_id }
        }
    }

    #[derive(Debug, Serialize, Deserialize)]
    pub struct Word {
        pub text: WordId,
        pub tags: Vec<WordData>,
    }

    #[derive(Debug, Serialize, Deserialize)]
    pub struct Token {
        pub word: Word,
        pub char_span: (usize, usize),
        pub byte_span: (usize, usize),
        pub has_space_before: bool,
        pub chunks: Vec<String>,
    }
}

/// A potentially identified word. If it is identified as a known word, many optimizations can be applied.
#[derive(Debug, Clone, PartialEq)]
pub struct WordId<'t>(pub(crate) Cow<'t, str>, pub(crate) Option<WordIdInt>);

impl<'t> WordId<'t> {
    pub(crate) fn to_owned_id(&self) -> owned::WordId {
        owned::WordId(self.0.to_string(), self.1)
    }

    pub(crate) fn id(&self) -> &Option<WordIdInt> {
        &self.1
    }
}

impl<'t> AsRef<str> for WordId<'t> {
    fn as_ref(&self) -> &str {
        self.0.as_ref()
    }
}

/// An identified part-of-speech tag. POS tags are treated as a closed set so every POS tag is identified.
#[derive(Debug, Clone, PartialEq)]
pub struct PosId<'t>(pub(crate) &'t str, pub(crate) PosIdInt);

impl<'t> PosId<'t> {
    pub fn to_owned_id(&self) -> owned::PosId {
        owned::PosId(self.0.to_string(), self.1)
    }

    pub(crate) fn id(&self) -> &PosIdInt {
        &self.1
    }
}

impl<'t> AsRef<str> for PosId<'t> {
    fn as_ref(&self) -> &str {
        self.0
    }
}

/// Lemma and part-of-speech tag associated with a word.
#[derive(Debug, Clone, PartialEq)]
pub struct WordData<'t> {
    pub lemma: WordId<'t>,
    pub pos: PosId<'t>,
}

impl<'t> WordData<'t> {
    pub fn new(lemma: WordId<'t>, pos: PosId<'t>) -> Self {
        WordData { lemma, pos }
    }

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
    pub text: WordId<'t>,
    pub tags: Vec<WordData<'t>>,
}

impl<'t> Word<'t> {
    pub fn new_with_tags(text: WordId<'t>, tags: Vec<WordData<'t>>) -> Self {
        Word { text, tags }
    }

    pub fn to_owned_word(&self) -> owned::Word {
        owned::Word {
            text: self.text.to_owned_id(),
            tags: self.tags.iter().map(|x| x.to_owned_word_data()).collect(),
        }
    }
}

/// A token where varying levels of information are set.
#[derive(Derivative)]
#[derivative(Debug, PartialEq)]
#[derive(Clone)]
pub struct IncompleteToken<'t> {
    pub word: Word<'t>,
    pub byte_span: (usize, usize),
    pub char_span: (usize, usize),
    pub is_sentence_end: bool,
    pub has_space_before: bool,
    pub chunks: Vec<String>,
    pub multiword_data: Option<WordData<'t>>,
    pub sentence: &'t str,
    #[derivative(PartialEq = "ignore", Debug = "ignore")]
    pub tagger: &'t Tagger,
}

/// A token to which disambiguation rules have been applied to.
#[derive(Derivative)]
#[derivative(Debug, PartialEq)]
#[derive(Clone)]
pub struct DisambiguatedToken<'t>(pub IncompleteToken<'t>);

/// A finished token with all information set.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Token<'t> {
    pub word: Word<'t>,
    pub char_span: (usize, usize),
    pub byte_span: (usize, usize),
    pub has_space_before: bool,
    pub chunks: Vec<String>,
    pub sentence: &'t str,
    #[derivative(Debug = "ignore")]
    pub tagger: &'t Tagger,
}

impl<'t> Token<'t> {
    /// Get the special sentence start token.
    pub fn sent_start(sentence: &'t str, tagger: &'t Tagger) -> Self {
        Token {
            word: Word::new_with_tags(
                tagger.id_word("".into()),
                vec![WordData::new(
                    tagger.id_word("".into()),
                    tagger.id_tag("SENT_START"),
                )]
                .into_iter()
                .collect(),
            ),
            char_span: (0, 0),
            byte_span: (0, 0),
            has_space_before: false,
            chunks: Vec::new(),
            sentence,
            tagger,
        }
    }

    pub fn to_owned_token(&self) -> owned::Token {
        owned::Token {
            word: self.word.to_owned_word(),
            char_span: self.char_span,
            byte_span: self.byte_span,
            has_space_before: self.has_space_before,
            chunks: self.chunks.clone(),
        }
    }
}

impl<'t> From<IncompleteToken<'t>> for Token<'t> {
    fn from(data: IncompleteToken<'t>) -> Self {
        let mut word = data.word.clone();

        word.tags.push(WordData::new(
            data.word.text.clone(),
            data.tagger.id_tag(""),
        ));

        // multiword tags are added last because they can not be touched by disambiguation
        word.tags.extend(data.multiword_data.into_iter());

        if word.tags.iter().all(|x| x.pos.0.is_empty()) {
            word.tags.push(WordData::new(
                data.word.text.clone(),
                data.tagger.id_tag("UNKNOWN"),
            ));
        }

        if data.is_sentence_end {
            word.tags.push(WordData::new(
                data.word.text,
                data.tagger.id_tag("SENT_END"),
            ));
        }

        Token {
            word,
            byte_span: data.byte_span,
            char_span: data.char_span,
            has_space_before: data.has_space_before,
            chunks: data.chunks,
            sentence: data.sentence,
            tagger: data.tagger,
        }
    }
}

/// Suggestion for change in a text.
#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Suggestion {
    /// The ID of the rule this suggestion is from.
    pub source: String,
    /// A human-readable message.
    pub message: String,
    /// The start character index (inclusive).
    pub start: usize,
    /// The end character index (exclusive).
    pub end: usize,
    /// The suggested replacement options for the text.
    pub replacements: Vec<String>,
}

impl Suggestion {
    /// Shift `start` and `end` to the right by the specified amount.
    pub fn rshift(&mut self, offset: usize) {
        self.start += offset;
        self.end += offset;
    }
}
