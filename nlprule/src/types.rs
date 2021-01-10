//! Fundamental types used by this crate.

use derivative::Derivative;
use serde::{Deserialize, Serialize};
use std::borrow::Cow;

use crate::tokenizer::tag::Tagger;

/// Lemma and part-of-speech tag associated with a word.
#[derive(Debug, Clone, PartialEq)]
pub struct WordData<'t> {
    pub lemma: Cow<'t, str>,
    pub pos_id: u16,
}

impl<'t> WordData<'t> {
    pub fn new<S: Into<Cow<'t, str>>>(lemma: S, pos_id: u16) -> Self {
        WordData {
            lemma: lemma.into(),
            pos_id,
        }
    }

    pub fn to_owned_word_data(&self) -> OwnedWordData {
        OwnedWordData {
            lemma: self.lemma.to_string(),
            pos_id: self.pos_id,
        }
    }
}

/// An owned version of [WordData] for serialization and use in longer-living structures e. g. rule tests.
#[derive(Debug, Serialize, Deserialize, Hash, Eq, PartialEq)]
pub struct OwnedWordData {
    pub lemma: String,
    pub pos_id: u16,
}

impl OwnedWordData {
    pub fn new(lemma: String, pos_id: u16) -> Self {
        OwnedWordData { lemma, pos_id }
    }
}

/// Contains all the local information about a token i. e.
/// the text itself and the [WordData]s associated with the word.
#[derive(Debug, Clone, PartialEq)]
pub struct Word<'t> {
    pub text: &'t str,
    pub tags: Vec<WordData<'t>>,
}

impl<'t> Word<'t> {
    pub fn new_with_tags(text: &'t str, tags: Vec<WordData<'t>>) -> Self {
        Word { text, tags }
    }

    pub fn to_owned_word(&self) -> OwnedWord {
        OwnedWord {
            text: self.text.to_string(),
            tags: self.tags.iter().map(|x| x.to_owned_word_data()).collect(),
        }
    }
}

/// An owned version of [Word] for serialization and use in longer-living structures e. g. rule tests.
#[derive(Debug, Serialize, Deserialize)]
pub struct OwnedWord {
    pub text: String,
    pub tags: Vec<OwnedWordData>,
}

/// A token where varying levels of information are set.
#[derive(Derivative)]
#[derivative(Debug, Clone, PartialEq)]
pub struct IncompleteToken<'t> {
    pub word: Word<'t>,
    pub byte_span: (usize, usize),
    pub char_span: (usize, usize),
    pub is_sentence_end: bool,
    pub has_space_before: bool,
    pub chunks: Vec<String>,
    pub text: &'t str,
    #[derivative(PartialEq = "ignore", Debug = "ignore")]
    pub tagger: &'t Tagger,
}

impl<'t> AsRef<str> for IncompleteToken<'t> {
    fn as_ref(&self) -> &str {
        self.word.text
    }
}

/// A finished token with all information set.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Token<'t> {
    pub word: Word<'t>,
    pub char_span: (usize, usize),
    pub byte_span: (usize, usize),
    pub has_space_before: bool,
    pub chunks: Vec<String>,
    pub text: &'t str,
    #[derivative(Debug = "ignore")]
    pub tagger: &'t Tagger,
}

/// An owned version of [Token] for serialization and use in longer-living structures e. g. rule tests.
#[derive(Debug, Serialize, Deserialize)]
pub struct OwnedToken {
    pub word: OwnedWord,
    pub char_span: (usize, usize),
    pub byte_span: (usize, usize),
    pub has_space_before: bool,
    pub chunks: Vec<String>,
}

impl<'t> AsRef<str> for Token<'t> {
    fn as_ref(&self) -> &str {
        self.word.text
    }
}

impl<'t> Token<'t> {
    /// Get the special sentence start token.
    pub fn sent_start(text: &'t str, tagger: &'t Tagger) -> Self {
        Token {
            word: Word::new_with_tags(
                "",
                vec![WordData::new("", tagger.tag_to_id("SENT_START"))]
                    .into_iter()
                    .collect(),
            ),
            char_span: (0, 0),
            byte_span: (0, 0),
            has_space_before: false,
            chunks: Vec::new(),
            text,
            tagger,
        }
    }

    pub fn to_owned_token(&self) -> OwnedToken {
        OwnedToken {
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

        word.tags
            .push(WordData::new(data.word.text, data.tagger.tag_to_id("")));

        if word
            .tags
            .iter()
            .all(|x| data.tagger.id_to_tag(x.pos_id).is_empty())
        {
            word.tags.push(WordData::new(
                data.word.text,
                data.tagger.tag_to_id("UNKNOWN"),
            ));
        }

        if data.is_sentence_end {
            word.tags.push(WordData::new(
                data.word.text,
                data.tagger.tag_to_id("SENT_END"),
            ));
        }

        Token {
            word,
            byte_span: data.byte_span,
            char_span: data.char_span,
            has_space_before: data.has_space_before,
            chunks: data.chunks,
            text: data.text,
            tagger: data.tagger,
        }
    }
}

/// Suggestion for change in a text.
#[derive(Debug, Serialize, Deserialize)]
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
    pub text: Vec<String>,
}
