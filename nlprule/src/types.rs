use serde::{Deserialize, Serialize};
use std::borrow::Cow;

#[derive(Debug, Clone, PartialEq)]
pub struct WordData<'t> {
    pub lemma: Cow<'t, str>,
    pub pos: &'t str,
}

impl<'t> WordData<'t> {
    pub fn new<S: Into<Cow<'t, str>>>(lemma: S, pos: &'t str) -> Self {
        WordData {
            lemma: lemma.into(),
            pos,
        }
    }

    pub fn to_owned_word_data(&self) -> OwnedWordData {
        OwnedWordData {
            lemma: self.lemma.to_string(),
            pos: self.pos.to_string(),
        }
    }
}

#[derive(Debug, Serialize, Deserialize, Hash, Eq, PartialEq)]
pub struct OwnedWordData {
    pub lemma: String,
    pub pos: String,
}

impl OwnedWordData {
    pub fn new(lemma: String, pos: String) -> Self {
        OwnedWordData { lemma, pos }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Word<'t> {
    pub text: &'t str,
    pub tags: Vec<WordData<'t>>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct OwnedWord {
    pub text: String,
    pub tags: Vec<OwnedWordData>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IncompleteToken<'t> {
    pub word: Word<'t>,
    pub byte_span: (usize, usize),
    pub char_span: (usize, usize),
    pub is_sentence_end: bool,
    pub has_space_before: bool,
    pub chunks: Vec<String>,
    pub text: &'t str,
}

impl<'t> AsRef<str> for IncompleteToken<'t> {
    fn as_ref(&self) -> &str {
        self.word.text
    }
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

#[derive(Debug)]
pub struct Token<'t> {
    pub word: Word<'t>,
    pub char_span: (usize, usize),
    pub byte_span: (usize, usize),
    pub has_space_before: bool,
    pub chunks: Vec<String>,
    pub text: &'t str,
}

#[derive(Debug)]
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
    pub fn sent_start(text: &'t str) -> Self {
        Token {
            word: Word::new_with_tags(
                "",
                vec![WordData::new("", "SENT_START")].into_iter().collect(),
            ),
            char_span: (0, 0),
            byte_span: (0, 0),
            has_space_before: false,
            chunks: Vec::new(),
            text,
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

        word.tags.push(WordData::new(data.word.text, ""));

        if word.tags.iter().all(|x| x.pos.is_empty()) {
            word.tags.push(WordData::new(data.word.text, "UNKNOWN"));
        }

        if data.is_sentence_end {
            word.tags.push(WordData::new(data.word.text, "SENT_END"));
        }

        Token {
            word,
            byte_span: data.byte_span,
            char_span: data.char_span,
            has_space_before: data.has_space_before,
            chunks: data.chunks,
            text: data.text,
        }
    }
}
