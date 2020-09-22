use lazy_static::lazy_static;
use onig::Regex;
use std::collections::HashSet;
use unicode_segmentation::UnicodeSegmentation;

mod chunk;
mod disambiguate;
mod tag;

use chunk::Chunker;
use disambiguate::Disambiguator;
use tag::Tagger;

// chunker can not be shared across threads, so cant be lazy static
thread_local! {
    pub static CHUNKER: Chunker = Chunker::new().unwrap();
}

lazy_static! {
    pub static ref DISAMBIGUATOR: Disambiguator = {
        Disambiguator::from_xml(format!(
            "data/disambiguation.{}.canonic.xml",
            std::env::var("RULE_LANG").unwrap()
        ))
    };
}

lazy_static! {
    pub static ref TAGGER: Tagger = Tagger::from_dumps(format!(
        "data/dumps/{}",
        std::env::var("RULE_LANG").unwrap()
    ))
    .unwrap();
}

// see https://stackoverflow.com/a/40296745
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

fn get_token_strs(text: &str) -> Vec<&str> {
    let mut tokens = Vec::new();

    lazy_static! {
        // see https://stackoverflow.com/a/3809435 and https://regexr.com/3e6m0
        static ref URL_REGEX: Regex = Regex::new(r"(http(s)?://.)?(www\.)?[-a-zA-Z0-9@:%._\+~#=]{2,256}\.[a-z]{2,6}\b([-a-zA-Z0-9@:%_\+.~#?&//=]*)").unwrap();
    }

    let mut prev = 0;
    let split_func = |c: char| c.is_whitespace() || r##"'’`´‘],.:!?/\()<=>„“”"+#…*"##.contains(c);

    for (start, end) in URL_REGEX.find_iter(text) {
        tokens.extend(split(&text[prev..start], split_func));
        tokens.push(&text[start..end]);
        prev = end;
    }

    tokens.extend(split(&text[prev..text.len()], split_func));

    tokens
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct WordData {
    pub lemma: String,
    pub pos: String,
}

impl WordData {
    pub fn new(lemma: String, pos: String) -> Self {
        WordData { lemma, pos }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Word {
    pub text: String,
    pub tags: HashSet<WordData>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IncompleteToken {
    pub word: Word,
    pub byte_span: (usize, usize),
    pub char_span: (usize, usize),
    pub is_sentence_end: bool,
    pub has_space_before: bool,
    pub chunks: Vec<String>,
}

impl<'a> From<IncompleteToken> for Token {
    fn from(data: IncompleteToken) -> Token {
        let mut inflections: Vec<_> = data.word.tags.iter().map(|x| x.lemma.clone()).collect();
        inflections.push(data.word.text.to_string());

        let lower_inflections = inflections.iter().map(|x| x.to_lowercase()).collect();
        let mut postags: Vec<_> = data.word.tags.iter().map(|x| x.pos.clone()).collect();

        if postags.is_empty() {
            postags = vec!["UNKNOWN".to_string()];
        }

        if data.is_sentence_end {
            postags.push("SENT_END".to_string());
        }

        Token {
            lower: data.word.text.to_lowercase(),
            text: data.word.text,
            byte_span: data.byte_span,
            char_span: data.char_span,
            inflections,
            lower_inflections,
            postags,
            has_space_before: data.has_space_before,
            chunks: data.chunks,
        }
    }
}

impl Word {
    pub fn new_with_tags(text: String, tags: HashSet<WordData>) -> Self {
        Word { text, tags }
    }

    pub fn new(text: String) -> Self {
        Word {
            tags: TAGGER.get_tags(&text),
            text,
        }
    }
}

#[derive(Debug)]
pub struct Token {
    pub text: String,
    pub lower: String,
    pub inflections: Vec<String>,
    pub lower_inflections: Vec<String>,
    pub postags: Vec<String>,
    pub char_span: (usize, usize),
    pub byte_span: (usize, usize),
    pub has_space_before: bool,
    pub chunks: Vec<String>,
}

impl<'a> Token {
    fn sent_start() -> Token {
        Token {
            text: String::new(),
            inflections: Vec::new(),
            lower_inflections: Vec::new(),
            lower: String::new(),
            postags: vec!["SENT_START".to_string()],
            char_span: (0, 0),
            byte_span: (0, 0),
            has_space_before: false,
            chunks: Vec::new(),
        }
    }
}

pub fn finalize(tokens: Vec<IncompleteToken>) -> Vec<Token> {
    let mut finalized = vec![Token::sent_start()];
    finalized.extend(tokens.into_iter().map(|x| x.into()));

    finalized
}

pub fn disambiguate_up_to_id(mut tokens: Vec<IncompleteToken>, id: &str) -> Vec<IncompleteToken> {
    DISAMBIGUATOR.apply_up_to_id(&mut tokens, id);
    tokens
}

pub fn disambiguate(mut tokens: Vec<IncompleteToken>) -> Vec<IncompleteToken> {
    DISAMBIGUATOR.apply(&mut tokens);
    tokens
}

pub fn tokenize(text: &str) -> Vec<IncompleteToken> {
    let sentence_indices = text
        .unicode_sentences()
        .map(|sentence| {
            let ptr = sentence.as_ptr() as usize;
            (ptr, ptr + sentence.len())
        })
        .fold((HashSet::new(), HashSet::new()), |mut a, x| {
            a.0.insert(x.0);
            a.1.insert(x.1);
            a
        });

    let mut current_char = 0;
    let mut tokens = Vec::new();

    tokens.extend(
        get_token_strs(text)
            .into_iter()
            .map(|x| {
                let char_start = current_char;
                let ptr = x.as_ptr() as usize;
                current_char += x.chars().count();

                let byte_start = x.as_ptr() as usize - text.as_ptr() as usize;
                let trimmed = x.trim();

                IncompleteToken {
                    word: Word::new(trimmed.to_string()),
                    char_span: (char_start, current_char),
                    byte_span: (byte_start, byte_start + x.len()),
                    is_sentence_end: sentence_indices.1.contains(&(ptr + x.len())),
                    has_space_before: text[..byte_start].ends_with(char::is_whitespace),
                    chunks: Vec::new(),
                }
            })
            .filter(|token| !token.word.text.is_empty()),
    );

    CHUNKER.with(|x| {
        x.apply(text, &mut tokens).unwrap();
    });
    tokens
}
