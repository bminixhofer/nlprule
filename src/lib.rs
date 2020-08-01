use thiserror::Error;
use unicode_segmentation::UnicodeSegmentation;

pub mod composition;
pub mod structure;
pub mod utils;

#[derive(Error, Debug)]
pub enum Error {
    #[error("unexpected condition: {0}")]
    Unexpected(String),
    #[error("feature not implemented: {0}")]
    Unimplemented(String),
}

fn is_word_boundary(input: char) -> bool {
    !(input.is_alphanumeric() || input == '-')
}

fn split<F>(input: &str, split_func: F) -> Vec<&str>
where
    F: Fn(char) -> bool,
{
    let mut out = Vec::new();
    let mut last_index = 0;

    for (i, character) in input.char_indices() {
        if split_func(character) {
            if last_index != i {
                out.push(&input[last_index..i]);
            }
            last_index = i + character.len_utf8();
            out.push(&input[i..last_index]);
        }
    }
    out.push(&input[last_index..]);

    out
}

#[derive(Debug)]
pub struct Token<'a> {
    pub text: &'a str,
    pub lower: String,
    pub char_span: (usize, usize),
    pub byte_span: (usize, usize),
    pub is_sentence_start: bool,
    pub is_sentence_end: bool,
}

impl<'a> Token<'a> {
    pub fn str_to_tokens(input: &'a str) -> Vec<Token<'a>> {
        let mut current_char = 0usize;

        input
            .unicode_sentences()
            .map(|sentence| {
                let splits = split(sentence, is_word_boundary);
                let split_len = splits.len();

                splits
                    .into_iter()
                    .enumerate()
                    .map(|(index, x)| {
                        let char_start = current_char;
                        current_char += x.chars().count();

                        let byte_start = x.as_ptr() as usize - input.as_ptr() as usize;

                        Token {
                            text: x.trim(),
                            lower: x.trim().to_lowercase(),
                            char_span: (char_start, current_char),
                            byte_span: (byte_start, byte_start + x.len()),
                            is_sentence_start: index == 0,
                            is_sentence_end: index == split_len - 1,
                        }
                    })
                    .filter(|token| !token.text.is_empty())
                    .collect::<Vec<_>>()
            })
            .flatten()
            .collect()
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
