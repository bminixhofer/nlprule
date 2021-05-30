use super::ERROR_MSG;
use crate::{
    components::{
        rules::{Disambiguator, Rules},
        tokenizer::Tokenizer,
    },
    properties::{tokenize, CreatePipe, Pipeline},
};

pub type Analyzer = tokenize::Pipeline<(Tokenizer, Disambiguator)>;
pub type Correcter = Pipeline<(Analyzer, Rules)>;

pub fn tokenizer() -> Tokenizer {
    binary!(Tokenizer, "de", "tokenizer").expect(ERROR_MSG)
}

pub fn disambiguator() -> Disambiguator {
    binary!(Disambiguator, "de", "disambiguator").expect(ERROR_MSG)
}

pub fn rules() -> Rules {
    binary!(Rules, "de", "rules").expect(ERROR_MSG)
}

pub fn analyzer() -> Analyzer {
    tokenize::Pipeline::new((tokenizer(), disambiguator())).expect(ERROR_MSG)
}

pub fn correcter() -> Correcter {
    Pipeline::new((analyzer(), rules())).expect(ERROR_MSG)
}
