use super::ERROR_MSG;
use crate::{
    components::{
        multiword_tagger::MultiwordTagger,
        rules::{Disambiguator, Rules},
        tokenizer::Tokenizer,
    },
    properties::{tokenize, CreatePipe, Pipeline},
};

pub type Analyzer = tokenize::Pipeline<(Tokenizer, MultiwordTagger, Disambiguator)>;
pub type Correcter = Pipeline<(Analyzer, Rules)>;

pub fn tokenizer() -> Tokenizer {
    binary!(Tokenizer, "es", "tokenizer").expect(ERROR_MSG)
}

pub fn multiword_tagger() -> MultiwordTagger {
    binary!(MultiwordTagger, "es", "multiword_tagger").expect(ERROR_MSG)
}

pub fn disambiguator() -> Disambiguator {
    binary!(Disambiguator, "es", "disambiguator").expect(ERROR_MSG)
}

pub fn rules() -> Rules {
    binary!(Rules, "es", "rules").expect(ERROR_MSG)
}

pub fn analyzer() -> Analyzer {
    tokenize::Pipeline::new((tokenizer(), multiword_tagger(), disambiguator())).expect(ERROR_MSG)
}

pub fn correcter() -> Correcter {
    Pipeline::new((analyzer(), rules())).expect(ERROR_MSG)
}
