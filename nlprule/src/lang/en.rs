use super::ERROR_MSG;
use crate::{
    components::{
        chunker::Chunker,
        multiword_tagger::MultiwordTagger,
        rules::{Disambiguator, Rules},
        tokenizer::Tokenizer,
    },
    properties::{tokenize, CreatePipe, Pipeline},
};

pub type Analyzer = tokenize::Pipeline<(Tokenizer, MultiwordTagger, Chunker, Disambiguator)>;
pub type Correcter = Pipeline<(Analyzer, Rules)>;

pub fn tokenizer() -> Tokenizer {
    binary!(Tokenizer, "en", "tokenizer").expect(ERROR_MSG)
}

pub fn multiword_tagger() -> MultiwordTagger {
    binary!(MultiwordTagger, "en", "tokenizer").expect(ERROR_MSG)
}

pub fn chunker() -> Chunker {
    binary!(Chunker, "en", "chunker").expect(ERROR_MSG)
}

pub fn disambiguator() -> Disambiguator {
    binary!(Disambiguator, "en", "disambiguator").expect(ERROR_MSG)
}

pub fn rules() -> Rules {
    binary!(Rules, "en", "rules").expect(ERROR_MSG)
}

pub fn analyzer() -> Analyzer {
    tokenize::Pipeline::new((tokenizer(), multiword_tagger(), chunker(), disambiguator()))
        .expect(ERROR_MSG)
}

pub fn correcter() -> Correcter {
    Pipeline::new((analyzer(), rules())).expect(ERROR_MSG)
}
