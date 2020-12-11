use std::sync::Arc;

use nlprule::tokenizer::{finalize, tag::Tagger, Token, TokenizerOptions};
use nlprule::tokenizer::{IncompleteToken, Tokenizer};
use pyo3::prelude::*;

#[pyclass(name = Tagger)]
pub struct PyTagger {
    tagger: Arc<Tagger>,
}

#[pymethods]
impl PyTagger {
    #[new]
    fn new(paths: Vec<&str>, remove_paths: Vec<&str>) -> PyResult<Self> {
        Ok(PyTagger {
            tagger: Arc::new(Tagger::from_dumps(&paths, &remove_paths)?),
        })
    }
}

impl PyTagger {
    pub fn tagger(&self) -> &Arc<Tagger> {
        &self.tagger
    }
}

#[pyclass(name = IncompleteTokens)]
#[derive(Clone)]
pub struct PyIncompleteTokens {
    tokens: Vec<IncompleteToken>,
}

impl From<Vec<IncompleteToken>> for PyIncompleteTokens {
    fn from(tokens: Vec<IncompleteToken>) -> Self {
        PyIncompleteTokens { tokens }
    }
}

impl From<PyIncompleteTokens> for Vec<IncompleteToken> {
    fn from(token: PyIncompleteTokens) -> Self {
        token.tokens
    }
}

#[pyclass(name = Token)]
pub struct PyToken {
    token: Token,
}

impl From<Token> for PyToken {
    fn from(token: Token) -> Self {
        PyToken { token }
    }
}

#[pymethods]
impl PyToken {
    #[getter]
    fn text(&self) -> &str {
        &self.token.word.text
    }

    #[getter]
    fn span(&self) -> (usize, usize) {
        self.token.char_span
    }

    #[getter]
    fn tags(&self) -> Vec<(&str, &str)> {
        self.token
            .word
            .tags
            .iter()
            .map(|x| (x.lemma.as_str(), x.pos.as_str()))
            .collect()
    }
}

#[pyclass(name = Tokenizer)]
pub struct PyTokenizer {
    tokenizer: Tokenizer,
}

#[pymethods]
impl PyTokenizer {
    #[new]
    fn new(path: &str, tagger: &PyTagger) -> PyResult<Self> {
        Ok(PyTokenizer {
            tokenizer: Tokenizer::from_xml(
                path,
                (*tagger).tagger().clone(),
                None,
                TokenizerOptions::default(),
            )
            .unwrap(),
        })
    }

    fn tokenize(&self, text: &str) -> PyIncompleteTokens {
        self.tokenizer.tokenize(text).into()
    }

    fn disambiguate(&self, tokens: PyIncompleteTokens) -> PyIncompleteTokens {
        self.tokenizer.disambiguate(tokens.into()).into()
    }

    fn finalize(&self, tokens: PyIncompleteTokens) -> Vec<PyToken> {
        finalize(tokens.into())
            .into_iter()
            .map(|x| x.into())
            .collect()
    }

    fn apply(&self, text: &str) -> Vec<PyToken> {
        self.finalize(self.disambiguate(self.tokenize(text)))
    }
}

#[pymodule]
fn pynlprule(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add("__version__", env!("CARGO_PKG_VERSION"))?;
    m.add_class::<PyTagger>()?;
    m.add_class::<PyTokenizer>()?;

    Ok(())
}
