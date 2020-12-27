use flate2::read::GzDecoder;
use nlprule_core::{
    rule::Rules,
    tokenizer::{finalize, tag::Tagger, Token},
};
use nlprule_core::{
    rule::Suggestion,
    tokenizer::{IncompleteToken, Tokenizer, TokenizerOptions},
};
use pyo3::exceptions::ValueError;
use pyo3::prelude::*;
use std::{
    fs::{self, File},
    io::{BufReader, Cursor, Read},
    path::PathBuf,
    sync::Arc,
};

fn get_resource(code: &str, name: &str) -> PyResult<impl Read> {
    let version = env!("CARGO_PKG_VERSION");
    let mut cache_path: Option<PathBuf> = None;

    // try to find a file at which to cache the data
    if let Some(project_dirs) = directories::ProjectDirs::from("", "", "nlprule") {
        let cache_dir = project_dirs.cache_dir();

        cache_path = Some(
            cache_dir.join(version).join(code).join(
                name.strip_suffix(".gz")
                    .expect("resource name must have .gz ending."),
            ),
        );
    }

    // if the file can be read, the data is already cached
    if let Some(path) = &cache_path {
        if let Ok(bytes) = fs::read(path) {
            return Ok(Cursor::new(bytes));
        }
    }

    // ... otherwise, request the data from the URL ...
    let bytes = reqwest::blocking::get(&format!(
        "https://github.com/bminixhofer/nlprule/raw/{}/storage/{}/{}",
        env!("CARGO_PKG_VERSION"),
        code,
        name
    ))
    .and_then(|x| x.bytes())
    .map_err(|x| ValueError::py_err(format!("{}", x)))?;

    let mut gz = GzDecoder::new(&bytes[..]);
    let mut buffer = Vec::new();
    gz.read_to_end(&mut buffer).expect("gunzipping failed");

    // ... and then cache the data at the provided file, if one was found
    if let Some(path) = &cache_path {
        fs::create_dir_all(path.parent().unwrap())?;
        fs::write(path, &buffer)?;
    }

    Ok(Cursor::new(buffer))
}

#[pyclass(name = Tagger)]
pub struct PyTagger {
    tagger: Arc<Tagger>,
    options: TokenizerOptions,
}

impl PyTagger {
    fn new(tagger: Arc<Tagger>, options: TokenizerOptions) -> Self {
        PyTagger { tagger, options }
    }
}

#[pymethods]
impl PyTagger {
    fn get_tags(&self, word: &str, add_lower: bool) -> Vec<(String, String)> {
        self.tagger
            .get_tags(word, add_lower, self.options.use_compound_split_heuristic)
            .into_iter()
            .map(|x| (x.lemma, x.pos))
            .collect()
    }

    fn get_group_members(&self, word: &str) -> Vec<&str> {
        self.tagger.get_group_members(&word.to_string())
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

#[pyclass(name = Suggestion)]
struct PySuggestion {
    suggestion: Suggestion,
}

#[pymethods]
impl PySuggestion {
    #[getter]
    fn start(&self) -> usize {
        self.suggestion.start
    }

    #[getter]
    fn end(&self) -> usize {
        self.suggestion.end
    }

    #[getter]
    fn text(&self) -> Vec<&str> {
        self.suggestion.text.iter().map(|x| x.as_str()).collect()
    }
}

impl From<Suggestion> for PySuggestion {
    fn from(suggestion: Suggestion) -> Self {
        PySuggestion { suggestion }
    }
}

#[pyclass(name = Tokenizer)]
pub struct PyTokenizer {
    tokenizer: Tokenizer,
}

impl PyTokenizer {
    fn tokenizer(&self) -> &Tokenizer {
        &self.tokenizer
    }
}

#[pymethods]
impl PyTokenizer {
    #[staticmethod]
    fn load(code: &str) -> PyResult<Self> {
        let bytes = get_resource(code, "tokenizer.bin.gz")?;

        let tokenizer: Tokenizer =
            bincode::deserialize_from(bytes).map_err(|x| ValueError::py_err(format!("{}", x)))?;
        Ok(PyTokenizer { tokenizer })
    }

    #[new]
    fn new(path: &str) -> PyResult<Self> {
        let reader = BufReader::new(File::open(path).unwrap());
        let tokenizer: Tokenizer = bincode::deserialize_from(reader).unwrap();

        Ok(PyTokenizer { tokenizer })
    }

    #[getter]
    fn tagger(&self) -> PyTagger {
        PyTagger::new(
            self.tokenizer.tagger().clone(),
            (*self.tokenizer.options()).clone(),
        )
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

#[pyclass(name = Rules)]
struct PyRules {
    rules: Rules,
    tokenizer: Py<PyTokenizer>,
}

#[pymethods]
impl PyRules {
    #[staticmethod]
    fn load(code: &str, tokenizer: Py<PyTokenizer>) -> PyResult<Self> {
        let bytes = get_resource(code, "rules.bin.gz")?;

        let rules: Rules =
            bincode::deserialize_from(bytes).map_err(|x| ValueError::py_err(format!("{}", x)))?;
        Ok(PyRules { rules, tokenizer })
    }

    #[new]
    fn new(path: &str, tokenizer: Py<PyTokenizer>) -> PyResult<Self> {
        let reader = BufReader::new(File::open(path).unwrap());
        let rules: Rules = bincode::deserialize_from(reader).unwrap();

        Ok(PyRules { rules, tokenizer })
    }

    fn suggest(&self, py: Python, text: &str) -> Vec<PySuggestion> {
        let tokenizer = self.tokenizer.borrow(py);
        let tokenizer = tokenizer.tokenizer();

        let tokens = finalize(tokenizer.disambiguate(tokenizer.tokenize(text)));
        self.rules
            .apply(&tokens)
            .into_iter()
            .map(|x| x.into())
            .collect()
    }

    fn correct(&self, py: Python, text: &str) -> String {
        let tokenizer = self.tokenizer.borrow(py);
        let tokenizer = tokenizer.tokenizer();

        let tokens = finalize(tokenizer.disambiguate(tokenizer.tokenize(text)));
        let suggestions = self.rules.apply(&tokens);

        Rules::correct(text, &suggestions)
    }
}

#[pymodule]
fn nlprule(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add("__version__", env!("CARGO_PKG_VERSION"))?;
    m.add_class::<PyTokenizer>()?;
    m.add_class::<PyRules>()?;

    Ok(())
}
