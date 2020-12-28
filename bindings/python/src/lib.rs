use flate2::read::GzDecoder;
use nlprule_core::{
    rule::Rules,
    tokenizer::{finalize, tag::Tagger, Token},
};
use nlprule_core::{
    rule::Suggestion,
    tokenizer::{IncompleteToken, Tokenizer, TokenizerOptions},
};
use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;
use pyo3::types::PyString;
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
    .map_err(|x| PyValueError::new_err(format!("{}", x)))?;

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

#[pyclass()]
pub struct SplitOn {
    split_chars: Vec<char>,
}

#[pymethods]
impl SplitOn {
    #[new]
    fn new(split_chars: Vec<&str>) -> PyResult<Self> {
        Ok(SplitOn {
            split_chars: split_chars
                .iter()
                .map(|x| {
                    let chars: Vec<_> = x.chars().collect();
                    if chars.len() != 1 {
                        Err(PyValueError::new_err(
                            "split_chars must consist of strings with exactly one character.",
                        ))
                    } else {
                        Ok(chars[0])
                    }
                })
                .collect::<PyResult<_>>()?,
        })
    }

    #[call]
    fn __call__<'a>(&self, texts: Vec<&'a str>) -> Vec<Vec<&'a str>> {
        let mut output = Vec::new();

        for text in texts {
            let mut sentences = Vec::new();
            let mut start = 0;

            for (i, c) in text.char_indices() {
                if self.split_chars.iter().any(|x| *x == c) {
                    let end = i + c.len_utf8();
                    sentences.push(&text[start..end]);
                    start = end;
                }
            }

            if start != text.len() {
                sentences.push(&text[start..]);
            }
            output.push(sentences);
        }

        output
    }
}

#[pyclass(name = "Tagger")]
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

#[pyclass(name = "IncompleteTokens")]
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

#[pyclass(name = "Token")]
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

#[pyclass(name = "Suggestion")]
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

#[pyclass(name = "Tokenizer")]
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

        let tokenizer: Tokenizer = bincode::deserialize_from(bytes)
            .map_err(|x| PyValueError::new_err(format!("{}", x)))?;
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

#[pyclass(name = "Rules")]
struct PyRules {
    rules: Rules,
    tokenizer: Py<PyTokenizer>,
    sentence_splitter: Option<PyObject>,
}

#[pymethods]
impl PyRules {
    #[staticmethod]
    fn load(
        code: &str,
        tokenizer: Py<PyTokenizer>,
        sentence_splitter: Option<PyObject>,
    ) -> PyResult<Self> {
        let bytes = get_resource(code, "rules.bin.gz")?;

        let rules: Rules = bincode::deserialize_from(bytes)
            .map_err(|x| PyValueError::new_err(format!("{}", x)))?;
        Ok(PyRules {
            rules,
            tokenizer,
            sentence_splitter,
        })
    }

    #[new]
    fn new(
        path: &str,
        tokenizer: Py<PyTokenizer>,
        sentence_splitter: Option<PyObject>,
    ) -> PyResult<Self> {
        let reader = BufReader::new(File::open(path).unwrap());
        let rules: Rules = bincode::deserialize_from(reader).unwrap();

        Ok(PyRules {
            rules,
            tokenizer,
            sentence_splitter,
        })
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

    fn correct(&self, py: Python, text_or_texts: PyObject) -> PyResult<PyObject> {
        let text_or_texts = text_or_texts.as_ref(py);
        let is_iterable =
            text_or_texts.hasattr("__iter__")? && !text_or_texts.is_instance::<PyString>()?;

        let texts: Vec<String> = if is_iterable {
            text_or_texts.extract()?
        } else {
            vec![text_or_texts.extract()?]
        };

        if let Some(sentence_splitter) = &self.sentence_splitter {
            let tokenizer = self.tokenizer.borrow(py);
            let tokenizer = tokenizer.tokenizer();

            let mut output = Vec::new();

            for sentences in sentence_splitter
                .as_ref(py)
                .call1((texts,))?
                .extract::<Vec<Vec<String>>>()?
            {
                output.push(
                    sentences
                        .iter()
                        .map(|x| {
                            let tokens = finalize(tokenizer.disambiguate(tokenizer.tokenize(x)));
                            let suggestions = self.rules.apply(&tokens);
                            Rules::correct(x, &suggestions)
                        })
                        .collect::<Vec<_>>()
                        .join(""),
                );
            }

            Ok(if is_iterable {
                output.to_object(py)
            } else {
                output[0].to_object(py)
            })
        } else {
            Err(PyValueError::new_err(
                "sentence_splitter must be set. Use .correct_sentence to correct one sentence.",
            ))
        }
    }
}

#[pymodule]
fn nlprule(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add("__version__", env!("CARGO_PKG_VERSION"))?;
    m.add_class::<PyTokenizer>()?;
    m.add_class::<PyRules>()?;
    m.add_class::<SplitOn>()?;

    Ok(())
}
