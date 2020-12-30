use flate2::read::GzDecoder;
use nlprule_core::{
    rule::Rules,
    tokenizer::{finalize, tag::Tagger, Token},
};
use nlprule_core::{
    rule::Suggestion,
    tokenizer::{Tokenizer, TokenizerOptions},
};
use pyo3::prelude::*;
use pyo3::types::PyString;
use pyo3::{exceptions::PyValueError, types::PyBytes};
use std::{
    fs::{self, File},
    io::{BufReader, Cursor, Read},
    path::PathBuf,
    sync::Arc,
};

fn serialize_splitter(py: Python, obj: &Option<PyObject>) -> PyResult<Vec<u8>> {
    let bytes: &PyBytes = py
        .import("pickle")?
        .call_method1("dumps", (obj.to_object(py),))?
        .downcast()?;
    Ok(bytes.as_bytes().to_vec())
}

fn deserialize_splitter(py: Python, bytes: Vec<u8>) -> PyResult<Option<PyObject>> {
    let pybytes = PyBytes::new(py, &bytes);
    let obj: &PyAny = py.import("pickle")?.call_method1("loads", (pybytes,))?;
    Ok(obj.extract::<Option<_>>()?)
}

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

fn sentence_guard<F, O>(py: Python, sentence_or_sentences: PyObject, f: F) -> PyResult<PyObject>
where
    F: Fn(String) -> PyResult<O>,
    O: ToPyObject,
{
    let sentence_or_sentences = sentence_or_sentences.as_ref(py);
    let is_iterable = sentence_or_sentences.hasattr("__iter__")?
        && !sentence_or_sentences.is_instance::<PyString>()?;

    let sentences: Vec<String> = if is_iterable {
        sentence_or_sentences.extract()?
    } else {
        vec![sentence_or_sentences.extract()?]
    };

    let mut output = Vec::new();

    for sentence in sentences {
        output.push(f(sentence)?);
    }

    Ok(if is_iterable {
        output.to_object(py)
    } else {
        output[0].to_object(py)
    })
}

fn text_guard<F, O>(
    py: Python,
    text_or_texts: PyObject,
    sentence_splitter: &Option<PyObject>,
    sentence_equivalent_name: &str,
    f: F,
) -> PyResult<PyObject>
where
    F: Fn(Vec<String>) -> PyResult<O>,
    O: ToPyObject,
{
    let text_or_texts = text_or_texts.as_ref(py);
    let is_iterable =
        text_or_texts.hasattr("__iter__")? && !text_or_texts.is_instance::<PyString>()?;

    let texts: Vec<String> = if is_iterable {
        text_or_texts.extract()?
    } else {
        vec![text_or_texts.extract()?]
    };

    if let Some(sentence_splitter) = sentence_splitter {
        let mut output = Vec::new();

        for sentences in sentence_splitter
            .as_ref(py)
            .call1((texts,))?
            .extract::<Vec<Vec<String>>>()?
        {
            output.push(f(sentences)?);
        }

        Ok(if is_iterable {
            output.to_object(py)
        } else {
            output[0].to_object(py)
        })
    } else {
        Err(PyValueError::new_err(format!(
            "sentence_splitter must be set. Use {} to correct one sentence.",
            sentence_equivalent_name
        )))
    }
}

#[pyclass(module = "nlprule")]
#[text_signature = "(split_chars)"]
#[derive(Default)]
pub struct SplitOn {
    split_chars: Vec<char>,
}

#[pymethods]
impl SplitOn {
    #[new]
    fn new(split_chars: Option<Vec<&str>>) -> PyResult<Self> {
        Ok(SplitOn {
            split_chars: split_chars
                .unwrap_or_else(Vec::new)
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

    pub fn __setstate__(&mut self, py: Python, state: PyObject) -> PyResult<()> {
        match state.extract::<&PyBytes>(py) {
            Ok(s) => {
                let state: Vec<char> = bincode::deserialize(s.as_bytes()).unwrap();
                self.split_chars = state;
                Ok(())
            }
            Err(e) => Err(e),
        }
    }

    pub fn __getstate__(&self, py: Python) -> PyResult<PyObject> {
        let state = &self.split_chars;

        Ok(PyBytes::new(py, &bincode::serialize(&state).unwrap()).to_object(py))
    }
}

#[pyclass(name = "Tagger", module = "nlprule")]
#[derive(Default)]
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

#[pyclass(name = "Token", module = "nlprule")]
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
    fn data(&self) -> Vec<(&str, &str)> {
        self.token
            .word
            .tags
            .iter()
            .map(|x| (x.lemma.as_str(), x.pos.as_str()))
            .collect()
    }

    #[getter]
    fn lemmas(&self) -> Vec<&str> {
        let mut lemmas: Vec<_> = self
            .token
            .word
            .tags
            .iter()
            .filter_map(|x| {
                if x.lemma.is_empty() {
                    None
                } else {
                    Some(x.lemma.as_str())
                }
            })
            .collect();
        lemmas.sort_unstable();
        lemmas.dedup();
        lemmas
    }

    #[getter]
    fn tags(&self) -> Vec<&str> {
        let mut tags: Vec<_> = self
            .token
            .word
            .tags
            .iter()
            .filter_map(|x| {
                if x.pos.is_empty() {
                    None
                } else {
                    Some(x.pos.as_str())
                }
            })
            .collect();
        tags.sort_unstable();
        tags.dedup();
        tags
    }

    #[getter]
    fn chunks(&self) -> Vec<&str> {
        self.token.chunks.iter().map(|x| x.as_str()).collect()
    }
}

#[pyclass(name = "Suggestion", module = "nlprule")]
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

    #[getter]
    fn source(&self) -> &str {
        &self.suggestion.source
    }
}

impl From<Suggestion> for PySuggestion {
    fn from(suggestion: Suggestion) -> Self {
        PySuggestion { suggestion }
    }
}

#[pyclass(name = "Tokenizer", module = "nlprule")]
#[text_signature = "(path, sentence_splitter=None)"]
#[derive(Default)]
pub struct PyTokenizer {
    tokenizer: Tokenizer,
    sentence_splitter: Option<PyObject>,
}

impl PyTokenizer {
    fn tokenizer(&self) -> &Tokenizer {
        &self.tokenizer
    }
}

#[pymethods]
impl PyTokenizer {
    #[text_signature = "(code, sentence_splitter=None)"]
    #[staticmethod]
    fn load(code: &str, sentence_splitter: Option<PyObject>) -> PyResult<Self> {
        let bytes = get_resource(code, "tokenizer.bin.gz")?;

        let tokenizer: Tokenizer = bincode::deserialize_from(bytes)
            .map_err(|x| PyValueError::new_err(format!("{}", x)))?;
        Ok(PyTokenizer {
            tokenizer,
            sentence_splitter,
        })
    }

    #[new]
    fn new(path: Option<&str>, sentence_splitter: Option<PyObject>) -> PyResult<Self> {
        let tokenizer = if let Some(path) = path {
            let reader = BufReader::new(File::open(path).unwrap());
            bincode::deserialize_from(reader).unwrap()
        } else {
            Tokenizer::default()
        };

        Ok(PyTokenizer {
            tokenizer,
            sentence_splitter,
        })
    }

    #[getter]
    fn tagger(&self) -> PyTagger {
        PyTagger::new(
            self.tokenizer.tagger().clone(),
            (*self.tokenizer.options()).clone(),
        )
    }

    #[text_signature = "(text_or_texts)"]
    fn tokenize(&self, py: Python, text_or_texts: PyObject) -> PyResult<PyObject> {
        text_guard(
            py,
            text_or_texts,
            &self.sentence_splitter,
            ".apply_sentence",
            |sentences| {
                let mut output = Vec::new();

                for sentence in sentences {
                    let tokens = finalize(
                        self.tokenizer
                            .disambiguate(self.tokenizer.tokenize(&sentence)),
                    )
                    .into_iter()
                    .map(|x| PyCell::new(py, PyToken::from(x)))
                    .collect::<PyResult<Vec<_>>>()?;
                    output.extend(tokens);
                }

                Ok(output)
            },
        )
    }

    #[text_signature = "(sentence_or_sentences)"]
    fn tokenize_sentence(&self, py: Python, sentence_or_sentences: PyObject) -> PyResult<PyObject> {
        sentence_guard(py, sentence_or_sentences, |sentence| {
            finalize(
                self.tokenizer
                    .disambiguate(self.tokenizer.tokenize(&sentence)),
            )
            .into_iter()
            .map(|x| PyCell::new(py, PyToken::from(x)))
            .collect::<PyResult<Vec<_>>>()
        })
    }

    pub fn __setstate__(&mut self, py: Python, state: PyObject) -> PyResult<()> {
        match state.extract::<&PyBytes>(py) {
            Ok(s) => {
                let state: (Tokenizer, Vec<u8>) = bincode::deserialize(s.as_bytes()).unwrap();
                self.tokenizer = state.0;
                self.sentence_splitter = deserialize_splitter(py, state.1)?;
                Ok(())
            }
            Err(e) => Err(e),
        }
    }

    pub fn __getstate__(&self, py: Python) -> PyResult<PyObject> {
        let state = (
            &self.tokenizer,
            serialize_splitter(py, &self.sentence_splitter)?,
        );

        Ok(PyBytes::new(py, &bincode::serialize(&state).unwrap()).to_object(py))
    }
}

impl PyTokenizer {
    fn from_tokenizer(tokenizer: Tokenizer, sentence_splitter: Option<PyObject>) -> Self {
        PyTokenizer {
            tokenizer,
            sentence_splitter,
        }
    }
}

#[pyclass(name = "Rules", module = "nlprule")]
#[text_signature = "(path, tokenizer, sentence_splitter=None)"]
struct PyRules {
    rules: Rules,
    tokenizer: Py<PyTokenizer>,
    sentence_splitter: Option<PyObject>,
}

#[pymethods]
impl PyRules {
    #[text_signature = "(code, tokenizer, sentence_splitter=None)"]
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
        py: Python,
        path: Option<&str>,
        tokenizer: Option<Py<PyTokenizer>>,
        sentence_splitter: Option<PyObject>,
    ) -> PyResult<Self> {
        let rules = if let Some(path) = path {
            let reader = BufReader::new(File::open(path).unwrap());
            bincode::deserialize_from(reader).unwrap()
        } else {
            Rules::default()
        };
        let tokenizer = if let Some(tokenizer) = tokenizer {
            tokenizer
        } else {
            Py::new(py, PyTokenizer::default())?
        };

        Ok(PyRules {
            rules,
            tokenizer,
            sentence_splitter,
        })
    }

    #[text_signature = "(sentence_or_sentences)"]
    fn suggest_sentence(&self, py: Python, sentence_or_sentences: PyObject) -> PyResult<PyObject> {
        sentence_guard(py, sentence_or_sentences, |sentence| {
            let tokenizer = self.tokenizer.borrow(py);
            let tokenizer = tokenizer.tokenizer();

            let tokens = finalize(tokenizer.disambiguate(tokenizer.tokenize(&sentence)));
            self.rules
                .apply(&tokens)
                .into_iter()
                .map(|x| PyCell::new(py, PySuggestion::from(x)))
                .collect::<PyResult<Vec<_>>>()
        })
    }

    #[text_signature = "(text_or_texts)"]
    fn suggest(&self, py: Python, text_or_texts: PyObject) -> PyResult<PyObject> {
        text_guard(
            py,
            text_or_texts,
            &self.sentence_splitter,
            ".suggest_sentence",
            |sentences| {
                let tokenizer = self.tokenizer.borrow(py);
                let tokenizer = tokenizer.tokenizer();

                let mut output = Vec::new();
                let mut offset = 0;

                for sentence in sentences.iter() {
                    let tokens = finalize(tokenizer.disambiguate(tokenizer.tokenize(sentence)));
                    let suggestions = self
                        .rules
                        .apply(&tokens)
                        .into_iter()
                        .map(|mut x| {
                            x.start += offset;
                            x.end += offset;
                            PyCell::new(py, PySuggestion::from(x))
                        })
                        .collect::<PyResult<Vec<_>>>()?;
                    output.extend(suggestions);
                    offset += sentence.chars().count();
                }

                Ok(output)
            },
        )
    }

    #[text_signature = "(sentence_or_sentences)"]
    fn correct_sentence(&self, py: Python, sentence_or_sentences: PyObject) -> PyResult<PyObject> {
        sentence_guard(py, sentence_or_sentences, |sentence| {
            let tokenizer = self.tokenizer.borrow(py);
            let tokenizer = tokenizer.tokenizer();

            let tokens = finalize(tokenizer.disambiguate(tokenizer.tokenize(&sentence)));
            let suggestions = self.rules.apply(&tokens);
            Ok(Rules::correct(&sentence, &suggestions))
        })
    }

    #[text_signature = "(text, suggestions)"]
    fn apply_suggestions(
        &self,
        py: Python,
        text: &str,
        suggestions: Vec<Py<PySuggestion>>,
    ) -> String {
        let suggestions: Vec<Suggestion> = suggestions
            .into_iter()
            .map(|x| {
                let x = x.borrow(py);

                Suggestion {
                    source: x.source().to_string(),
                    text: x.text().iter().map(|x| x.to_string()).collect(),
                    start: x.start(),
                    end: x.end(),
                }
            })
            .collect();

        Rules::correct(text, &suggestions)
    }

    #[text_signature = "(text_or_texts)"]
    fn correct(&self, py: Python, text_or_texts: PyObject) -> PyResult<PyObject> {
        text_guard(
            py,
            text_or_texts,
            &self.sentence_splitter,
            ".correct_sentence",
            |sentences| {
                let tokenizer = self.tokenizer.borrow(py);
                let tokenizer = tokenizer.tokenizer();

                Ok(sentences
                    .iter()
                    .map(|x| {
                        let tokens = finalize(tokenizer.disambiguate(tokenizer.tokenize(x)));
                        let suggestions = self.rules.apply(&tokens);
                        Rules::correct(x, &suggestions)
                    })
                    .collect::<Vec<_>>()
                    .join(""))
            },
        )
    }

    pub fn __setstate__(&mut self, py: Python, state: PyObject) -> PyResult<()> {
        match state.extract::<&PyBytes>(py) {
            Ok(s) => {
                let state: (Rules, Tokenizer, Vec<u8>, Vec<u8>) =
                    bincode::deserialize(s.as_bytes()).unwrap();
                self.rules = state.0;
                self.sentence_splitter = deserialize_splitter(py, state.2)?;
                self.tokenizer = Py::new(
                    py,
                    PyTokenizer::from_tokenizer(state.1, deserialize_splitter(py, state.3)?),
                )?;
                Ok(())
            }
            Err(e) => Err(e),
        }
    }

    pub fn __getstate__(&self, py: Python) -> PyResult<PyObject> {
        let tokenizer = self.tokenizer.borrow(py);
        let state = (
            &self.rules,
            tokenizer.tokenizer(),
            serialize_splitter(py, &self.sentence_splitter)?,
            serialize_splitter(py, &self.tokenizer.borrow(py).sentence_splitter)?,
        );

        Ok(PyBytes::new(py, &bincode::serialize(&state).unwrap()).to_object(py))
    }
}

#[pymodule]
fn nlprule(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add("__version__", env!("CARGO_PKG_VERSION"))?;
    m.add_class::<PyTokenizer>()?;
    m.add_class::<PyRules>()?;
    m.add_class::<PySuggestion>()?;
    m.add_class::<PyToken>()?;
    m.add_class::<SplitOn>()?;

    Ok(())
}
