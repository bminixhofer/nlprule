use flate2::read::GzDecoder;
use nlprule::types::*;
use nlprule::{
    rule::Rule,
    rules::{correct, Rules},
    tokenizer::{finalize, tag::Tagger},
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
        "https://github.com/bminixhofer/nlprule/releases/download/{}/{}_{}",
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

/// A convience class to split text on specific characters.
/// Can be passed as `sentence_splitter` to the `Rules` or `Tokenizer` to enable processing more than one sentence.
///
/// # Example
///
/// ```python
/// split = SplitOn([".", "?", "!"])
/// split(["This is a test. This is also a test."])
/// # returns [["This is a test. ", "This is also a test."]]
/// ```
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

/// A tagger dictionary.
/// Associates many words with possible POS tags and lemmas.
///
/// Can not be created directly but accessed by the `.tagger` attribute on the tokenizer.
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
    /// Get the data for a given word from the dictionary.
    ///
    /// Arguments:
    ///     word (str): The input word.
    ///     add_lower (Optional[bool]):
    ///         Whether to add data for the lowercase variant of the word.
    ///         If unset, will be set according to the language config.
    ///
    /// Returns:
    ///     data (List[Tuple[str, str]]):
    ///         A list of tuples of (lemma, POS).
    ///         Not contextualized so it can be thought of as possible lemma / POS of the given word.
    #[text_signature = "(word, add_lower=None)"]
    fn get_data(&self, word: &str, add_lower: Option<bool>) -> Vec<(String, String)> {
        self.tagger
            .get_tags(
                word,
                add_lower.unwrap_or(self.options.always_add_lower_tags),
                self.options.use_compound_split_heuristic,
            )
            .into_iter()
            .map(|x| (x.lemma.as_ref().to_string(), x.pos.as_ref().to_string()))
            .collect()
    }

    /// Get the words with the same lemma as the given lemma.
    ///
    /// Arguments:
    ///     lemma (str): The lemma.
    ///
    /// Returns:
    ///     group_members (List[str]): The words in the dictionary with the same lemma.
    fn get_group_members(&self, lemma: &str) -> Vec<&str> {
        self.tagger.get_group_members(&lemma.to_string())
    }
}

impl PyTagger {
    pub fn tagger(&self) -> &Arc<Tagger> {
        &self.tagger
    }
}

/// An analyzed token with the attributes:
/// * text (str): the text of this token
/// * span (Tuple[int, int]): the character span of this token in the original string
/// * data (List[Tuple[str, str]]): Lemmas and corresponding POS tags of this token
/// * lemmas (List[str]): A list of lemmas of this token
/// * tags (List[str]): A list of possible POS tags for this token. Including special SENT_START and SENT_END tags.
/// * chunks (List[str]): Chunks of this token. Are not set for some languages (e. g. German).
#[pyclass(name = "Token", module = "nlprule")]
pub struct PyToken {
    token: owned::Token,
}

impl From<owned::Token> for PyToken {
    fn from(token: owned::Token) -> Self {
        PyToken { token }
    }
}

#[pymethods]
impl PyToken {
    #[getter]
    fn text(&self) -> &str {
        self.token.word.text.as_ref()
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
            .map(|x| (x.lemma.as_ref(), x.pos.as_ref()))
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
                if x.lemma.as_ref().is_empty() {
                    None
                } else {
                    Some(x.lemma.as_ref())
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
                if x.pos.as_ref().is_empty() {
                    None
                } else {
                    Some(x.pos.as_ref())
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

/// A replacement suggestion with the attributes:
/// * start (int): The start character position of the suggestion in the original text.
/// * end (int): The end character position of the suggestion in the original text.
/// * text (List[str]): A list of suggested replacements.
/// * source (str): The ID of the rule that triggered this suggestion.
/// * message (str): A human-readable message for this suggestion.
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

    #[getter]
    fn message(&self) -> &str {
        &self.suggestion.message
    }
}

impl From<Suggestion> for PySuggestion {
    fn from(suggestion: Suggestion) -> Self {
        PySuggestion { suggestion }
    }
}

/// The tokenizer.
/// Does dictionary- and rule-based POS tagging, lemmatization and (depending on the language) chunking.
/// Can be created from a tokenizer binary:
/// ```python
/// tokenizer = Tokenizer("/path/to/tokenizer.bin")
/// ```
/// or from a language code:
/// ```python
/// tokenizer = Tokenizer.load("en")
/// ```
/// When created from a language code, the binary is downloaded from the internet the first time.
/// Then it is stored at your cache and loaded from there.
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

    /// Get the tagger dictionary of this tokenizer.
    ///
    /// Returns:
    ///     tagger (Tagger): The tagger dictionary.
    #[getter]
    fn tagger(&self) -> PyTagger {
        PyTagger::new(
            self.tokenizer.tagger().clone(),
            (*self.tokenizer.options()).clone(),
        )
    }

    /// Tokenizes the given sentence(s).
    /// "tokenizing" in this context includes POS tagging, lemmatization and chunking.
    ///
    /// Arguments:
    ///     sentence_or_sentences (Union[str, List[str]]): The sentence(s) to tokenize.
    ///
    /// Returns:
    ///     tokens (Union[List[Token], List[List[Token]]]):
    ///         The analyzed tokens. Batched if the input is batched.
    ///         NB: a special SENT_START token is always inserted as the first token, otherwise tokens mostly correspond to words.
    #[text_signature = "(sentence_or_sentences)"]
    fn tokenize_sentence(&self, py: Python, sentence_or_sentences: PyObject) -> PyResult<PyObject> {
        sentence_guard(py, sentence_or_sentences, |sentence| {
            finalize(
                self.tokenizer
                    .disambiguate(self.tokenizer.tokenize(&sentence)),
            )
            .into_iter()
            .map(|x| PyCell::new(py, PyToken::from(x.to_owned_token())))
            .collect::<PyResult<Vec<_>>>()
        })
    }

    /// Tokenize an arbitrary text. See the documentation for `tokenize_sentence`.
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
                    .map(|x| PyCell::new(py, PyToken::from(x.to_owned_token())))
                    .collect::<PyResult<Vec<_>>>()?;
                    output.extend(tokens);
                }

                Ok(output)
            },
        )
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

/// One grammatical rule.
///
/// Can not be created directly but accessed by the `.rules` attribute on the rules.
/// Attributes:
/// * id (str): The id of this rule.
#[pyclass(name = "Rule", module = "nlprule")]
struct PyRule {
    id: String,
}

impl PyRule {
    fn from_rule(rule: &Rule) -> Self {
        PyRule {
            id: rule.id().to_string(),
        }
    }
}

#[pymethods]
impl PyRule {
    #[getter]
    fn id(&self) -> &str {
        &self.id
    }
}

/// The grammatical rules.
/// Can be created from a rules binary:
/// ```python
/// tokenizer = Tokenizer("/path/to/rules.bin")
/// ```
/// or from a language code and `Tokenizer`:
/// ```python
/// tokenizer = Tokenizer.load("en")
/// rules = Rules.load("en", tokenizer)
/// ```
/// When created from a language code, the binary is downloaded from the internet the first time.
/// Then it is stored at your cache and loaded from there.
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

    #[getter]
    fn rules(&self) -> Vec<PyRule> {
        self.rules.rules().iter().map(PyRule::from_rule).collect()
    }

    /// Get suggestions for the given sentence.
    ///
    /// Arguments:
    ///     sentence_or_sentences (Union[str, List[str]]): The sentence(s) to get suggestions for.
    ///
    /// Returns:
    ///     suggestions (Union[List[Suggestion], List[List[Suggestion]]]):
    ///         The computed suggestions. Batched if the input is batched.
    #[text_signature = "(sentence_or_sentences)"]
    fn suggest_sentence(&self, py: Python, sentence_or_sentences: PyObject) -> PyResult<PyObject> {
        sentence_guard(py, sentence_or_sentences, |sentence| {
            let tokenizer = self.tokenizer.borrow(py);
            let tokenizer = tokenizer.tokenizer();

            let tokens = finalize(tokenizer.disambiguate(tokenizer.tokenize(&sentence)));
            self.rules
                .suggest(&tokens, &tokenizer)
                .into_iter()
                .map(|x| PyCell::new(py, PySuggestion::from(x)))
                .collect::<PyResult<Vec<_>>>()
        })
    }

    /// Get suggestions for an arbitrary text. See the documentation for `suggest_sentence`.
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
                        .suggest(&tokens, &tokenizer)
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

    /// Correct the given sentence.
    ///
    /// Arguments:
    ///     sentence_or_sentences (Union[str, List[str]]): The sentence(s) to correct.
    ///
    /// Returns:
    ///     sentence_or_sentences (Union[str, List[str]]):
    ///         The corrected texts. Batched if the input is batched.
    #[text_signature = "(sentence_or_sentences)"]
    fn correct_sentence(&self, py: Python, sentence_or_sentences: PyObject) -> PyResult<PyObject> {
        sentence_guard(py, sentence_or_sentences, |sentence| {
            let tokenizer = self.tokenizer.borrow(py);
            let tokenizer = tokenizer.tokenizer();

            let tokens = finalize(tokenizer.disambiguate(tokenizer.tokenize(&sentence)));
            let suggestions = self.rules.suggest(&tokens, &tokenizer);
            Ok(correct(&sentence, &suggestions))
        })
    }

    /// Correct an arbitrary text. See the documentation for `correct_sentence`.
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
                        let suggestions = self.rules.suggest(&tokens, &tokenizer);
                        correct(x, &suggestions)
                    })
                    .collect::<Vec<_>>()
                    .join(""))
            },
        )
    }

    /// Convenience method to apply suggestions to the given text.
    /// Always uses the first element of `suggestion.text` as replacement.
    ///
    /// Arguments:
    ///     text (str): The input text.
    ///     suggestions (List[Suggestion]): A list of suggestions to apply.
    ///
    /// Returns:
    ///     text (str): The text with the suggestions applied to it.
    #[text_signature = "(text, suggestions)"]
    #[staticmethod]
    fn apply_suggestions(py: Python, text: &str, suggestions: Vec<Py<PySuggestion>>) -> String {
        let suggestions: Vec<Suggestion> = suggestions
            .into_iter()
            .map(|x| {
                let x = x.borrow(py);

                Suggestion {
                    source: x.source().to_string(),
                    message: x.message().to_string(),
                    text: x.text().iter().map(|x| x.to_string()).collect(),
                    start: x.start(),
                    end: x.end(),
                }
            })
            .collect();

        correct(text, &suggestions)
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
