use flate2::read::GzDecoder;
use nlprule::{
    rule::{id::Selector, Example, Rule},
    rules::{apply_suggestions, Rules},
    tokenizer::tag::Tagger,
    tokenizer::Tokenizer,
    types::*,
};
use parking_lot::{
    MappedRwLockReadGuard, MappedRwLockWriteGuard, RwLock, RwLockReadGuard, RwLockWriteGuard,
};
use pyo3::prelude::*;
use pyo3::types::PyString;
use pyo3::{exceptions::PyValueError, types::PyBytes};
use std::{
    convert::TryFrom,
    fs,
    io::{Cursor, Read},
    path::PathBuf,
    sync::Arc,
};

fn get_resource(lang_code: &str, name: &str) -> PyResult<impl Read> {
    let version = env!("CARGO_PKG_VERSION");
    let mut cache_path: Option<PathBuf> = None;

    // try to find a file at which to cache the data
    if let Some(project_dirs) = directories::ProjectDirs::from("", "", "nlprule") {
        let cache_dir = project_dirs.cache_dir();

        cache_path = Some(
            cache_dir.join(version).join(lang_code).join(
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
        lang_code,
        name
    ))
    .and_then(|x| x.error_for_status())
    .and_then(|x| x.bytes())
    .map_err(|x| PyValueError::new_err(format!("{}", x)))?;

    let mut gz = GzDecoder::new(&bytes[..]);
    let mut buffer = Vec::new();
    gz.read_to_end(&mut buffer)?;

    // ... and then cache the data at the provided file, if one was found
    if let Some(path) = &cache_path {
        fs::create_dir_all(path.parent().expect("filepath must have parent"))?;
        fs::write(path, &buffer)?;
    }

    Ok(Cursor::new(buffer))
}

fn text_guard<F, O>(py: Python, text_or_texts: PyObject, f: F) -> PyResult<PyObject>
where
    F: Fn(String) -> PyResult<O>,
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

    let mut output = Vec::new();

    for text in texts {
        output.push(f(text)?);
    }

    Ok(if is_iterable {
        output.to_object(py)
    } else {
        output[0].to_object(py)
    })
}

/// A tagger dictionary.
/// Associates many words with possible POS tags and lemmas.
///
/// Can not be created directly but accessed by the `.tagger` attribute on the tokenizer.
#[pyclass(name = "Tagger", module = "nlprule")]
#[derive(Default)]
pub struct PyTagger {
    tagger: Arc<Tagger>,
}

impl From<Arc<Tagger>> for PyTagger {
    fn from(tagger: Arc<Tagger>) -> Self {
        PyTagger { tagger }
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
    ///         If unset, will be set according to the language options.
    ///     use_compound_split_heuristic (Optional[bool]):
    ///         Whether to use a heuristic to split compound words.
    ///         If unset, will be set according to the language options.
    /// Returns:
    ///     data (List[Tuple[str, str]]):
    ///         A list of tuples of (lemma, POS).
    ///         Not contextualized so it can be thought of as possible lemma / POS of the given word.
    #[text_signature = "(word, add_lower=None, use_compound_split_heuristic=None)"]
    fn get_data(
        &self,
        word: &str,
        add_lower: Option<bool>,
        use_compound_split_heuristic: Option<bool>,
    ) -> Vec<(String, String)> {
        self.tagger
            .get_tags_with_options(word, add_lower, use_compound_split_heuristic)
            .into_iter()
            .map(|x| (x.lemma().as_str().to_string(), x.pos().as_str().to_string()))
            .collect()
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
    token: IncompleteToken<'static>,
}

impl From<IncompleteToken<'static>> for PyToken {
    fn from(token: IncompleteToken<'static>) -> Self {
        PyToken { token }
    }
}

#[pymethods]
impl PyToken {
    #[getter]
    fn text(&self) -> &str {
        self.token.as_str()
    }

    #[getter]
    fn span(&self) -> (usize, usize) {
        (self.token.span().char().start, self.token.span().char().end)
    }

    #[getter]
    fn data(&self) -> Vec<(&str, &str)> {
        self.token
            .tags()
            .iter()
            .map(|x| (x.lemma().as_str(), x.pos().as_str()))
            .collect()
    }

    #[getter]
    fn lemmas(&self) -> Vec<&str> {
        let mut lemmas: Vec<_> = self
            .token
            .tags()
            .iter()
            .filter_map(|x| {
                if x.lemma().as_str().is_empty() {
                    None
                } else {
                    Some(x.lemma().as_str())
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
            .tags()
            .iter()
            .filter_map(|x| {
                if x.pos().as_str().is_empty() {
                    None
                } else {
                    Some(x.pos().as_str())
                }
            })
            .collect();
        tags.sort_unstable();
        tags.dedup();
        tags
    }

    #[getter]
    fn chunks(&self) -> Vec<&str> {
        self.token.chunks().iter().map(|x| x.as_str()).collect()
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
        self.suggestion.span().char().start
    }

    #[getter]
    fn end(&self) -> usize {
        self.suggestion.span().char().end
    }

    #[getter]
    fn replacements(&self) -> Vec<&str> {
        self.suggestion
            .replacements()
            .iter()
            .map(|x| x.as_str())
            .collect()
    }

    #[getter]
    fn source(&self) -> &str {
        self.suggestion.source()
    }

    #[getter]
    fn message(&self) -> &str {
        self.suggestion.message()
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
    fn load(lang_code: &str) -> PyResult<Self> {
        let bytes = get_resource(lang_code, "tokenizer.bin.gz")?;

        let tokenizer: Tokenizer = bincode::deserialize_from(bytes)
            .map_err(|x| PyValueError::new_err(format!("{}", x)))?;
        Ok(PyTokenizer { tokenizer })
    }

    #[new]
    fn new(path: Option<&str>) -> PyResult<Self> {
        let tokenizer = if let Some(path) = path {
            Tokenizer::new(path)
                .map_err(|x| PyValueError::new_err(format!("error creating Tokenizer: {}", x)))?
        } else {
            Tokenizer::default()
        };

        Ok(PyTokenizer { tokenizer })
    }

    /// Get the tagger dictionary of this tokenizer.
    ///
    /// Returns:
    ///     tagger (Tagger): The tagger dictionary.
    #[getter]
    fn tagger(&self) -> PyTagger {
        self.tokenizer.tagger().clone().into()
    }

    /// Applies the full tokenization pipeline to the given text.
    /// This includes POS tagging, lemmatization, chunking and sentencization.
    ///
    /// Arguments:
    ///     text_or_texts (Union[str, List[str]]): The text(s) to tokenize.
    ///
    /// Returns:
    ///     tokens (Union[List[List[Token]], List[List[List[Token]]]]):
    ///         The analyzed tokens. A list of lists of tokens. The outer list corresponds to a sentence. Batched if the input is batched.
    #[text_signature = "(text_or_texts)"]
    fn pipe(&self, py: Python, text_or_texts: PyObject) -> PyResult<PyObject> {
        text_guard(py, text_or_texts, |text| {
            let tokens = self
                .tokenizer
                .pipe(&text)
                .map(|sentence| {
                    sentence
                        .into_iter()
                        .map(|token| PyCell::new(py, PyToken::from(token.into_static())))
                        .collect::<PyResult<Vec<_>>>()
                })
                .collect::<PyResult<Vec<Vec<_>>>>()?;

            Ok(tokens)
        })
    }

    pub fn __setstate__(&mut self, py: Python, state: PyObject) -> PyResult<()> {
        match state.extract::<&PyBytes>(py) {
            Ok(s) => {
                self.tokenizer = bincode::deserialize(s.as_bytes()).map_err(|_| {
                    PyValueError::new_err("deserializing state with `bincode` failed")
                })?;
                Ok(())
            }
            Err(e) => Err(e),
        }
    }

    pub fn __getstate__(&self, py: Python) -> PyResult<PyObject> {
        Ok(PyBytes::new(
            py,
            &bincode::serialize(&self.tokenizer)
                .map_err(|_| PyValueError::new_err("serializing state with `bincode` failed"))?,
        )
        .to_object(py))
    }
}

impl From<Tokenizer> for PyTokenizer {
    fn from(tokenizer: Tokenizer) -> Self {
        PyTokenizer { tokenizer }
    }
}

/// An example associated with a grammatical rule.
///
/// Attributes:
/// * text (str): the text of this example
/// * suggestion (Option[Suggestion]): The suggestion for this example.
///     If this is None, it is an example of where the rule should not trigger.
#[pyclass(name = "Example", module = "nlprule")]
struct PyExample {
    text: String,
    suggestion: Option<Py<PySuggestion>>,
}

impl PyExample {
    fn from_example(py: Python, example: &Example) -> PyResult<Self> {
        Ok(PyExample {
            text: example.text().to_owned(),
            suggestion: if let Some(suggestion) = example.suggestion() {
                let suggestion = PySuggestion::from((*suggestion).clone());
                Some(Py::new(py, suggestion)?)
            } else {
                None
            },
        })
    }
}

#[pymethods]
impl PyExample {
    #[getter]
    fn text(&self) -> &str {
        &self.text
    }

    #[getter]
    fn suggestion<'py>(&'py self, py: Python<'py>) -> Option<PyRef<'py, PySuggestion>> {
        self.suggestion.as_ref().map(|x| x.borrow(py))
    }
}

/// One grammatical rule.
///
/// Can not be created directly but accessed by the `.rules` attribute on the rules.
/// Attributes:
/// * id (str): The id of this rule.
/// * url (Option[str]): A URL for more information.
/// * short (Option[str]): A short description of this rule e. g. "Possible typo".
/// * examples (List[Example]): Examples associated with this rule. Always at least one.
/// * name (str): A human-readable name for this rule.
/// * category_name (str): A human-readable name of the category this rule is in.
/// * category_type (Option[str]): The type of the category this rule is in e. g. "style" or "grammar".
/// * enabled (bool): Whether the rule is enabled.
#[pyclass(name = "Rule", module = "nlprule")]
struct PyRule {
    rules: Arc<RwLock<Rules>>,
    index: usize,
}

impl PyRule {
    fn rule(&self) -> MappedRwLockReadGuard<'_, Rule> {
        RwLockReadGuard::map(self.rules.read(), |x| &x.rules()[self.index])
    }

    fn rule_mut(&self) -> MappedRwLockWriteGuard<'_, Rule> {
        RwLockWriteGuard::map(self.rules.write(), |x| &mut x.rules_mut()[self.index])
    }

    fn from_rule(index: usize, rules: Arc<RwLock<Rules>>) -> Self {
        PyRule { rules, index }
    }
}

#[pymethods]
impl PyRule {
    #[getter]
    fn id(&self) -> String {
        self.rule().id().to_string()
    }

    #[getter]
    fn url(&self) -> Option<String> {
        self.rule().url().map(ToOwned::to_owned)
    }

    #[getter]
    fn short(&self) -> Option<String> {
        self.rule().short().map(ToOwned::to_owned)
    }

    #[getter]
    fn examples(&self, py: Python) -> PyResult<Vec<PyExample>> {
        self.rule()
            .examples()
            .iter()
            .map(|x| PyExample::from_example(py, x))
            .collect()
    }

    #[getter]
    fn name(&self) -> String {
        self.rule().name().to_owned()
    }

    #[getter]
    fn category_name(&self) -> String {
        self.rule().category_name().to_owned()
    }

    #[getter]
    fn category_type(&self) -> Option<String> {
        self.rule().category_type().map(ToOwned::to_owned)
    }

    #[getter]
    fn enabled(&self) -> bool {
        self.rule().enabled()
    }

    /// Hints that this rule should be enabled.
    fn enable(&self) {
        self.rule_mut().enable();
    }

    /// Hints that this rule should be disabled.
    fn disable(&self) {
        self.rule_mut().disable();
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
    rules: Arc<RwLock<Rules>>,
    tokenizer: Py<PyTokenizer>,
}

#[pymethods]
impl PyRules {
    #[text_signature = "(code, tokenizer, sentence_splitter=None)"]
    #[staticmethod]
    fn load(lang_code: &str, tokenizer: Py<PyTokenizer>) -> PyResult<Self> {
        let bytes = get_resource(lang_code, "rules.bin.gz")?;

        let rules: Rules = bincode::deserialize_from(bytes)
            .map_err(|x| PyValueError::new_err(format!("{}", x)))?;
        Ok(PyRules {
            rules: Arc::from(RwLock::from(rules)),
            tokenizer,
        })
    }

    #[new]
    fn new(py: Python, path: Option<&str>, tokenizer: Option<Py<PyTokenizer>>) -> PyResult<Self> {
        let rules = if let Some(path) = path {
            Rules::new(path)
                .map_err(|x| PyValueError::new_err(format!("error creating Rules: {}", x)))?
        } else {
            Rules::default()
        };
        let tokenizer = if let Some(tokenizer) = tokenizer {
            tokenizer
        } else {
            Py::new(py, PyTokenizer::default())?
        };

        Ok(PyRules {
            rules: Arc::from(RwLock::from(rules)),
            tokenizer,
        })
    }

    #[getter]
    fn rules(&self) -> Vec<PyRule> {
        self.rules
            .read()
            .rules()
            .iter()
            .enumerate()
            .map(|(i, _)| PyRule::from_rule(i, self.rules.clone()))
            .collect()
    }

    /// Finds a rule by selector.
    fn select(&self, id: &str) -> PyResult<Vec<PyRule>> {
        let selector = Selector::try_from(id.to_owned())
            .map_err(|err| PyValueError::new_err(format!("error creating selector: {}", err)))?;

        Ok(self
            .rules
            .read()
            .rules()
            .iter()
            .enumerate()
            .filter(|(_, rule)| selector.is_match(rule.id()))
            .map(|(i, _)| PyRule::from_rule(i, self.rules.clone()))
            .collect())
    }

    /// Get suggestions for the given text.
    ///
    /// Arguments:
    ///     text_or_texts (Union[str, List[str]]): The text(s) to get suggestions for.
    ///
    /// Returns:
    ///     suggestions (Union[List[Suggestion], List[List[Suggestion]]]):
    ///         The computed suggestions. Batched if the input is batched.
    #[text_signature = "(sentence_or_sentences)"]
    fn suggest(&self, py: Python, sentence_or_sentences: PyObject) -> PyResult<PyObject> {
        text_guard(py, sentence_or_sentences, |sentence| {
            let tokenizer = self.tokenizer.borrow(py);
            let tokenizer = tokenizer.tokenizer();

            self.rules
                .read()
                .suggest(&sentence, &tokenizer)
                .into_iter()
                .map(|x| PyCell::new(py, PySuggestion::from(x)))
                .collect::<PyResult<Vec<_>>>()
        })
    }

    /// Correct the given text(s).
    ///
    /// Arguments:
    ///     text_or_texts (Union[str, List[str]]): The text(s) to correct.
    ///
    /// Returns:
    ///     text_or_texts (Union[str, List[str]]):
    ///         The corrected texts. Batched if the input is batched.
    #[text_signature = "(text_or_texts)"]
    fn correct(&self, py: Python, text_or_texts: PyObject) -> PyResult<PyObject> {
        text_guard(py, text_or_texts, |text| {
            let tokenizer = self.tokenizer.borrow(py);
            let tokenizer = tokenizer.tokenizer();

            Ok(self.rules.read().correct(&text, tokenizer))
        })
    }

    /// Convenience method to apply suggestions to the given text.
    /// Always uses the first element of `suggestion.replacements` as replacement.
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

                x.suggestion.clone()
            })
            .collect();

        apply_suggestions(text, &suggestions)
    }

    pub fn __setstate__(&mut self, py: Python, state: PyObject) -> PyResult<()> {
        match state.extract::<&PyBytes>(py) {
            Ok(s) => {
                let state: (Rules, Tokenizer) =
                    bincode::deserialize(s.as_bytes()).map_err(|_| {
                        PyValueError::new_err("deserializing state with `bincode` failed")
                    })?;
                // a roundtrip through pickle can not preserve references so we need to create a new Arc<RwLock<..>>
                self.rules = Arc::from(RwLock::from(state.0));
                self.tokenizer = Py::new(py, PyTokenizer::from(state.1))?;
                Ok(())
            }
            Err(e) => Err(e),
        }
    }

    pub fn __getstate__(&self, py: Python) -> PyResult<PyObject> {
        let tokenizer = self.tokenizer.borrow(py);
        // rwlock is serialized the same way as the inner type
        let state = (&self.rules, tokenizer.tokenizer());

        Ok(PyBytes::new(
            py,
            &bincode::serialize(&state)
                .map_err(|_| PyValueError::new_err("serializing state with `bincode` failed"))?,
        )
        .to_object(py))
    }
}

#[pymodule]
fn nlprule(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add("__version__", env!("CARGO_PKG_VERSION"))?;
    m.add_class::<PyTokenizer>()?;
    m.add_class::<PyRules>()?;
    m.add_class::<PySuggestion>()?;
    m.add_class::<PyToken>()?;

    Ok(())
}
