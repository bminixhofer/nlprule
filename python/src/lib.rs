use flate2::read::GzDecoder;
use nlprule::{
    rule::{id::Selector, Example, Rule},
    rules::{apply_suggestions, Rules},
    spellcheck::{Candidate, Spell},
    tokenizer::tag::Tagger,
    tokenizer::Tokenizer,
    types::*,
};
use parking_lot::{
    MappedRwLockReadGuard, MappedRwLockWriteGuard, RwLock, RwLockReadGuard, RwLockWriteGuard,
};
use pyo3::{exceptions::PyValueError, types::PyBytes};
use pyo3::{
    prelude::*,
    types::{PyDict, PyFrozenSet},
    wrap_pymodule,
};
use pyo3::{types::PyString, ToPyObject};
use pythonize::depythonize;
use std::{
    collections::HashSet,
    convert::TryFrom,
    fs,
    io::{Cursor, Read},
    path::PathBuf,
    sync::Arc,
};

fn err(error: nlprule::Error) -> PyErr {
    PyValueError::new_err(format!("{}", error))
}

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
    fn replacements(&self) -> Vec<&str> {
        self.suggestion
            .replacements
            .iter()
            .map(|x| x.as_str())
            .collect()
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
#[text_signature = "(path)"]
#[derive(Default)]
pub struct PyTokenizer {
    tokenizer: Arc<Tokenizer>,
}

impl PyTokenizer {
    fn tokenizer(&self) -> &Arc<Tokenizer> {
        &self.tokenizer
    }
}

#[pymethods]
impl PyTokenizer {
    #[text_signature = "(code)"]
    #[staticmethod]
    fn load(lang_code: &str) -> PyResult<Self> {
        let bytes = get_resource(lang_code, "tokenizer.bin.gz")?;

        let tokenizer = Tokenizer::from_reader(bytes).map_err(err)?;
        Ok(PyTokenizer {
            tokenizer: Arc::new(tokenizer),
        })
    }

    #[new]
    fn new(path: Option<&str>) -> PyResult<Self> {
        let tokenizer = if let Some(path) = path {
            Tokenizer::new(path).map_err(err)?
        } else {
            Tokenizer::default()
        };

        Ok(PyTokenizer {
            tokenizer: Arc::new(tokenizer),
        })
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
    ///         NB: a special SENT_START token is always inserted as the first token in each sentence, otherwise tokens mostly correspond to words.
    #[text_signature = "(text_or_texts)"]
    fn pipe(&self, py: Python, text_or_texts: PyObject) -> PyResult<PyObject> {
        text_guard(py, text_or_texts, |text| {
            let tokens = self
                .tokenizer
                .pipe(&text)
                .into_iter()
                .map(|tokens| {
                    tokens
                        .into_iter()
                        .map(|x| PyCell::new(py, PyToken::from(x.to_owned_token())))
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

impl From<Arc<Tokenizer>> for PyTokenizer {
    fn from(tokenizer: Arc<Tokenizer>) -> Self {
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

    fn from_rules(index: usize, rules: Arc<RwLock<Rules>>) -> Self {
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

#[pyclass(name = "SpellOptions", module = "nlprule.spell")]
struct PySpellOptions {
    rules: Arc<RwLock<Rules>>,
}

impl PySpellOptions {
    fn spell(&self) -> MappedRwLockReadGuard<'_, Spell> {
        RwLockReadGuard::map(self.rules.read(), |x| x.spell())
    }

    fn spell_mut(&self) -> MappedRwLockWriteGuard<'_, Spell> {
        RwLockWriteGuard::map(self.rules.write(), |x| x.spell_mut())
    }
}

#[pymethods]
impl PySpellOptions {
    #[getter]
    fn get_variant(&self) -> Option<String> {
        self.spell()
            .options()
            .variant
            .as_ref()
            .map(|x| x.as_str().to_owned())
    }

    #[setter]
    fn set_variant(&self, variant: Option<&str>) -> PyResult<()> {
        if let Some(variant) = variant {
            let mut spell = self.spell_mut();
            let variant = spell.variant(variant).map_err(err)?;

            spell.options_mut().variant = Some(variant);
        } else {
            self.spell_mut().options_mut().variant = None;
        }

        Ok(())
    }

    #[getter]
    fn get_max_distance(&self) -> usize {
        self.spell().options().max_distance
    }

    #[setter]
    fn set_max_distance(&self, max_distance: usize) {
        self.spell_mut().options_mut().max_distance = max_distance
    }

    #[getter]
    fn get_prefix_length(&self) -> usize {
        self.spell().options().prefix_length
    }

    #[setter]
    fn set_prefix_length(&self, prefix_length: usize) {
        self.spell_mut().options_mut().prefix_length = prefix_length
    }

    #[getter]
    fn get_freq_weight(&self) -> f32 {
        self.spell().options().freq_weight
    }

    #[setter]
    fn set_freq_weight(&self, freq_weight: f32) {
        self.spell_mut().options_mut().freq_weight = freq_weight
    }

    #[getter]
    fn get_top_n(&self) -> usize {
        self.spell().options().top_n
    }

    #[setter]
    fn set_top_n(&self, top_n: usize) {
        self.spell_mut().options_mut().top_n = top_n
    }

    #[getter]
    fn get_whitelist<'py>(&self, py: Python<'py>) -> PyResult<&'py PyFrozenSet> {
        let spell = self.spell();
        let whitelist: Vec<&str> = spell
            .options()
            .whitelist
            .iter()
            .map(|x| x.as_str())
            .collect();

        PyFrozenSet::new(py, &whitelist)
    }

    #[setter]
    fn set_whitelist(&self, py: Python, whitelist: PyObject) -> PyResult<()> {
        let whitelist: PyResult<HashSet<String>> = whitelist
            .as_ref(py)
            .iter()?
            .map(|x| x.and_then(PyAny::extract::<String>))
            .collect();
        self.spell_mut().options_mut().whitelist = whitelist?;
        Ok(())
    }
}

#[pyclass(name = "Candidate", module = "nlprule.spell")]
struct PyCandidate {
    candidate: Candidate,
}

#[pymethods]
impl PyCandidate {
    #[getter]
    fn score(&self) -> f32 {
        self.candidate.score()
    }

    #[getter]
    fn distance(&self) -> usize {
        self.candidate.distance()
    }

    #[getter]
    fn freq(&self) -> usize {
        self.candidate.freq()
    }

    #[getter]
    fn term(&self) -> &str {
        self.candidate.term()
    }
}

#[pyclass(name = "Spell", module = "nlprule.spell")]
struct PySpell {
    rules: Arc<RwLock<Rules>>,
}

#[pymethods]
impl PySpell {
    #[getter]
    fn variants(&self) -> Vec<String> {
        self.rules
            .read()
            .spell()
            .variants()
            .iter()
            .map(|x| x.as_str().to_owned())
            .collect()
    }

    #[getter]
    fn get_options(&self) -> PySpellOptions {
        PySpellOptions {
            rules: self.rules.clone(),
        }
    }

    #[setter]
    fn set_options(&self, py: Python, options: &PyDict) -> PyResult<()> {
        let mut guard = self.rules.write();
        *guard.spell_mut().options_mut() = depythonize(options.to_object(py).as_ref(py))?;
        Ok(())
    }

    fn check(&self, word: &str) -> bool {
        self.rules.read().spell().check(word)
    }

    fn search(&self, word: &str) -> Vec<PyCandidate> {
        self.rules
            .read()
            .spell()
            .search(word)
            .into_iter()
            .map(|candidate| PyCandidate { candidate })
            .collect()
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
#[text_signature = "(path, tokenizer)"]
struct PyRules {
    rules: Arc<RwLock<Rules>>,
}

#[pymethods]
impl PyRules {
    #[text_signature = "(code, tokenizer)"]
    #[staticmethod]
    fn load(lang_code: &str, tokenizer: &PyTokenizer) -> PyResult<Self> {
        let bytes = get_resource(lang_code, "rules.bin.gz")?;

        let rules = Rules::from_reader(bytes, tokenizer.tokenizer().clone()).map_err(err)?;
        Ok(PyRules {
            rules: Arc::from(RwLock::from(rules)),
        })
    }

    #[new]
    fn new(path: Option<&str>, tokenizer: Option<&PyTokenizer>) -> PyResult<Self> {
        let tokenizer = if let Some(tokenizer) = tokenizer {
            tokenizer.tokenizer().clone()
        } else {
            PyTokenizer::default().tokenizer().clone()
        };

        let rules = if let Some(path) = path {
            Rules::new(path, tokenizer).map_err(err)?
        } else {
            Rules::default()
        };

        Ok(PyRules {
            rules: Arc::from(RwLock::from(rules)),
        })
    }

    #[getter]
    fn spell(&self) -> PySpell {
        PySpell {
            rules: self.rules.clone(),
        }
    }

    #[getter]
    fn rules(&self) -> Vec<PyRule> {
        self.rules
            .read()
            .rules()
            .iter()
            .enumerate()
            .map(|(i, _)| PyRule::from_rules(i, self.rules.clone()))
            .collect()
    }

    /// Finds a rule by selector.
    fn select(&self, id: &str) -> PyResult<Vec<PyRule>> {
        let selector = Selector::try_from(id.to_owned())
            .map_err(|err| PyValueError::new_err(format!("{}", err)))?;

        Ok(self
            .rules
            .read()
            .rules()
            .iter()
            .enumerate()
            .filter(|(_, rule)| selector.is_match(rule.id()))
            .map(|(i, _)| PyRule::from_rules(i, self.rules.clone()))
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
    #[text_signature = "(text_or_texts)"]
    fn suggest(&self, py: Python, text_or_texts: PyObject) -> PyResult<PyObject> {
        text_guard(py, text_or_texts, |text| {
            self.rules
                .read()
                .suggest(&text)
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
            Ok(self.rules.read().correct(&text))
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

                Suggestion {
                    source: x.source().to_string(),
                    message: x.message().to_string(),
                    replacements: x.replacements().iter().map(|x| x.to_string()).collect(),
                    start: x.start(),
                    end: x.end(),
                }
            })
            .collect();

        apply_suggestions(text, &suggestions)
    }

    pub fn __setstate__(&mut self, py: Python, state: PyObject) -> PyResult<()> {
        match state.extract::<&PyBytes>(py) {
            Ok(s) => {
                let rules: Rules = bincode::deserialize(s.as_bytes()).map_err(|_| {
                    PyValueError::new_err("deserializing state with `bincode` failed")
                })?;
                // a roundtrip through pickle can not preserve references so we need to create a new Arc<RwLock<..>>
                self.rules = Arc::new(RwLock::new(rules));
                Ok(())
            }
            Err(e) => Err(e),
        }
    }

    pub fn __getstate__(&self, py: Python) -> PyResult<PyObject> {
        Ok(PyBytes::new(
            py,
            // rwlock serialization is transparent
            &bincode::serialize(&self.rules)
                .map_err(|_| PyValueError::new_err("serializing state with `bincode` failed"))?,
        )
        .to_object(py))
    }
}

#[pymodule]
fn spell(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<PySpell>()?;
    m.add_class::<PySpellOptions>()?;
    Ok(())
}

#[pymodule]
fn nlprule(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add("__version__", env!("CARGO_PKG_VERSION"))?;

    m.add_class::<PyTokenizer>()?;
    m.add_class::<PyRules>()?;
    m.add_class::<PySuggestion>()?;
    m.add_class::<PyToken>()?;

    m.add_wrapped(wrap_pymodule!(spell))?;

    Ok(())
}
