use super::IncompleteToken;
use pyo3::{
    prelude::*,
    types::{PyDict, PyList, PyModule},
};

pub struct Chunker {
    module: Py<PyModule>,
}

impl Default for Chunker {
    fn default() -> Self {
        Chunker::new()
    }
}

impl Chunker {
    pub fn new() -> Self {
        Chunker {
            module: PyModule::from_code(
                Python::acquire_gil().python(),
                include_str!("./chunk.py"),
                "chunk.py",
                "chunk",
            )
            .unwrap()
            .into(),
        }
    }

    pub fn apply(&self, text: &str, tokens: &mut Vec<IncompleteToken>) -> PyResult<()> {
        let guard = Python::acquire_gil();
        let py = guard.python();

        let out: &PyList = self.module.as_ref(py).call1("chunk", (text,))?.downcast()?;

        for chunk in out.iter() {
            let chunk: &PyDict = chunk.downcast()?;
            let chunk_char_span: (usize, usize) = chunk.get_item("range").unwrap().extract()?;
            let tag: String = chunk.get_item("tag").unwrap().extract()?;

            let mut i = 0;

            for j in 0..tokens.len() {
                // if token span is inside chunk span
                if tokens[j].char_span.0 >= chunk_char_span.0
                    && tokens[j].char_span.1 <= chunk_char_span.1
                {
                    if tokens[j].chunk.is_some() {
                        continue;
                    }

                    if tag.starts_with("NP") // if at the end of chunk
                        && (j == tokens.len() - 1
                            || (tokens[j + 1].char_span.1 > chunk_char_span.1))
                    {
                        if i == 0 {
                            tokens[j].chunk = Some(format!("B-{0}|E-{0}", tag))
                        } else {
                            tokens[j].chunk = Some(format!("E-{0}", tag))
                        }
                    } else if i == 0 {
                        tokens[j].chunk = Some(format!("B-{}", tag))
                    } else {
                        tokens[j].chunk = Some(format!("I-{}", tag))
                    }

                    i += 1;
                }
            }
        }

        Ok(())
    }
}
