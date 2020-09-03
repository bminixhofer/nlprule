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

        for token in tokens {
            for chunk in out.iter() {
                let chunk: &PyDict = chunk.downcast()?;
                let chunk_char_span: (usize, usize) = chunk.get_item("range").unwrap().extract()?;

                // if token span is inside chunk span
                if token.char_span.0 >= chunk_char_span.0 && token.char_span.1 <= chunk_char_span.1
                {
                    token.chunk = Some(chunk.get_item("tag").unwrap().extract()?);
                }
            }
        }
        Ok(())
    }
}
