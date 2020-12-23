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
            .map_err(|x| x.print(Python::acquire_gil().python()))
            .unwrap()
            .into(),
        }
    }

    pub fn apply(&self, text: &str, tokens: &mut Vec<IncompleteToken>) -> PyResult<()> {
        let guard = Python::acquire_gil();
        let py = guard.python();

        let out: &PyList = self.module.as_ref(py).call1("chunk", (text,))?.downcast()?;
        let mut chunks = vec![(String::new(), -1isize); tokens.len()];

        for (token, token_chunk) in tokens.iter().zip(chunks.iter_mut()) {
            for (i, chunk) in out.iter().enumerate() {
                let chunk: &PyDict = chunk.downcast()?;
                let chunk_char_span: (usize, usize) = chunk.get_item("range").unwrap().extract()?;

                // if token span is inside chunk span
                if token.char_span.0 >= chunk_char_span.0 && token.char_span.1 <= chunk_char_span.1
                {
                    token_chunk.0 = chunk.get_item("tag").unwrap().extract()?;
                    token_chunk.1 = i as isize;
                }
            }
        }

        for (i, chunk) in chunks.iter().enumerate() {
            tokens[i].chunks = match chunk.0.as_str() {
                "PP" => vec!["B-PP".into()],
                "VP" => {
                    if i == 0 || chunks[i - 1].1 != chunk.1 {
                        vec!["B-VP".into()]
                    } else {
                        vec!["I-VP".into()]
                    }
                }
                "NP-singular" | "NP-plural" => {
                    let mut out = Vec::new();

                    if i == 0 || chunks[i - 1].1 != chunk.1 {
                        out.push(format!("B-{}", chunk.0));
                    }
                    if i == tokens.len() - 1 || chunks[i + 1].1 != chunk.1 {
                        out.push(format!("E-{}", chunk.0));
                    }

                    if out.is_empty() {
                        out.push(format!("I-{}", chunk.0))
                    }

                    out
                }
                _ => Vec::new(),
            }
        }

        Ok(())
    }
}
