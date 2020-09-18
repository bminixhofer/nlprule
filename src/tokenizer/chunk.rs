use super::IncompleteToken;
use pyo3::{prelude::*, types::PyList};
use serde::Deserialize;
use std::collections::HashMap;
use std::fs::File;
use std::io::BufReader;
use tokenizers::models::bpe::BPE;
use tokenizers::normalizers::bert::BertNormalizer;
use tokenizers::pre_tokenizers::byte_level::ByteLevel;
use tokenizers::processors::bert::BertProcessing;
use tokenizers::tokenizer::{EncodeInput, Tokenizer};

#[derive(Deserialize, Debug)]
struct JSONVocab(Vec<HashMap<String, String>>);

pub struct Chunker {
    module: Py<PyModule>,
    vocab: Vec<HashMap<usize, String>>,
    tokenizer: Tokenizer,
}

impl Chunker {
    pub fn new() -> Result<Self, Box<dyn std::error::Error + Send + Sync>> {
        let module = PyModule::from_code(
            Python::acquire_gil().python(),
            include_str!("./chunk.py"),
            "chunk.py",
            "chunk",
        )
        .unwrap()
        .into();

        let file = File::open("src/tokenizer/train-chunker/vocab.json")?;
        let reader = BufReader::new(file);

        let vocab: JSONVocab = serde_json::from_reader(reader)?;
        let vocab = vocab
            .0
            .into_iter()
            .map(|map| {
                map.into_iter()
                    .map(|x| (x.0.parse().unwrap(), x.1))
                    .collect()
            })
            .collect();

        let bpe = BPE::from_files(
            "src/tokenizer/train-chunker/distilroberta-base-vocab.json",
            "src/tokenizer/train-chunker/distilroberta-base-merges.txt",
        )
        .unk_token("<unk>".into())
        .build()?;
        let mut tokenizer = Tokenizer::new(Box::new(bpe));
        tokenizer.with_normalizer(Box::new(BertNormalizer::new(true, true, false, false)));
        tokenizer.with_pre_tokenizer(Box::new(ByteLevel::new(false, true)));

        let sep = tokenizer.get_model().token_to_id("</s>").unwrap();
        let cls = tokenizer.get_model().token_to_id("<s>").unwrap();
        tokenizer.with_post_processor(Box::new(BertProcessing::new(
            ("</s>".into(), sep),
            ("<s>".into(), cls),
        )));

        Ok(Chunker {
            module,
            vocab,
            tokenizer,
        })
    }

    fn decode_label(&self, pred: &[f32], group: usize) -> &str {
        let mut max = -f32::INFINITY;
        let mut argmax = 0;

        let offset = (0..group)
            .map(|i| self.vocab[i].keys().max().unwrap() + 1)
            .sum();
        let n_labels = *self.vocab[group].keys().max().unwrap() + 1;

        for (i, x) in pred[offset..(offset + n_labels)].iter().enumerate() {
            if *x > max {
                max = *x;
                argmax = i;
            }
        }

        self.vocab[group].get(&argmax).unwrap()
    }

    fn predict(
        &self,
        py: Python,
        input_ids: Vec<Vec<u32>>,
        attention_mask: Vec<Vec<u32>>,
    ) -> PyResult<Vec<Vec<Vec<f32>>>> {
        let out: &PyList = self
            .module
            .as_ref(py)
            .call1("predict", (input_ids, attention_mask))?
            .downcast()?;

        out.extract()
    }

    pub fn apply(&self, text: &str, tokens: &mut Vec<IncompleteToken>) -> PyResult<()> {
        let guard = Python::acquire_gil();
        let py = guard.python();

        let encoded = self
            .tokenizer
            .encode(EncodeInput::Single(text.into()), true)
            .unwrap();

        let input_ids = vec![encoded.get_ids().to_vec()];
        let attention_mask = vec![encoded.get_attention_mask().to_vec()];
        let offsets = encoded.get_offsets();

        let mut preds = self.predict(py, input_ids, attention_mask)?;
        let preds = preds.remove(0);

        let mut chunks: Vec<(usize, usize, &str)> = Vec::new();

        for (i, pred) in preds.iter().enumerate() {
            let label = self.decode_label(pred, 1);

            let is_same = !chunks.is_empty() && chunks[chunks.len() - 1].2 == label;

            if is_same {
                let end = chunks.len() - 1;
                chunks[end].1 = i;
            } else {
                chunks.push((i, i, label));
            }
        }

        for (i, chunk) in chunks.iter().enumerate() {
            for token in tokens.iter_mut() {
                if token.byte_span.0 >= offsets[chunk.0].0
                    && token.byte_span.1 <= offsets[chunk.1].1
                {
                    token.chunks = if chunk.2.ends_with("NP") {
                        let mut start = chunk.0;
                        for x in chunks[0..i].iter().rev() {
                            if x.2 == "I-NP" || chunk.2 == "B-NP" {
                                start = x.0;
                            } else {
                                break;
                            }
                        }

                        let mut end = chunk.1;
                        for x in chunks[(i + 1)..chunks.len()].iter() {
                            if x.2 == "I-NP" {
                                end = x.1;
                            } else {
                                break;
                            }
                        }

                        let is_plural =
                            (start..(end + 1)).any(|x| self.decode_label(&preds[x], 2) == "plural");
                        let number_name = if is_plural { "plural" } else { "singular" };

                        vec![format!("{}-{}", chunk.2, number_name)]
                    } else {
                        vec![chunk.2.to_string()]
                    };
                }
            }
        }

        for i in 0..tokens.len() {
            if tokens[i].chunks.is_empty() {
                continue;
            }

            let chunk = tokens[i].chunks[0].as_str();

            if chunk.contains("NP")
                && (i >= tokens.len() - 1
                    || tokens[i + 1].chunks.is_empty()
                    || !tokens[i + 1].chunks[0].starts_with("I-NP"))
            {
                let end_name = format!("E{}", chunk.chars().skip(1).collect::<String>());

                if chunk.starts_with("B-NP") {
                    tokens[i].chunks.push(end_name);
                } else {
                    tokens[i].chunks = vec![end_name];
                }
            }
        }

        Ok(())
    }
}
