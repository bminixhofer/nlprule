use super::IncompleteToken;
use fasthash::{xx::Hash64, FastHash};
use serde::Deserialize;
use std::collections::HashMap;
use tract_ndarray::prelude::*;
use tract_onnx::prelude::*;

#[derive(Deserialize, Debug)]
struct JSONVocab(Vec<HashMap<String, String>>);

fn get_shape(word: &str) -> String {
    let mut shape = Vec::new();
    let mut prev_char_shape: char;
    let mut char_shape: char = ' ';
    let mut consecutive_count = 0;

    for c in word.chars() {
        prev_char_shape = char_shape;

        char_shape = if c.is_uppercase() {
            'X'
        } else if c.is_lowercase() {
            'x'
        } else if c.is_digit(10) {
            'd'
        } else {
            c
        };

        consecutive_count = if char_shape == prev_char_shape {
            consecutive_count + 1
        } else {
            0
        };

        if consecutive_count < 4 {
            shape.push(char_shape)
        }
    }

    shape.into_iter().collect()
}

#[allow(clippy::needless_collect)]
fn tokenize(tokens: &[&str], vocab_lengths: &[u64]) -> Array2<i64> {
    let mut input = ArrayBase::zeros((tokens.len(), 5));

    for (i, token) in tokens.iter().enumerate() {
        let lower = token.to_lowercase();

        let ids = vec![
            1 + token.chars().next().unwrap().is_uppercase() as u64,
            1 + Hash64::hash_with_seed(&lower, 0) % (vocab_lengths[1] - 1),
            1 + Hash64::hash_with_seed(get_shape(token), 0) % (vocab_lengths[2] - 1),
            1 + Hash64::hash_with_seed(lower.chars().take(3).collect::<String>(), 0)
                % (vocab_lengths[3] - 1),
            1 + Hash64::hash_with_seed(
                {
                    let rev_chars = lower.chars().rev().take(3).collect::<Vec<_>>();
                    rev_chars.into_iter().rev().collect::<String>()
                },
                0,
            ) % (vocab_lengths[4] - 1),
        ];

        let ids: Vec<_> = ids.into_iter().map(|x| x as i64).collect();

        input.slice_mut(s![i, ..]).assign(&Array::from(ids));
    }

    input
}

pub struct Chunker {
    labels: &'static [&'static str],
    vocab_lengths: &'static [u64],
    model: TypedModel,
}

impl Chunker {
    pub fn new() -> TractResult<Self> {
        let labels = &[
            "<pad>", "B-NP", "E-NP", "B-VP", "O", "S-NP", "B-ADVP", "I-VP", "I-NP", "B-PP",
            "B-PRT", "B-ADJP", "I-ADJP", "B-SBAR", "I-ADVP", "I-SBAR", "B-INTJ", "I-INTJ", "B-LST",
            "I-PP", "B-CONJP", "I-CONJP", "B-UCP",
        ];

        let model = onnx()
            .model_for_path("../chunker/model.onnx")?
            .with_input_fact(
                0,
                InferenceFact::dt_shape(i64::datum_type(), tvec!(1.into(), TDim::s(), 5.into())),
            )?
            .into_optimized()?;

        Ok(Chunker {
            labels,
            vocab_lengths: &[3, 100_001, 1001, 10_001, 10_001],
            model,
        })
    }

    pub fn apply(&self, tokens: &mut Vec<IncompleteToken>) -> TractResult<()> {
        let texts: Vec<_> = tokens.iter().map(|x| x.word.text.as_str()).collect();

        let input = tokenize(&texts, self.vocab_lengths);
        let input = input.insert_axis(Axis(0));

        let optimized_model = self
            .model
            .concretize_stream_dim(input.shape()[1])?
            .optimize()?
            .into_runnable()?;

        let output: ArrayD<f32> = (*optimized_model.run(tvec!(input.into()))?[0])
            .clone()
            .into_array()?;

        let chunks = output
            .axis_iter(Axis(1))
            .map(|value| {
                let argmax = value
                    .iter()
                    .enumerate()
                    .fold(None, |m, (i, &x)| {
                        m.map_or(Some((i, x)), |mv: (usize, f32)| {
                            Some(if x > mv.1 { (i, x) } else { mv })
                        })
                    })
                    .unwrap()
                    .0;

                self.labels[argmax]
            })
            .collect::<Vec<_>>();

        let mut number_mask: Vec<_> = chunks.iter().map(|_| false).collect();

        for i in 0..chunks.len() {
            if chunks[i].ends_with("-NP") {
                let mut end_idx = i;

                for (j, chunk) in chunks.iter().enumerate().skip(i + 1) {
                    if tokens[j].has_space_before && *chunk != "I-NP" && *chunk != "E-NP" {
                        break;
                    }

                    end_idx = j;
                }

                let is_plural = (i..(end_idx + 1))
                    .any(|idx| tokens[idx].word.tags.iter().any(|x| x.pos == "NNS"));

                for switch in number_mask[i..(end_idx + 1)].iter_mut() {
                    if is_plural {
                        *switch = true;
                    }
                }
            }
        }

        for i in 0..chunks.len() {
            if chunks[i].ends_with("-NP") {
                let number = if number_mask[i] { "plural" } else { "singular" };

                tokens[i].chunks = match chunks[i] {
                    "B-NP" => vec![format!("B-NP-{}", number)],
                    "I-NP" => vec![format!("I-NP-{}", number)],
                    "E-NP" => vec![format!("E-NP-{}", number)],
                    "S-NP" => vec![format!("B-NP-{}", number), format!("E-NP-{}", number)],
                    x => panic!("invalid chunk {}", x),
                };
            } else {
                tokens[i].chunks.push(chunks[i].to_string());
            }
        }

        Ok(())
    }
}
