use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use super::IncompleteToken;

fn softmax(vec: &mut Vec<f32>) {
    for x in vec.iter_mut() {
        *x = x.exp();
    }

    let sum = vec.iter().fold(0., |a, b| a + b);
    for x in vec.iter_mut() {
        *x /= sum;
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct Context {
    parameters: Vec<f32>,
    outcomes: Vec<usize>,
}

#[derive(Debug, Clone)]
struct Sequence<'a> {
    outcomes: Vec<&'a str>,
    probs: Vec<f32>,
}

impl<'a> Default for Sequence<'a> {
    fn default() -> Self {
        Sequence {
            outcomes: Vec::new(),
            probs: Vec::new(),
        }
    }
}

impl<'a> Sequence<'a> {
    pub fn new(outcomes: Vec<&'a str>, probs: Vec<f32>) -> Self {
        Sequence { outcomes, probs }
    }

    pub fn log_prob(&self) -> f32 {
        self.probs.iter().fold(0., |a, b| a + b.ln())
    }

    #[inline]
    pub fn outcomes(&self) -> &[&'a str] {
        &self.outcomes
    }

    #[inline]
    pub fn probs(&self) -> &[f32] {
        &self.probs
    }
}

#[derive(Serialize, Deserialize)]
struct Model {
    outcome_labels: Vec<String>,
    pmap: HashMap<String, Context>,
}

impl Model {
    pub fn eval(&self, context: &[String]) -> Vec<f32> {
        let scontexts: Vec<Option<&Context>> = context.iter().map(|x| self.pmap.get(x)).collect();
        let mut prior =
            vec![(1. / (self.outcome_labels.len() as f32)).ln(); self.outcome_labels.len()];

        for context in scontexts {
            if let Some(context) = context {
                for (idx, param) in context.outcomes.iter().zip(context.parameters.iter()) {
                    prior[*idx] += param;
                }
            }
        }

        softmax(&mut prior);
        prior
    }

    pub fn get_top_n(&self, probs: &[f32], n: usize) -> Vec<(usize, f32, &str)> {
        let mut probs: Vec<_> = probs.iter().enumerate().collect();
        probs.sort_by(|a, b| b.1.partial_cmp(a.1).unwrap());

        probs
            .into_iter()
            .map(|(i, p)| (i, *p, self.outcome_labels[i].as_str()))
            .take(n)
            .collect::<Vec<_>>()
    }

    fn beam_search<
        S,
        C: Fn(&[S], &[&str], usize) -> Vec<String>,
        V: Fn(&[S], &[&str], usize, &str) -> bool,
    >(
        &self,
        tokens: &[S],
        context_fn: C,
        valid_fn: V,
        size: usize,
    ) -> Vec<Sequence> {
        let mut prev: Vec<Sequence> = vec![Sequence::default()];
        let mut next: Vec<Sequence> = Vec::with_capacity(size);

        for i in 0..tokens.len() {
            prev.sort_by(|a, b| b.log_prob().partial_cmp(&a.log_prob()).unwrap());
            prev = prev.into_iter().take(size).collect();

            for seq in prev.iter() {
                let context = context_fn(tokens, &seq.outcomes(), i);
                let scores = self.eval(&context);

                for (_, p, pred) in self.get_top_n(&scores, scores.len()) {
                    if valid_fn(tokens, &seq.outcomes(), i, pred) {
                        let mut next_outcomes: Vec<_> = seq.outcomes().to_vec();
                        next_outcomes.push(pred);

                        let mut next_probs: Vec<_> = seq.probs().to_vec();
                        next_probs.push(p);

                        next.push(Sequence::new(next_outcomes, next_probs));
                    }
                }
            }

            prev = next.clone();
            next.clear();
        }

        prev
    }
}

#[derive(Serialize, Deserialize)]
struct MaxentTokenizer {
    model: Model,
}

impl MaxentTokenizer {
    fn add_char_context(key: &str, c: char, context: &mut Vec<String>) {
        macro_rules! add {
            ($x: expr) => {
                context.push(format!("{}{}", key, $x));
            };
        }

        context.push(format!("{}={}", key, c));

        if c.is_alphabetic() {
            add!("_alpha");
            if c.is_uppercase() {
                add!("_caps");
            }
        } else if c.is_digit(10) {
            add!("_num");
        } else if c.is_whitespace() {
            add!("_ws");
        } else if ['.', '?', '!'].contains(&c) {
            add!("_eos");
        } else if ['`', '"', '\''].contains(&c) {
            add!("_quote");
        } else if ['[', '{', '('].contains(&c) {
            add!("_lp");
        } else if [']', '}', ')'].contains(&c) {
            add!("_rp");
        }
    }

    fn context(chars: &[char], i: usize) -> Vec<String> {
        let mut context = Vec::new();

        let prefix: String = chars[..i].iter().collect();
        let suffix: String = chars[i..].iter().collect();

        context.push(format!("p={}", prefix));
        context.push(format!("s={}", suffix));

        if i > 0 {
            Self::add_char_context("p1", chars[i - 1], &mut context);
            if i > 1 {
                Self::add_char_context("p2", chars[i - 2], &mut context);
                context.push(format!("p21={}{}", chars[i - 2], chars[i - 1]));
            } else {
                context.push("p2=bok".into());
            }
            context.push(format!("p1f1={}{}", chars[i - 1], chars[i]));
        } else {
            context.push("b1=bok".into());
        }

        Self::add_char_context("f1", chars[i], &mut context);
        if i + 1 < chars.len() {
            Self::add_char_context("f2", chars[i + 1], &mut context);
            context.push(format!("f12={}{}", chars[i], chars[i + 1]));
        } else {
            context.push("f2=bok".into());
        }

        if chars[0] == '&' && chars[chars.len() - 1] == ';' {
            context.push("cc".into()); // character code
        }

        // TODO: add abbreviations
        if i == chars.len() - 1 && [].contains(&chars) {
            context.push("pabb".into());
        }

        context
    }

    fn tokenize<'a>(&self, text: &'a str) -> Vec<&'a str> {
        let mut tokens = Vec::new();
        let char_indices: Vec<_> = text.char_indices().collect();

        for pre_token in text.split_whitespace() {
            let byte_offset = pre_token.as_ptr() as usize - text.as_ptr() as usize;
            let char_start = char_indices
                .iter()
                .position(|(i, _)| *i == byte_offset)
                .unwrap();
            let byte_start = char_indices[char_start].0;

            let token_char_indices =
                &char_indices[char_start..char_start + pre_token.chars().count()];
            let token_chars: Vec<_> = token_char_indices.iter().map(|(_, c)| c).cloned().collect();

            if (token_char_indices.len() < 2)
                || pre_token.chars().all(|x| x.is_ascii_alphanumeric())
            {
                tokens.push(pre_token);
            } else {
                let mut start = byte_start;

                for i in 1..token_char_indices.len() {
                    let context = Self::context(&token_chars, i);
                    let (_, _, best) = self.model.get_top_n(&self.model.eval(&context), 1)[0];

                    if best == "T" {
                        tokens.push(&text[start..token_char_indices[i].0]);
                        start = token_char_indices[i].0;
                    }
                }

                tokens.push(&text[start..byte_start + pre_token.len()]);
            }
        }

        tokens
    }
}

#[derive(Serialize, Deserialize)]
struct MaxentPosTagger {
    model: Model,
    tagdict: HashMap<String, Vec<String>>,
}

impl MaxentPosTagger {
    fn get_suffixes_prefixes(string: &str) -> Vec<String> {
        let chars: Vec<_> = string.chars().collect();
        let mut output = Vec::new();

        for i in 0..4 {
            output.push(format!(
                "suf={}",
                chars[std::cmp::max((chars.len() as isize) - 1 - i, 0) as usize..]
                    .iter()
                    .collect::<String>()
            ));
        }

        for i in 0..4 {
            output.push(format!(
                "pre={}",
                chars[..std::cmp::min(i + 1, chars.len())]
                    .iter()
                    .collect::<String>()
            ));
        }

        output
    }

    fn context(tokens: &[&str], tags: &[&str], i: usize) -> Vec<String> {
        let mut context = Vec::new();

        let lex = tokens[i];
        let next = if i + 1 < tokens.len() {
            tokens[i + 1]
        } else {
            "*SE*"
        };
        let nextnext = if i + 2 < tokens.len() {
            tokens[i + 2]
        } else {
            "*SE*"
        };
        let (prev, tagprev) = if i >= 1 {
            (tokens[i - 1], Some(tags[i - 1]))
        } else {
            ("*SB*", None)
        };
        let (prevprev, tagprevprev) = if i >= 2 {
            (tokens[i - 2], Some(tags[i - 2]))
        } else {
            ("*SB*", None)
        };

        context.push("default".into());
        context.push(format!("w={}", lex));

        context.extend(Self::get_suffixes_prefixes(&lex));

        if lex.contains('-') {
            context.push("h".into());
        }
        if lex.chars().any(|c| c.is_ascii_uppercase()) {
            context.push("c".into());
        }
        if lex.chars().any(|c| c.is_ascii_digit()) {
            context.push("d".into());
        }

        context.push(format!("p={}", prev));
        if prev != "*SB*" {
            context.push(format!("pp={}", prevprev));
        }

        context.push(format!("n={}", next));
        if next != "*SE*" {
            context.push(format!("nn={}", nextnext));
        }

        if let Some(tagprev) = tagprev {
            context.push(format!("t={}", tagprev));

            if let Some(tagprevprev) = tagprevprev {
                context.push(format!("t2={},{}", tagprevprev, tagprev));
            }
        }

        context
    }

    fn valid(&self, tokens: &[&str], _outcomes: &[&str], i: usize, outcome: &str) -> bool {
        if let Some(allowed) = self.tagdict.get(tokens[i]) {
            allowed.contains(&outcome.to_string())
        } else {
            true
        }
    }

    fn tag(&self, tokens: &[&str]) -> Sequence {
        self.model
            .beam_search(
                tokens,
                Self::context,
                |a, b, c, d| self.valid(a, b, c, d),
                3,
            )
            .remove(0)
    }
}

#[derive(Serialize, Deserialize)]
struct MaxentChunker {
    model: Model,
}

impl MaxentChunker {
    #[allow(clippy::manual_strip)]
    fn valid(&self, _input: &[(&str, &str)], outcomes: &[&str], _i: usize, outcome: &str) -> bool {
        if outcome.starts_with("I-") {
            if let Some(prev_outcome) = outcomes.iter().rev().next() {
                // can byte index here because "I, B, E, -" are all 1 byte
                *prev_outcome == "O" || prev_outcome[2..] == outcome[2..]
            } else {
                false
            }
        } else {
            true
        }
    }

    fn context(input: &[(&str, &str)], preds: &[&str], i: usize) -> Vec<String> {
        let (tokens, tags): (Vec<&str>, Vec<&str>) = input.iter().cloned().unzip();

        let (w_2, t_2, p_2) = if i < 2 {
            ("w_2=bos".into(), "t_2=bos".into(), "p_2=bos".into())
        } else {
            (
                format!("w_2={}", tokens[i - 2]),
                format!("t_2={}", tags[i - 2]),
                format!("p_2{}", preds[i - 2]),
            )
        };

        let (w_1, t_1, p_1) = if i < 1 {
            ("w_1=bos".into(), "t_1=bos".into(), "p_1=bos".into())
        } else {
            (
                format!("w_1={}", tokens[i - 1]),
                format!("t_1={}", tags[i - 1]),
                format!("p_1={}", preds[i - 1]),
            )
        };

        let w0 = format!("w0={}", tokens[i]);
        let t0 = format!("t0={}", tags[i]);

        let (w1, t1) = if i + 1 >= tokens.len() {
            ("w1=eos".into(), "t1=eos".into())
        } else {
            (
                format!("w1={}", tokens[i + 1]),
                format!("t1={}", tags[i + 1]),
            )
        };

        let (w2, t2) = if i + 2 >= tokens.len() {
            ("w2=eos".into(), "t2=eos".into())
        } else {
            (
                format!("w2={}", tokens[i + 2]),
                format!("t2={}", tags[i + 2]),
            )
        };

        return vec![
            // add word features
            w_2.clone(),
            w_1.clone(),
            w0.clone(),
            w1.clone(),
            w2.clone(),
            w_1.clone() + &w0,
            w0.clone() + &w1,
            // add tag features
            t_2.clone(),
            t_1.clone(),
            t0.clone(),
            t1.clone(),
            t2.clone(),
            t_2.clone() + &t_1,
            t_1.clone() + &t0,
            t0.clone() + &t1,
            t1.clone() + &t2,
            t_2.clone() + &t_1 + &t0,
            t_1.clone() + &t0 + &t1,
            t0.clone() + &t1 + &t2,
            // add pred tags
            p_2.clone(),
            p_1.clone(),
            p_2 + &p_1,
            // add pred and tag
            p_1.clone() + &t_2,
            p_1.clone() + &t_1,
            p_1.clone() + &t0,
            p_1.clone() + &t1,
            p_1.clone() + &t2,
            p_1.clone() + &t_2 + &t_1,
            p_1.clone() + &t_1 + &t0,
            p_1.clone() + &t0 + &t1,
            p_1.clone() + &t1 + &t2,
            p_1.clone() + &t_2 + &t_1 + &t0,
            p_1.clone() + &t_1 + &t0 + &t1,
            p_1.clone() + &t0 + &t1 + &t2,
            // add pred and word
            p_1.clone() + &w_2,
            p_1.clone() + &w_1,
            p_1.clone() + &w0,
            p_1.clone() + &w1,
            p_1.clone() + &w2,
            p_1.clone() + &w_1 + &w0,
            p_1.clone() + &w0 + &w1,
        ];
    }

    fn chunk(&self, input: &[(&str, &str)]) -> Sequence {
        self.model
            .beam_search(
                input,
                Self::context,
                |a, b, c, d| self.valid(a, b, c, d),
                10,
            )
            .remove(0)
    }
}

#[derive(Serialize, Deserialize)]
pub struct Chunker {
    token_model: MaxentTokenizer,
    pos_model: MaxentPosTagger,
    chunk_model: MaxentChunker,
}

impl Chunker {
    pub fn apply(&self, text: &str, tokens: &mut Vec<IncompleteToken>) {
        // replacements must not change char indices
        let text = text.replace('’', "\'");

        let mut byte_to_char_idx: HashMap<usize, usize> = text
            .char_indices()
            .enumerate()
            .map(|(ci, (bi, _))| (bi, ci))
            .collect();
        byte_to_char_idx.insert(text.len(), text.chars().count());

        let internal_tokens = self.token_model.tokenize(&text);
        let tags = self.pos_model.tag(&internal_tokens);
        let chunks = self.chunk_model.chunk(
            &internal_tokens
                .iter()
                .cloned()
                .zip(tags.outcomes().iter().cloned())
                .collect::<Vec<_>>(),
        );

        let internal_chunks: Vec<_> = chunks
            .outcomes()
            .iter()
            .zip(internal_tokens)
            .map(|(chunk, token)| {
                let byte_start = token.as_ptr() as usize - text.as_ptr() as usize;
                let char_start = *byte_to_char_idx.get(&byte_start).unwrap();
                let char_end = *byte_to_char_idx.get(&(byte_start + token.len())).unwrap();

                (*chunk, (char_start, char_end))
            })
            .collect();
        let mut chunks = Vec::new();
        let mut number = "singular";

        for i in 0..internal_chunks.len() {
            let chunk = internal_chunks[i].0;

            if chunk == "B-NP" {
                number = "singular";

                for (next_chunk, char_span) in internal_chunks[i..].iter().cloned() {
                    if next_chunk != "I-NP" && next_chunk != "B-NP" {
                        break;
                    }

                    if tokens
                        .iter()
                        .find(|token| token.char_span == char_span)
                        .map(|token| token.word.tags.iter().any(|tag| tag.pos == "NNS"))
                        .unwrap_or(false)
                    {
                        number = "plural";
                    }
                }
            }

            let is_noun_end = i + 1 >= internal_chunks.len() || internal_chunks[i + 1].0 != "I-NP";
            let mut to_push = Vec::new();

            if chunk == "B-NP" {
                to_push.push(format!("B-NP-{}", number));
                if is_noun_end {
                    to_push.push(format!("E-NP-{}", number));
                }
            } else if chunk == "I-NP" {
                if is_noun_end {
                    to_push.push(format!("E-NP-{}", number));
                } else {
                    to_push.push(format!("I-NP-{}", number));
                }
            } else {
                to_push.push(chunk.into())
            }

            chunks.push(to_push);
        }

        for token in tokens.iter_mut() {
            for (chunk, (_, char_span)) in chunks.iter().zip(internal_chunks.iter()) {
                if *char_span == token.char_span {
                    token.chunks = (*chunk).clone();
                }
            }
        }
    }
}
