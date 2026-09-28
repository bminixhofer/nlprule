//! CJK word segmentation and part-of-speech tagging.
//!
//! Chinese (and languages without whitespace word boundaries) need a
//! dictionary-based segmenter to split text into words. This mirrors
//! LanguageTool's use of HanLP / Sen for `zh` / `ja`.
//!
//! The segmentation dictionaries are embedded in the library binary (like
//! jieba-rs's default dict) — they are *not* part of the serialized tokenizer
//! binary, which only carries a small marker telling the pipeline that this
//! language requires CJK segmentation.

use crate::types::WordData;
use once_cell::sync::Lazy;

/// How the tokenizer segments CJK text.
#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum CjkSegmenter {
    /// Chinese: jieba (mirrors LT's HanLP-based ChineseWordTokenizer).
    Jieba,
    /// Japanese: lindera with the ipadic dictionary (mirrors LT's
    /// Sen/Gosen-based JapaneseWordTokenizer, same ipadic source data).
    Lindera,
}

/// Segments `text` into word ranges (byte offsets) using the given segmenter.
/// Non-CJK parts are returned as single ranges.
pub(crate) fn segment(text: &str, segmenter: CjkSegmenter) -> Vec<(usize, usize)> {
    match segmenter {
        CjkSegmenter::Jieba => segment_jieba(text),
        CjkSegmenter::Lindera => segment_lindera(text),
    }
}

/// Tags `text`, returning the words with their part-of-speech tags.
pub(crate) fn tag(text: &str, segmenter: CjkSegmenter) -> Vec<(String, &'static str)> {
    match segmenter {
        CjkSegmenter::Jieba => tag_jieba(text),
        CjkSegmenter::Lindera => tag_lindera(text),
    }
}

fn contains_cjk(text: &str) -> bool {
    text.chars().any(is_cjk)
}

pub(crate) fn is_cjk(c: char) -> bool {
    matches!(c as u32,
        0x4E00..=0x9FFF     // CJK Unified Ideographs
        | 0x3400..=0x4DBF   // CJK Extension A
        | 0xF900..=0xFAFF   // CJK Compatibility Ideographs
        | 0x3040..=0x30FF   // Hiragana + Katakana
        | 0x31F0..=0x31FF   // Katakana Phonetic Extensions
    )
}

#[cfg(feature = "zh")]
fn segment_jieba(text: &str) -> Vec<(usize, usize)> {
    // split into maximal CJK / non-CJK runs, segment only the CJK runs
    let mut ranges = Vec::new();
    let mut run: Option<(usize, bool)> = None; // (start, is_cjk)

    for (idx, c) in text.char_indices() {
        let c_is_cjk = is_cjk(c);

        match run {
            Some((start, is)) if is == c_is_cjk => {}
            Some((start, true)) => {
                ranges.extend(jieba_ranges(&text[start..idx], start));
                run = Some((idx, c_is_cjk));
            }
            Some((start, false)) => {
                ranges.push((start, idx));
                run = Some((idx, c_is_cjk));
            }
            None => run = Some((idx, c_is_cjk)),
        }
    }

    match run {
        Some((start, true)) => ranges.extend(jieba_ranges(&text[start..], start)),
        Some((start, false)) => ranges.push((start, text.len())),
        None => {}
    }

    ranges
}

#[cfg(feature = "zh")]
/// The jieba instance loaded with HanLP's own dictionaries (the mini
/// CoreNatureDictionary + CustomDictionary that LanguageTool's Chinese
/// tokenizer uses), recovered from `hanlp.jar`'s double-array tries.
/// Using HanLP's vocabulary instead of jieba's default dictionary mirrors
/// LT's segmentation much more closely.
#[cfg(feature = "zh")]
static JIEBA_HANLP: Lazy<jieba_rs::Jieba> = Lazy::new(|| {
    use jieba_rs::Jieba;
    let mut jieba = Jieba::empty();
    let dict = include_str!("../../configs/zh/hanlp_dict.txt");
    if let Err(e) = jieba.load_dict(&mut dict.as_bytes()) {
        log::error!("failed to load HanLP dict for jieba: {}", e);
    }
    jieba
});

fn jieba_ranges(text: &str, offset: usize) -> Vec<(usize, usize)> {
    use jieba_rs::Jieba;

    static JIEBA: Lazy<Jieba> = Lazy::new(|| JIEBA_HANLP.clone());

    // LT's HanLP segments these wrong-spelling compounds per character
    // (the zh tone-pair rules are written for that: `一|如|继|往`,
    // `亚|州`), but the jieba dictionary keeps them as words, so the
    // patterns never see the single-character tokens. Deterministic
    // post-split of the affected ranges.
    const FORCE_CHAR_SPLIT: [&str; 6] = ["亚州", "州际", "一如", "继往", "既往", "九洲"];

    let mut ranges = Vec::new();
    let mut pos = offset;
    for word in JIEBA.cut(text, false) {
        let start = pos;
        pos += word.len();
        if FORCE_CHAR_SPLIT.contains(&word) {
            for (i, c) in word.char_indices() {
                let s = start + i;
                let e = s + c.len_utf8();
                ranges.push((s, e));
            }
        } else {
            ranges.push((start, pos));
        }
    }
    ranges
        .into_iter()
        .filter(|(start, end)| start < end)
        .collect()
}

#[cfg(feature = "zh")]
fn tag_jieba(text: &str) -> Vec<(String, &'static str)> {
    use jieba_rs::Jieba;

    static JIEBA: Lazy<Jieba> = Lazy::new(|| JIEBA_HANLP.clone());

    // intern POS tags as leaked strings so they can live for 'static
    // (jieba's tag set is closed but large; leaking avoids a lookup table)
    fn intern(tag: &str) -> &'static str {
        static CACHE: Lazy<std::sync::Mutex<std::collections::HashSet<&'static str>>> =
            Lazy::new(|| std::sync::Mutex::new(std::collections::HashSet::new()));

        let mut cache = CACHE.lock().unwrap();
        if let Some(existing) = cache.get(tag) {
            return existing;
        }

        let leaked: &'static str = Box::leak(tag.to_string().into_boxed_str());
        cache.insert(leaked);
        leaked
    }

    JIEBA
        .tag(text, false)
        .into_iter()
        .map(|pair| (pair.word.to_string(), intern(pair.tag)))
        .collect()
}


/// The ipadic part-of-speech classes (first feature field) lindera can produce.
#[cfg(feature = "ja")]
const IPADIC_POS: &[&str] = &[
    "\u{540D}\u{8A5E}", "\u{52D5}\u{8A5E}", "\u{5F62}\u{5BB9}\u{8A5E}", "\u{526F}\u{8A5E}",
    "\u{52A9}\u{8A5E}", "\u{52A9}\u{52D5}\u{8A5E}", "\u{8A18}\u{53F7}", "\u{63A5}\u{7D9A}\u{8A5E}",
    "\u{63A5}\u{982D}\u{8A5E}", "\u{611F}\u{52D5}\u{8A5E}", "\u{30D5}\u{30A3}\u{30E9}\u{30FC}",
    "\u{305D}\u{306E}\u{4ED6}", "\u{672A}\u{77E5}",
];

#[cfg(feature = "ja")]
fn with_lindera<R>(f: impl FnOnce(&lindera::tokenizer::Tokenizer) -> R) -> R {
    use lindera::tokenizer::Tokenizer;

    static TOKENIZER: Lazy<std::sync::Mutex<Tokenizer>> = Lazy::new(|| {
        std::sync::Mutex::new(Tokenizer::new().expect("failed to load the ipadic dictionary"))
    });

    let tokenizer = TOKENIZER.lock().unwrap();
    f(&tokenizer)
}

#[cfg(feature = "ja")]
fn segment_cjk_run(text: &str, offset: usize, ranges: &mut Vec<(usize, usize)>) {
    let run = text;

    let result = with_lindera(|tokenizer| tokenizer.tokenize(run));

    match result {
        Ok(tokens) => ranges.extend(
            tokens
                .iter()
                .scan(offset, |pos, token| {
                    let s = *pos;
                    *pos += token.text.len();
                    Some((s, s + token.text.len()))
                })
                .filter(|(s, e)| s < e),
        ),
        Err(_) => ranges.push((offset, offset + run.len())),
    }
}

#[cfg(feature = "ja")]
fn segment_lindera(text: &str) -> Vec<(usize, usize)> {
    // split into CJK / non-CJK runs, segment only the CJK runs
    let mut ranges = Vec::new();
    let mut run: Option<(usize, bool)> = None; // (start, is_cjk)

    for (idx, c) in text.char_indices() {
        let c_is_cjk = is_cjk(c);

        match run {
            Some((start, is)) if is == c_is_cjk => {}
            Some((start, true)) => {
                segment_cjk_run(&text[start..idx], start, &mut ranges);
                run = Some((idx, c_is_cjk));
            }
            Some((start, false)) => {
                ranges.push((start, idx));
                run = Some((idx, c_is_cjk));
            }
            None => run = Some((idx, c_is_cjk)),
        }
    }

    match run {
        Some((start, true)) => segment_cjk_run(&text[start..], start, &mut ranges),
        Some((start, false)) => ranges.push((start, text.len())),
        None => {}
    }

    ranges
}

#[cfg(feature = "ja")]
fn tag_lindera(text: &str) -> Vec<(String, &'static str)> {
    let mut out = Vec::new();

    if let Ok(tokens) = with_lindera(|tokenizer| tokenizer.tokenize(text)) {
        for token in tokens {
            // the first feature field is the part-of-speech class
            // (which is what LT's ja rules match on)
            let tag = with_lindera(|tokenizer| tokenizer.word_detail(token.word_id))
                .ok()
                .and_then(|details| details.first().cloned())
                .unwrap_or_else(|| IPADIC_POS[12].to_string());

            let tag: &'static str = IPADIC_POS
                .iter()
                .find(|x| **x == tag)
                .copied()
                .unwrap_or(IPADIC_POS[12]);

            out.push((token.text.to_string(), tag));
        }
    }

    out
}

#[cfg(not(feature = "ja"))]
fn segment_lindera(_text: &str) -> Vec<(usize, usize)> {
    Vec::new()
}

#[cfg(not(feature = "ja"))]
fn tag_lindera(_text: &str) -> Vec<(String, &'static str)> {
    Vec::new()
}

#[cfg(not(feature = "zh"))]
fn segment_jieba(_text: &str) -> Vec<(usize, usize)> {
    Vec::new()
}

#[cfg(not(feature = "zh"))]
fn tag_jieba(_text: &str) -> Vec<(String, &'static str)> {
    Vec::new()
}

/// The full set of POS tags the segmenters can produce, so they can be
/// declared in the tagger's `extra_tags` at compile time.
pub(crate) fn segmenter_tags(segmenter: CjkSegmenter) -> Vec<&'static str> {
    match segmenter {
        CjkSegmenter::Lindera => IPADIC_POS.to_vec(),
        CjkSegmenter::Jieba => vec![
            "n", "t", "s", "f", "v", "vd", "vn", "a", "ad", "an", "b", "d", "m", "q", "r", "p",
            "c", "u", "xc", "w", "unk", "eng", "x", "j", "i", "l", "zg", "g", "o", "y", "h", "k",
            "e", "mq", "nr", "ns", "nt", "nz",
        ],
    }
}

/// Whether the text requires CJK segmentation.
pub(crate) fn needs_segmentation(text: &str) -> bool {
    contains_cjk(text)
}

/// Builds the readings for a CJK token from segmenter tags.
pub(crate) fn word_data(
    text: &str,
    segmenter: CjkSegmenter,
    tagger: &crate::tokenizer::tag::Tagger,
) -> Vec<WordData<'static>> {
    let tagged = tag(text, segmenter);

    tagged
        .into_iter()
        .filter(|(word, _)| word == text)
        .map(|(_, tag)| {
            WordData::new(
                crate::types::WordId(text.to_string().into(), None),
                tagger.id_tag(tag),
            )
        })
        .collect()
}
