use std::convert::TryInto;

use lazy_static::lazy_static;
use nlprule::{lang::en, properties::*, rule::id::Category, types::Position};
use quickcheck_macros::quickcheck;

lazy_static! {
    static ref ANALYZER: en::Analyzer = en::analyzer();
    static ref CORRECTER: en::Correcter = en::correcter();
}

#[test]
fn can_analyze_empty_text() {
    let sentences: Vec<_> = ANALYZER.tokenize("").collect();
    assert!(sentences.is_empty());
}

#[test]
fn handles_whitespace_correctly() {
    // preceding whitespace has to be included, trailing whitespace behavior is unspecified
    let text = "  hello.\ttest.\t\t";

    let mut sentences = ANALYZER.tokenize(text);
    assert_eq!(
        &text[sentences.next().unwrap().span().byte().clone()],
        "  hello.\t"
    );
    assert_eq!(
        &text[sentences.next().unwrap().span().byte().clone()],
        "test.\t"
    );
    assert!(sentences.next().is_none());
}

#[quickcheck]
fn can_analyze_anything(text: String) -> bool {
    let _: Vec<_> = ANALYZER.tokenize(&text).collect();
    true
}

#[test]
fn suggest_indices_are_relative_to_input_text() {
    let suggestions: Vec<_> = CORRECTER
        .suggest("I can due his homework for 10€. I can due his homework.")
        .flatten()
        .collect();

    assert_eq!(*suggestions[0].span().char(), 6..9);
    assert_eq!(*suggestions[0].span().byte(), 6..9);

    assert_eq!(*suggestions[1].span().char(), 38..41);
    assert_eq!(
        *suggestions[1].span().byte(),
        38 + '€'.len_utf8() - 1..41 + '€'.len_utf8() - 1
    );
}

#[test]
fn sentence_spans_correct() {
    let text = "A short test. A test with emoji 😊.";

    let sentences: Vec<_> = ANALYZER.tokenize(text).collect();
    assert_eq!(sentences.len(), 2);

    assert_eq!(*sentences[0].span().char(), 0..14);
    assert_eq!(*sentences[0].span().byte(), 0..14);

    assert_eq!(*sentences[1].span().char(), 14..34);
    assert_eq!(*sentences[1].span().byte(), 14..37);
}

#[test]
fn token_spans_correct() {
    let text = "A short test. A test with emoji 😊.";

    let tokens: Vec<_> = ANALYZER
        .tokenize(text)
        .map(|x| x.into_iter())
        .flatten()
        .collect();
    assert_eq!(*tokens[0].span().byte(), 0..1);
    assert_eq!(*tokens[0].span().char(), 0..1);

    assert_eq!(*tokens[2].span().char(), 8..12);
    assert_eq!(*tokens[2].span().byte(), 8..12);

    assert_eq!(*tokens[tokens.len() - 2].span().char(), 32..33);
    assert_eq!(*tokens[tokens.len() - 2].span().byte(), 32..36);

    assert_eq!(*tokens[tokens.len() - 1].span().char(), 33..34);
    assert_eq!(*tokens[tokens.len() - 1].span().byte(), 36..37);
}

#[quickcheck]
fn no_gaps_between_sentences(text: String) {
    let mut prev_pos = Position::default();
    let mut contains_sentence = false;

    for sentence in ANALYZER.tokenize(&text) {
        assert_eq!(sentence.span().start(), prev_pos);
        prev_pos += sentence.span().len();

        contains_sentence = true;
    }

    assert_eq!(contains_sentence, !text.trim().is_empty());
}

#[test]
fn rules_can_be_disabled_enabled() {
    let mut correcter = CORRECTER.clone();

    // enabled by default
    assert!(correcter
        .suggest("I can due his homework")
        .flatten()
        .next()
        .is_some());

    correcter
        .components_mut()
        .1
        .select_mut(
            &Category::new("confused_words")
                .join("confusion_due_do")
                .into(),
        )
        .for_each(|x| x.disable());

    // disabled now
    assert!(correcter
        .suggest("I can due his homework")
        .flatten()
        .next()
        .is_none());

    // disabled by default
    assert!(correcter.suggest("I can not go").flatten().next().is_none());

    correcter
        .components_mut()
        .1
        .select_mut(&"typos/can_not".try_into().unwrap())
        .for_each(|x| x.enable());

    // enabled now
    assert!(correcter.suggest("I can not go").flatten().next().is_some());
}

#[test]
fn pipelines_work_with_references() -> Result<(), crate::Error> {
    let _pipeline = Pipeline::new((&*ANALYZER, &CORRECTER.components().1))?;

    Ok(())
}
