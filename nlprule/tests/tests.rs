use std::convert::TryInto;

use lazy_static::lazy_static;
use nlprule::{Rules, Tokenizer};
use quickcheck_macros::quickcheck;

const TOKENIZER_PATH: &str = "../storage/en_tokenizer.bin";
const RULES_PATH: &str = "../storage/en_rules.bin";

lazy_static! {
    static ref TOKENIZER: Tokenizer = Tokenizer::new(TOKENIZER_PATH).unwrap();
    static ref RULES: Rules = Rules::new(RULES_PATH).unwrap();
}

#[test]
fn can_tokenize_empty_text() {
    TOKENIZER.pipe("");
}

#[quickcheck]
fn can_tokenize_anything(text: String) -> bool {
    TOKENIZER.pipe(&text);
    true
}

#[test]
fn rules_selectors_can_be_changed() {
    let mut rules = Rules::new(RULES_PATH).unwrap();

    // enabled by default
    assert!(!rules
        .suggest("I can due his homework", &*TOKENIZER)
        .is_empty());

    rules
        .mut_options()
        .selectors
        .push(("confused_words/confusion_due_do".try_into().unwrap(), false));

    // disabled now
    assert!(rules
        .suggest("I can due his homework", &*TOKENIZER)
        .is_empty());

    // disabled by default
    assert!(rules.suggest("I can not go", &*TOKENIZER).is_empty());

    rules
        .mut_options()
        .selectors
        .push(("typos/can_not".try_into().unwrap(), true));

    // enabled now
    assert!(!rules.suggest("I can not go", &*TOKENIZER).is_empty());

    rules.suggest("hello!", &*TOKENIZER);
}
