use std::{convert::TryInto, sync::Arc};

use lazy_static::lazy_static;
use nlprule::{rule::id::Category, Error, Rules, Tokenizer};
use quickcheck_macros::quickcheck;

const TOKENIZER_PATH: &str = "../storage/en_tokenizer.bin";
const RULES_PATH: &str = "../storage/en_rules.bin";

lazy_static! {
    static ref TOKENIZER: Arc<Tokenizer> = Arc::new(Tokenizer::new(TOKENIZER_PATH).unwrap());
    static ref RULES: Rules = Rules::new(RULES_PATH, TOKENIZER.clone()).unwrap();
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
fn rules_can_be_disabled_enabled() {
    let mut rules = Rules::new(RULES_PATH, TOKENIZER.clone()).unwrap();

    // enabled by default
    assert!(!rules.suggest("I can due his homework").is_empty());

    rules
        .select_mut(
            &Category::new("confused_words")
                .join("confusion_due_do")
                .into(),
        )
        .for_each(|x| x.disable());

    // disabled now
    assert!(rules.suggest("I can due his homework").is_empty());

    // disabled by default
    assert!(rules.suggest("I can not go").is_empty());

    rules
        .select_mut(&"typos/can_not".try_into().unwrap())
        .for_each(|x| x.enable());

    // enabled now
    assert!(!rules.suggest("I can not go").is_empty());
}

#[test]
fn spellchecker_works() -> Result<(), Error> {
    let mut rules = Rules::new(RULES_PATH, TOKENIZER.clone()).unwrap();
    rules.spell_mut().options_mut().variant = Some(rules.spell().variant("en_GB")?);

    Ok(())
}
