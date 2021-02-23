import pytest
import pickle
from nlprule import Tokenizer, Rules


@pytest.fixture()
def tokenizer_and_rules():
    tokenizer = Tokenizer("storage/en_tokenizer.bin")
    rules = Rules("storage/en_rules.bin", tokenizer)
    return tokenizer, rules


def test_correct(tokenizer_and_rules):
    (_, rules) = tokenizer_and_rules

    # just some sample corrections, tests covering all rules are done in rust

    assert (
        rules.correct("He wants that you send him an email.")
        == "He wants you to send him an email."
    )

    assert (
        rules.correct("Thanks for your’s and Lucy’s help.")
        == "Thanks for yours and Lucy’s help."
    )

    # there is a rule for this but it is turned off
    assert rules.correct("I can not go.") == "I can not go."

    assert rules.correct("I can due his homework.") == "I can do his homework."


def test_sentencization_sane(tokenizer_and_rules):
    (tokenizer, _) = tokenizer_and_rules

    sentences = tokenizer.pipe(
        "e.g. U.K. and Mr. do not split. SRX is a rule-based format."
    )
    assert (sentences[0][-2].text, sentences[0][-1].text) == ("split", ".")
    assert (sentences[1][1].text, sentences[1][2].text) == ("SRX", "is")


def test_suggest(tokenizer_and_rules):
    (_, rules) = tokenizer_and_rules

    text = "She was not been here since Monday instead off working."

    suggestions = rules.suggest(text)
    assert len(suggestions) == 2

    assert (suggestions[0].start, suggestions[0].end) == (4, 16)
    assert set(suggestions[0].replacements) == {"was not", "has not been"}

    assert (suggestions[1].start, suggestions[1].end) == (35, 46)
    assert set(suggestions[1].replacements) == {"instead of"}

    assert (
        rules.apply_suggestions(text, suggestions)
        == "She was not here since Monday instead of working."
    )


def test_rules_inspectable(tokenizer_and_rules):
    (_, rules) = tokenizer_and_rules

    suggestion = rules.suggest("He was taken back by my response.")[0]

    rule = rules.select(suggestion.source)[0]
    assert rule.id == suggestion.source

    # metadata of the rule itself
    assert rule.short == "Commonly confused word"
    assert rule.url == "https://www.merriam-webster.com/dictionary/take%20aback"
    assert rule.id == "CONFUSED_WORDS/BACK_ABACK/0"
    assert rule.name == "taken back (aback) by"

    # category related metadata
    assert rule.category_name == "Commonly Confused Words"
    assert rule.category_type == "misspelling"

    # data related to rule examples
    assert len(rule.examples) == 2
    assert rule.examples[0].text == "He was totally taken back by my response."
    assert rule.examples[0].suggestion is not None

    assert (
        rules.apply_suggestions(rule.examples[0].text, [rule.examples[0].suggestion])
        == "He was totally taken aback by my response."
    )

    assert rule.examples[1].text == "He was totally taken a bag by my response."
    assert rule.examples[1].suggestion is not None

    assert (
        rules.apply_suggestions(rule.examples[0].text, [rule.examples[0].suggestion])
        == "He was totally taken aback by my response."
    )


def test_pickle_roundtrip_works(tokenizer_and_rules):
    (tokenizer, rules) = tokenizer_and_rules

    dump = pickle.dumps((tokenizer, rules))
    (tokenizer, rules) = pickle.loads(dump)

    assert len(rules.rules) > 0


def test_invalid_selector_fails(tokenizer_and_rules):
    (tokenizer, rules) = tokenizer_and_rules

    with pytest.raises(ValueError):
        rule = rules.select("GRAMMAR///")

    with pytest.raises(ValueError):
        # index has to be integer
        rule = rules.select("x/y/z")
