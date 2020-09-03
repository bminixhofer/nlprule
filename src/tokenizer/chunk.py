import spacy
from spacy.matcher import Matcher
from spacy.symbols import ADP

nlp = spacy.load("en_core_web_sm", disable=["ner"])

verb_matcher = Matcher(nlp.vocab)
verb_matcher.add(
    "VerbPhrases",
    None,
    [
        {"POS": "VERB", "OP": "*"},
        {"POS": "ADV", "OP": "*"},
        {"POS": "PART", "OP": "*"},
        {"POS": "VERB", "OP": "+"},
        {"POS": "PART", "OP": "*"},
    ],
)


def chunk(text):
    doc = nlp(text)
    verb_phrases = verb_matcher(doc)

    out = []

    for match_id, start, end in verb_phrases:
        span = doc[start:end]

        for i, token in enumerate(span):
            out.append(
                {
                    "text": token.text,
                    "range": (token.idx, token.idx + len(token.text)),
                    "tag": "B-VP" if i == 0 else "I-VP",
                }
            )

    for noun_chunk in doc.noun_chunks:
        is_plural = any("Number_plur" in token.morph for token in noun_chunk)
        num_string = "plural" if is_plural else "singular"

        for i, token in enumerate(noun_chunk):
            tags = []

            if i == 0:
                tags.append(f"B-NP-{num_string}")

            if i == len(noun_chunk) - 1:
                tags.append(f"E-NP-{num_string}")

            if i > 0 and i < len(noun_chunk) - 1:
                tags.append(f"I-NP-{num_string}")

            out.append(
                {
                    "text": token.text,
                    "range": (token.idx, token.idx + len(token.text)),
                    "tag": "|".join(tags),
                }
            )

    for token in doc:
        if token.pos == ADP:
            out.append(
                {
                    "text": token.text,
                    "range": (token.idx, token.idx + len(token.text)),
                    "tag": "B-PP",
                }
            )

    mask = [False] * len(text)

    def no_overlap(x):
        if any(mask[slice(*x["range"])]):
            return False

        for i in range(*x["range"]):
            mask[i] = True

        return True

    out = sorted(filter(no_overlap, out), key=lambda x: x["range"][0])
    return out
