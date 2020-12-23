import spacy
from spacy.matcher import Matcher
from spacy.symbols import ADP, DET, NOUN

nlp = spacy.load("en_core_web_sm", disable=["ner"])

verb_matcher = Matcher(nlp.vocab)
verb_matcher.add(
    "VerbPhrases",
    None,
    [
        {"POS": "AUX", "OP": "*"},
        {"POS": "VERB", "OP": "*"},
        {"POS": "ADV", "OP": "*"},
        {"POS": "PART", "OP": "*"},
        {"POS": "AUX", "OP": "*"},
        {"POS": "VERB", "OP": "+"},
        {"POS": "PART", "OP": "*"},
    ],
)


def chunk(text):
    doc = nlp(text)
    verb_phrases = verb_matcher(doc)

    out = []

    for noun_chunk in doc.noun_chunks:
        is_plural = any("Number_plur" in token.morph for token in noun_chunk)
        num_string = "plural" if is_plural else "singular"

        out.append(
            {
                "text": str(noun_chunk),
                "range": (
                    noun_chunk[0].idx,
                    noun_chunk[-1].idx + len(noun_chunk[-1].text),
                ),
                "tag": f"NP-{num_string}",
            }
        )

    for token in doc:
        if token.pos in {NOUN, DET}:
            is_plural = "Number_plur" in token.morph
            num_string = "plural" if is_plural else "singular"

            out.append(
                {
                    "text": token.text,
                    "range": (token.idx, token.idx + len(token.text)),
                    "tag": f"NP-{num_string}",
                }
            )

    for match_id, start, end in verb_phrases:
        span = doc[start:end]

        out.append(
            {
                "text": str(span),
                "range": (span[0].idx, span[-1].idx + len(span[-1].text)),
                "tag": "VP",
            }
        )

    for token in doc:
        if token.pos == ADP:
            out.append(
                {
                    "text": token.text,
                    "range": (token.idx, token.idx + len(token.text)),
                    "tag": "PP",
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
