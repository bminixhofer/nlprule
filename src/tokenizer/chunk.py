import spacy
from spacy.matcher import Matcher
from spacy.symbols import ADP, NOUN, ADJ, PRON

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


def all_children(token):
    out = []

    for child in token.children:
        out += all_children(child)
        out.append(child)

    return out


def chunk(text):
    doc = nlp(text)
    verb_phrases = verb_matcher(doc)

    chunks = []

    for token in doc:
        if (
            token.pos == PRON
            or token.pos == NOUN
            or (
                token.pos == ADJ
                and token.head.pos not in [PRON, NOUN]
                and len(list(token.children)) > 0
            )
        ):
            contained = sorted(all_children(token), key=lambda x: x.idx - token.idx)
            contained.append(token)

            is_plural = any("Number_plur" in token.morph for token in contained)

            chunks.append(
                {
                    "tag": "NP-" + ("plural" if is_plural else "singular"),
                    "range": (contained[0].idx, token.idx + len(token.text)),
                }
            )

    for _, start, end in verb_phrases:
        span = doc[start:end]

        chunks.append({"range": (span.start_char, span.end_char), "tag": "VP"})

    for token in doc:
        if token.pos == ADP:
            chunks.append(
                {"range": (token.idx, token.idx + len(token.text)), "tag": "PP"}
            )

    return chunks


# def chunk(text):
#     doc = nlp(text)
#     verb_phrases = verb_matcher(doc)

#     out = []

#     for noun_chunk in doc.noun_chunks:
#         is_plural = any("Number_plur" in token.morph for token in noun_chunk)

#         out.append(
#             {
#                 "text": noun_chunk.text,
#                 "range": (noun_chunk.start_char, noun_chunk.end_char),
#                 "tag": "NP-" + ("plural" if is_plural else "singular"),
#             }
#         )

#     for _, start, end in verb_phrases:
#         span = doc[start:end]

#         out.append(
#             {"text": span.text, "range": (span.start_char, span.end_char), "tag": "VP"}
#         )

#     for token in doc:
#         if token.pos == ADP:
#             out.append(
#                 {
#                     "text": token.text,
#                     "range": (token.idx, token.idx + len(token.text)),
#                     "tag": "PP",
#                 }
#             )

#     # early exit if theres no matches
#     if len(out) == 0:
#         return []

#     mask = [False] * len(text)

#     def no_overlap(x):
#         if any(mask[slice(*x["range"])]):
#             return False

#         for i in range(*x["range"]):
#             mask[i] = True

#         return True

#     out = sorted(out, key=lambda x: x["range"][0])
#     out = list(filter(no_overlap, out))

#     return out
