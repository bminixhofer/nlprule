import os
import copy
import spacy
from torch.utils import data
from torch.nn.utils.rnn import pad_sequence
import torch
from pathlib import Path
from tqdm.auto import tqdm
from flair.datasets import CONLL_2000

nlp = spacy.load("en_core_web_sm", disable=["ner"])


def load(kind, max_samples=None):
    corpus = CONLL_2000()

    if kind == "train":
        dataset = data.ConcatDataset([corpus.train, corpus.dev])
    elif kind == "test":
        dataset = corpus.test

    texts = []
    chunk_spans = []
    sing_plur_spans = []

    for sentence in tqdm(dataset):
        sentence.infer_space_after()

        text = sentence.to_plain_string()

        sing_plur_label = []
        chunk_label = []

        # detect singular / plural with SpaCy
        for token in nlp(text):
            sing_plur = None

            if "Number_plur" in token.morph:
                sing_plur = "plural"
            elif "Number_sing" in token.morph:
                sing_plur = "singular"

            if sing_plur is not None:
                sing_plur_label.append(
                    {
                        "span": (token.idx, token.idx + len(token.text)),
                        "sing_plur": sing_plur,
                    }
                )

        inc_text = ""
        for token in sentence.tokens:
            span = (len(inc_text), len(inc_text) + len(token.text))
            inc_text += token.text + (" " if token.whitespace_after else "")

            chunk_label.append(
                {
                    "span": span,
                    "pos": token.annotation_layers["pos"][0].value,
                    "chunk": token.annotation_layers["np"][0].value,
                }
            )

        texts.append(text)
        chunk_spans.append(chunk_label)
        sing_plur_spans.append(sing_plur_label)

    return texts, chunk_spans, sing_plur_spans


def divide_spans(offset_mapping, spans, labels, default=None):
    out = []

    for (start, end) in offset_mapping:
        current = default

        for i, (span_start, span_end) in enumerate(spans):
            if start != end and start >= span_start and end <= span_end:
                current = labels[i]
                break

        out.append(current)

    return out


def encode(inp, dct):
    out = []

    for x in inp:
        if x not in dct:
            dct[x] = len(dct) + 2  # leave 1 for padding

        out.append(dct[x])

    return out


def proc(texts, chunk_spans, sing_plur_spans, tokenizer, target_vocabs):
    input_ids = []
    attention_masks = []
    targets = []

    for text, chunk_span, sing_plur_span in tqdm(
        zip(texts, chunk_spans, sing_plur_spans)
    ):
        encoded = tokenizer.encode_plus(
            text, return_offsets_mapping=True, add_special_tokens=True
        )

        input_ids.append(torch.tensor(encoded["input_ids"]))
        attention_masks.append(torch.tensor(encoded["attention_mask"]))

        targets.append(
            torch.tensor(
                [
                    encode(
                        divide_spans(
                            encoded["offset_mapping"],
                            [x["span"] for x in chunk_span],
                            [x["pos"] for x in chunk_span],
                            default="O",
                        ),
                        target_vocabs[0],
                    ),
                    encode(
                        divide_spans(
                            encoded["offset_mapping"],
                            [x["span"] for x in chunk_span],
                            [x["chunk"] for x in chunk_span],
                            default="O",
                        ),
                        target_vocabs[1],
                    ),
                    encode(
                        divide_spans(
                            encoded["offset_mapping"],
                            [x["span"] for x in sing_plur_span],
                            [x["sing_plur"] for x in sing_plur_span],
                            default="O",
                        ),
                        target_vocabs[2],
                    ),
                ]
            ).T
        )

    return (input_ids, attention_masks, targets)


class CoNLL2000Dataset(data.Dataset):
    def __init__(
        self,
        kind,
        tokenizer,
        max_samples=None,
        target_vocabs=None,
        max_length=512,
        cachedir=None,
    ):
        assert tokenizer.pad_token_id == 1  # simplifies padding the target
        self.tokenizer = tokenizer

        if cachedir is not None and os.path.exists(cachedir):
            cachedir = Path(cachedir)

            self.target_vocabs = torch.load(cachedir / "target_vocabs.pth")
            self.input_ids, self.attention_mask, self.targets = torch.load(
                cachedir / "data.pth"
            )
        else:
            texts, chunk_spans, sing_plur_spans = load(kind, max_samples=max_samples)
            if target_vocabs is None:
                self.target_vocabs = [{}, {}, {}]
            else:
                self.target_vocabs = copy.deepcopy(target_vocabs)

            self.input_ids, self.attention_mask, self.targets = proc(
                texts, chunk_spans, sing_plur_spans, tokenizer, self.target_vocabs
            )

            if cachedir is not None:
                cachedir = Path(cachedir)
                cachedir.mkdir(exist_ok=True, parents=True)

                torch.save(self.target_vocabs, cachedir / "target_vocabs.pth")

                torch.save(
                    [self.input_ids, self.attention_mask, self.targets],
                    cachedir / "data.pth",
                )

    def collate_fn(self, batch):
        batch = list(zip(*batch))

        out = [
            pad_sequence(x, batch_first=True, padding_value=self.tokenizer.pad_token_id)
            if len(x[0].shape) > 0
            else torch.stack(x, 0)
            for x in batch
        ]

        return out

    def __len__(self):
        return len(self.input_ids)

    def __getitem__(self, idx):
        output = [
            self.input_ids[idx],
            self.attention_mask[idx],
            self.targets[idx],
        ]

        return output
