import json
from torch.utils.data import DataLoader
import torch
import torch.nn.functional as F
from transformers import (
    AutoTokenizer,
    AutoModelForTokenClassification,
)
from dataset import CoNLL2000Dataset
from tqdm.auto import tqdm
from sklearn import metrics

model_name = "distilroberta-base"
batch_size = 32
grad_acc_steps = 1
epochs = 5
max_lr = 5e-4
device = torch.device("cuda")

tokenizer = AutoTokenizer.from_pretrained(model_name, use_fast=True)
train_dataset = CoNLL2000Dataset("train", tokenizer, cachedir="data/train")
test_dataset = CoNLL2000Dataset(
    "test", tokenizer, target_vocabs=train_dataset.target_vocabs, cachedir="data/test",
)

vocab_lengths = [max(x.values()) + 1 for x in train_dataset.target_vocabs]
inv_vocabs = []

for vocab in train_dataset.target_vocabs:
    inv_vocabs.append({value: key for key, value in vocab.items()})

model = AutoModelForTokenClassification.from_pretrained(
    model_name, num_labels=sum(vocab_lengths)
).to(device)

# remove some layers
model.base_model.encoder.layer = model.base_model.encoder.layer[:2]

# remove some heads
# TODO: do this smarter after training
model.prune_heads({0: [0, 1, 2, 3, 4, 5], 1: [0, 1, 2, 3, 4, 5]})

train_loader = DataLoader(
    train_dataset,
    collate_fn=train_dataset.collate_fn,
    batch_size=batch_size,
    shuffle=True,
)
test_loader = DataLoader(
    test_dataset,
    collate_fn=test_dataset.collate_fn,
    batch_size=batch_size,
    shuffle=False,
)

optimizer = torch.optim.AdamW(model.parameters(), lr=max_lr)
scheduler = torch.optim.lr_scheduler.OneCycleLR(
    optimizer,
    max_lr=max_lr,
    total_steps=int(len(train_loader) / grad_acc_steps * epochs),
)


def loss_fn(y_pred, y_true):
    prev = 0
    loss = 0.0

    for i, length in enumerate(vocab_lengths):
        loss += F.cross_entropy(
            y_pred[:, :, prev : prev + length].permute(0, 2, 1), y_true[:, :, i]
        )
        prev += length

    return loss


def get_spans(text):
    encoded = tokenizer.encode_plus(text, return_offsets_mapping=True)

    raw_preds = model(torch.tensor(encoded["input_ids"], device=device)[None, :])[0][0]
    preds = []

    for p in raw_preds:
        pred = []
        prev = 0

        for vocab, length in zip(inv_vocabs, vocab_lengths):

            pred.append(vocab[int(torch.argmax(p[prev : prev + length]))])

            prev += length

        preds.append(pred)

    spans = [[], [], []]
    prev = [None, None, None]

    for p, span in zip(preds, encoded["offset_mapping"]):
        # ignore special tokens
        if span[0] == span[1]:
            continue

        for i, x in enumerate(p):
            if x == prev[i]:
                spans[i][-1]["span"][1] = span[1]
            else:
                spans[i].append({"value": x, "span": [span[0], span[1]]})

            prev[i] = x

    return spans


for epoch in tqdm(range(epochs), desc="Epoch"):
    epoch_train_loss = 0
    model.train()
    model.zero_grad()

    for step, batch in enumerate(tqdm(train_loader, desc="Train")):
        input_ids = batch[0].to(device)
        attention_mask = batch[1].to(device)
        labels = batch[2].to(device)

        inputs = {
            "input_ids": input_ids,
            "attention_mask": attention_mask,
        }

        outputs = model(**inputs)

        loss = loss_fn(outputs[0], labels)
        loss = loss / grad_acc_steps

        loss.backward()

        if (step + 1) % grad_acc_steps == 0:
            torch.nn.utils.clip_grad_norm_(model.parameters(), 1.0)
            optimizer.step()
            scheduler.step()

            model.zero_grad()

    model.eval()

    test_preds = [[], [], []]
    test_labels = [[], [], []]

    for step, batch in enumerate(tqdm(test_loader, desc="Valid")):
        input_ids = batch[0].to(device)
        attention_mask = batch[1].to(device)
        labels = batch[2].to(device)

        inputs = {
            "input_ids": input_ids,
            "attention_mask": attention_mask,
        }

        outputs = model(**inputs)

        labels = labels[attention_mask > 0]
        out = outputs[0][attention_mask > 0]

        prev = 0

        for i, length in enumerate(vocab_lengths):
            x = torch.argmax(out[:, prev : prev + length], 1)

            test_preds[i] += x.tolist()
            test_labels[i] += labels[:, i].tolist()

            prev += length

    for i, name in enumerate(["POS", "Chunk", "Plural/Singular"]):
        f1 = metrics.f1_score(test_labels[i], test_preds[i], average="micro")
        recall = metrics.recall_score(test_labels[i], test_preds[i], average="micro")
        precision = metrics.precision_score(
            test_labels[i], test_preds[i], average="micro"
        )

        print(name)
        print(f"F1 Score: {f1:.4f}",)
        print(f"Recall: {recall:.4f}",)
        print(f"Precision: {precision:.4f}",)
        print()

input_ids = torch.tensor(input_ids, dtype=torch.long, device=device)
attention_mask = torch.tensor(attention_mask, dtype=torch.long, device=device)

torch.onnx.export(
    model,
    (input_ids, attention_mask),
    "model.onnx",
    input_names=["input_ids", "attention_mask"],
    output_names=["output"],
    opset_version=11,  # for onnxruntime
    dynamic_axes={
        "input_ids": {0: "batch", 1: "seq"},
        "attention_mask": {0: "batch", 1: "seq"},
        "output": {0: "batch", 1: "seq"},
    },
)

json.dump(inv_vocabs, open("vocab.json", "w"), indent=4)
