#!/usr/bin/env python3
"""Compares nlprule's output against the official LanguageTool HTTP server.

Sends the embedded grammar examples of each language to both engines and
reports rule-ID agreement per sentence.

Usage:
    python3 compare_server.py --tokenizer ../storage/es_tokenizer.bin \
        --rules ../storage/es_rules.bin --lang es --grammar ../data2/es/grammar.xml \
        [--port 8081] [--limit 100]

The LanguageTool server must be running (start it with:
  java -cp languagetool-server.jar org.languagetool.server.HTTPServer --port 8081
from the LanguageTool distribution).
"""

import argparse
import json
import re
import subprocess
import urllib.parse
import urllib.request
import xml.etree.ElementTree as ET


def examples_from_grammar(path, limit):
    """Incorrect example sentences (marker stripped), deduplicated."""
    out = []
    seen = set()
    try:
        root = ET.parse(path).getroot()
    except ET.ParseError:
        return out
    for example in root.iter("example"):
        text = None
        for marker in example.iter("marker"):
            text = "".join(marker.itertext())
            break
        if text is None:
            continue
        text = text.strip()
        if text and text not in seen:
            seen.add(text)
            out.append(text)
        if len(out) >= limit:
            break
    return out


def lt_matches(port, lang, text):
    data = urllib.parse.urlencode(
        {
            "text": text,
            "language": lang,
            "enabledOnly": "false",
        }
    ).encode()
    req = urllib.request.Request(
        f"http://localhost:{port}/v2/check", data=data, method="POST"
    )
    with urllib.request.urlopen(req, timeout=60) as resp:
        body = json.load(resp)
    return [(m["rule"]["id"], m["offset"], m["length"]) for m in body.get("matches", [])]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--tokenizer", required=True)
    ap.add_argument("--rules", required=True)
    ap.add_argument("--lang", required=True)
    ap.add_argument("--grammar", required=True)
    ap.add_argument("--port", type=int, default=8081)
    ap.add_argument("--limit", type=int, default=100)
    ap.add_argument("--bin", default="./target/release/check_server")
    args = ap.parse_args()

    sentences = examples_from_grammar(args.grammar, args.limit)
    if not sentences:
        print(json.dumps({"lang": args.lang, "sentences": 0}))
        return

    proc = subprocess.run(
        [args.bin, args.tokenizer, args.rules, args.lang],
        input="\n".join(sentences),
        capture_output=True,
        text=True,
        check=True,
    )
    ours = {}
    for line in proc.stdout.splitlines():
        obj = json.loads(line)
        ours[obj["i"]] = set(h[0] for h in obj["hits"])

    both = only_us = only_lt = 0
    per_sentence_agreement = []
    for i, text in enumerate(sentences):
        try:
            lt = set(m[0] for m in lt_matches(args.port, args.lang, text))
        except Exception as e:
            print(f"LT error on {i}: {e}", file=__import__("sys").stderr)
            lt = set()
        us = ours.get(i, set())
        both += len(us & lt)
        only_us += len(us - lt)
        only_lt += len(lt - us)
        union = us | lt
        per_sentence_agreement.append(
            len(us & lt) / len(union) if union else 1.0
        )

    total = both + only_us + only_lt
    precision = both / (both + only_us) if both + only_us else 1.0
    recall = both / (both + only_lt) if both + only_lt else 1.0
    print(
        json.dumps(
            {
                "lang": args.lang,
                "sentences": len(sentences),
                "matches_total": total,
                "agreed": both,
                "only_nlprule": only_us,
                "only_lt": only_lt,
                "id_precision_vs_lt": round(precision, 4),
                "id_recall_vs_lt": round(recall, 4),
                "jaccard": round(
                    sum(per_sentence_agreement) / len(per_sentence_agreement), 4
                ),
            }
        )
    )


if __name__ == "__main__":
    main()
