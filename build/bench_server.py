#!/usr/bin/env python3
"""Performance benchmark: nlprule vs the official LanguageTool HTTP server.

Runs the same sentences (each language's embedded grammar examples) through
both engines and reports load time, per-sentence latency and throughput.

  * nlprule: in-process, single thread, via the `bench` binary
  * LanguageTool: sequential POST /v2/check against the local HTTPServer
    (includes HTTP + JVM overhead, as a real client would see it)

Usage:
    python3 bench_server.py --lang en --grammar ../data2/en/grammar.xml \
        --tokenizer ../storage/en_tokenizer.bin --rules ../storage/en_rules.bin \
        [--port 8081] [--limit 200] [--repeats 3]

The LanguageTool server must already be running.
"""

import argparse
import json
import subprocess
import time
import urllib.parse
import urllib.request
import statistics
import xml.etree.ElementTree as ET


def examples_from_grammar(path, limit):
    out, seen = [], set()
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


def lt_check(port, lang, text):
    data = urllib.parse.urlencode(
        {"text": text, "language": lang, "enabledOnly": "false"}
    ).encode()
    req = urllib.request.Request(
        f"http://localhost:{port}/v2/check", data=data, method="POST"
    )
    with urllib.request.urlopen(req, timeout=120) as resp:
        return json.load(resp)


def stats(values_ms):
    values = sorted(values_ms)
    if not values:
        return {}
    q = lambda p: values[min(len(values) - 1, round((len(values) - 1) * p))]
    return {
        "mean_ms": round(statistics.mean(values), 1),
        "p50_ms": round(q(0.5), 1),
        "p95_ms": round(q(0.95), 1),
        "wall_s": round(sum(values) / 1000.0, 2),
        "throughput_sps": round(len(values) / (sum(values) / 1000.0), 1) if sum(values) else 0,
    }


def jvm_rss_mb():
    import glob
    total = 0
    for statm in glob.glob("/proc/*/statm"):
        try:
            pid = statm.split("/")[2]
            with open(f"/proc/{pid}/cmdline", "rb") as f:
                cmd = f.read().decode(errors="replace")
            if "HTTPServer" in cmd or "languagetool-server" in cmd:
                with open(statm) as f:
                    total = int(f.read().split()[1]) * 4096 / 1024 / 1024
        except (OSError, IndexError, ValueError):
            continue
    return round(total, 0)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--tokenizer", required=True)
    ap.add_argument("--rules", required=True)
    ap.add_argument("--lang", required=True)
    ap.add_argument("--grammar", required=True)
    ap.add_argument("--port", type=int, default=8081)
    ap.add_argument("--limit", type=int, default=200)
    ap.add_argument("--repeats", type=int, default=3)
    ap.add_argument("--bin", default="./target/release/bench")
    ap.add_argument("--lt-repeats", type=int, default=1)
    args = ap.parse_args()

    sentences = examples_from_grammar(args.grammar, args.limit)
    if not sentences:
        print(json.dumps({"lang": args.lang, "sentences": 0}))
        return

    # ---- nlprule ----
    proc = subprocess.run(
        [args.bin, args.tokenizer, args.rules, str(args.repeats)],
        input="\n".join(sentences),
        capture_output=True,
        text=True,
        check=True,
    )
    ours = json.loads(proc.stdout.strip().splitlines()[-1])

    # ---- LanguageTool HTTP server ----
    lt = lt_check(args.port, args.lang, sentences[0])  # warmup
    del lt
    lat = []
    lt_matches = 0
    for _ in range(max(1, args.lt_repeats)):
        for text in sentences:
            t0 = time.monotonic()
            body = lt_check(args.port, args.lang, text)
            lat.append((time.monotonic() - t0) * 1000.0)
            lt_matches += len(body.get("matches", []))

    print(
        json.dumps(
            {
                "lang": args.lang,
                "sentences": len(sentences),
                "nlprule": {
                    "load_ms": ours["load_ms"],
                    "load_rss_mb": ours["load_rss_mb"],
                    "rss_mb": ours["rss_mb"],
                    "repeats": args.repeats,
                    **{
                        k: ours[k]
                        for k in ("mean_ms", "p50_ms", "p95_ms", "throughput_sps")
                    },
                    "matches": ours["matches"],
                },
                "languagetool_server": {
                    "rss_mb": jvm_rss_mb(),
                    "repeats": max(1, args.lt_repeats),
                    **stats(lat),
                    "matches": lt_matches,
                },
            }
        )
    )


if __name__ == "__main__":
    main()
