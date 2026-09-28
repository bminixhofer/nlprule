#!/usr/bin/env python3
"""Aggregate which rule IDs the LT server fires but nlprule does not (per lang)."""
import json, subprocess, sys, urllib.parse, urllib.request, collections
import xml.etree.ElementTree as ET

def examples_from_grammar(path, limit):
    out, seen = [], set()
    for example in ET.parse(path).getroot().iter("example"):
        text = None
        for marker in example.iter("marker"):
            text = "".join(marker.itertext()); break
        if text is None: continue
        text = text.strip()
        if text and text not in seen:
            seen.add(text); out.append(text)
        if len(out) >= limit: break
    return out

def lt_matches(port, lang, text):
    data = urllib.parse.urlencode({"text": text, "language": lang, "enabledOnly": "false"}).encode()
    req = urllib.request.Request(f"http://localhost:{port}/v2/check", data=data, method="POST")
    with urllib.request.urlopen(req, timeout=120) as resp:
        body = json.load(resp)
    return [m["rule"]["id"] for m in body.get("matches", [])]

lang, grammar, tok, rules, port, limit = sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4], int(sys.argv[5]), int(sys.argv[6])
sentences = examples_from_grammar(grammar, limit)
proc = subprocess.run(["./target/release/check_server", tok, rules, lang], input="\n".join(sentences), capture_output=True, text=True, check=True)
ours = {}
for line in proc.stdout.splitlines():
    obj = json.loads(line); ours[obj["i"]] = set(h[0] for h in obj["hits"])

only_lt = collections.Counter(); only_us = collections.Counter()
for i, text in enumerate(sentences):
    try: lt = set(lt_matches(port, lang, text))
    except Exception: lt = set()
    us = ours.get(i, set())
    only_lt.update(lt - us); only_us.update(us - lt)
print(json.dumps({"lang": lang, "sentences": len(sentences),
                  "top_only_lt": only_lt.most_common(15),
                  "top_only_nlprule": only_us.most_common(8)}))
