#!/usr/bin/env python3
"""HTTP front-end for nlprule's `check_server` binary, speaking the
LanguageTool HTTP API so the LanguageTool browser extension works against
it unchanged.

LanguageTool-compatible endpoints:
    POST /v2/check    form-encoded (or JSON) params:
                        text, language (e.g. "es-ES", "auto"),
                        disabledRules, enabledRules, enabledOnly,
                        disabledCategories, enabledCategories
                      -> LT-shaped JSON (matches with rule/category info,
                         character offsets into the original text)
    GET  /v2/languages LT language-list shape

Native endpoints:
    POST /check       JSON {"text": "...", "lang": "es"} -> raw matches
    GET  /languages   served languages
    GET  /health      readiness (does not force child startup)

Each language gets one long-lived `check_server` child (NLPRULE_FULL_ID=1 so
the full CATEGORY/GROUP/N id is emitted). Children start lazily: rule-binary
loading takes ~1 s (en) to ~15 s (es). Newlines are exchanged for spaces
1:1 before checking, keeping offsets aligned with the original text;
byte offsets from the checker are converted to character offsets, which is
what the LT API (and the extension) expects.

Fields LanguageTool has but nlprule does not (rule message, description,
issueType, sentence segmentation) are approximated: see `_fake_lt_fields`.

Environment:
    NLPRULE_LANGS     comma-separated language codes (default: en,es)
    NLPRULE_STORAGE   where the *_tokenizer.bin / *_rules.bin live (/storage)
    NLPRULE_HTTP_PORT listen port (default 8080)
"""

import bisect
import email
import email.policy
import json
import os
import re
import select
import subprocess
import sys
import threading
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import parse_qs, urlsplit

STORAGE = os.environ.get("NLPRULE_STORAGE", "/storage")
LANGS = [x.strip() for x in os.environ.get("NLPRULE_LANGS", "en,es").split(",") if x.strip()]
PORT = int(os.environ.get("NLPRULE_HTTP_PORT", "8080"))
VERSION = os.environ.get("NLPRULE_VERSION", "0.6.4-fork")
COLD_TIMEOUT = 180.0  # first request must also cover rule-binary loading
WARM_TIMEOUT = 60.0

LANG_NAMES = {"en": "English", "es": "Spanish", "ca": "Catalan"}

# LT uses long codes ("ca-ES") in /v2/languages and in the check response;
# the extension dropdown works with those, so echo them back
LANG_LONG = {"en": "en-US", "es": "es-ES", "ca": "ca-ES"}
LANG_VARIANTS = {
    "en": ["en-US", "en-GB", "en-AU", "en-CA", "en-ZA", "en-NZ"],
    "es": ["es-ES", "es-419", "es-AR", "es-MX"],
    "ca": ["ca-ES", "ca-ES-valencia"],
}

# very small stopword models for language=auto — only needs to pick between
# the languages this instance actually serves
ES_MARKERS = frozenset(
    "el la los las de que y en un una es por con para como pero más muy porque "
    "cuando está están soy eres somos sois fue fueron sea sido tiene tengo "
    "hacer no si tú él ella ellos nosotros yo me te se lo le les al del "
    "hola gracias dónde cómo qué quién buenos buenas día días dia tardes "
    "noches usted ustedes aquí también ahora entonces siempre nunca".split()
)
EN_MARKERS = frozenset(
    "the a an of to in and is are was were be been being have has had do does "
    "did for with on at by from this that these those it its i you he she we "
    "they my your his her our their not no yes hello thanks where how what who".split()
)
CA_MARKERS = frozenset(
    "els les de que i en un una és per amb com però perquè més molt també "
    "això allò aquell aquells sóc ets som són està estan ha han tingut "
    "puc vull sé mai on què qui ara després dintre encara mateix tots totes "
    "hola gràcies quan".split()
)
# accents unique to each language tip the scale beyond stopwords
LANG_ACCENTS = {"es": "ñ¿¡", "ca": "àèò"}


def detect_language(text, preferred=()):
    """Pick between the served languages. `preferred` carries the bases of
    the client's preferredVariants (LT semantics: tie-breaker for auto
    detection), so the extension's language preferences are honoured."""
    words = re.findall(r"[a-záéíóúüñçàèò]+", text.lower())
    scores = {}
    for lang in LANGS:
        markers = {"es": ES_MARKERS, "en": EN_MARKERS, "ca": CA_MARKERS}.get(lang, ())
        scores[lang] = sum(1 for w in words if w in markers)
        scores[lang] += 2 * sum(1 for c in LANG_ACCENTS.get(lang, "") if c in text)
        if lang in preferred:
            # preferredVariants: tie-breaker, earlier entries win
            scores[lang] += len(preferred) - preferred.index(lang)
    if not scores or max(scores.values()) == 0:
        # no signal at all: fall back to Spanish or the first preferred
        for p in preferred:
            if p in LANGS:
                return p
        return "es"
    return max(scores, key=scores.get)


def normalize_lang(lang, text, preferred=()):
    """Map an LT language code ("es-ES", "en-GB", "auto", "de-DE") to a
    served language, or None if impossible."""
    lang = (lang or "").strip()
    if not lang or lang == "auto":
        return detect_language(text, preferred)
    if lang in LANGS:
        return lang
    base = lang.split("-")[0]
    return base if base in LANGS else None


class LangWorker:
    """One persistent check_server child per (language, level), guarded by a lock."""

    def __init__(self, lang, picky=False):
        self.lang = lang
        self.picky = picky
        self.lock = threading.Lock()
        self.proc = None
        self.next_i = 0

    def _spawn(self):
        tok = os.path.join(STORAGE, f"{self.lang}_tokenizer.bin")
        rules = os.path.join(STORAGE, f"{self.lang}_rules.bin")
        env = dict(os.environ, NLPRULE_FULL_ID="1")
        if self.picky:
            env["NLPRULE_PICKY"] = "1"
        self.proc = subprocess.Popen(
            ["check_server", tok, rules, self.lang],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            bufsize=1,
            text=True,
            env=env,
        )
        self.next_i = 0

    def check(self, text):
        """Return raw hits [(full_rule_id, byte_start, byte_end, replacement)].

        Newlines (incl. CRLF) are swapped 1:1 for spaces: the checker is
        line-based and this keeps every offset aligned with the original.
        """
        flat = (
            text.replace("\r\n", "  ").replace("\n", " ").replace("\r", " ")
        )
        if not flat.strip():
            return []
        with self.lock:
            for _attempt in (0, 1):
                if self.proc is None or self.proc.poll() is not None:
                    self._spawn()
                i = self.next_i
                self.next_i += 1
                try:
                    self.proc.stdin.write(flat + "\n")
                    self.proc.stdin.flush()
                except (BrokenPipeError, ValueError):
                    self.proc = None
                    continue
                timeout = COLD_TIMEOUT if i == 0 else WARM_TIMEOUT
                ready, _, _ = select.select([self.proc.stdout], [], [], timeout)
                if not ready:
                    self.proc.kill()
                    self.proc = None
                    raise TimeoutError(f"check_server ({self.lang}) timed out")
                line = self.proc.stdout.readline()
                if not line:
                    self.proc = None
                    continue
                obj = json.loads(line)
                assert obj["i"] == i, f"desynchronized reply: {obj['i']} != {i}"
                return [tuple(h) for h in obj["hits"]]
            raise RuntimeError(f"check_server ({self.lang}) died twice in a row")

    def loaded(self):
        return self.proc is not None and self.proc.poll() is None


WORKERS = {(lang, picky): LangWorker(lang, picky) for lang in LANGS for picky in (False, True)}


def offset_tables(text):
    """Per-code-point offset tables: byte_offsets[i] = utf-8 byte offset of
    char i, u16_offsets[i] = utf-16 code-unit offset of char i (both with a
    full-length sentinel entry). Used to turn the checker's byte offsets
    into the UTF-16 code-unit offsets the LT API (Java/JS) uses."""
    bo, uo = [], []
    b = u = 0
    for ch in text:
        bo.append(b)
        uo.append(u)
        b += len(ch.encode("utf-8"))
        u += 2 if ord(ch) > 0xFFFF else 1
    bo.append(b)
    uo.append(u)
    return bo, uo


def byte_to_char_offsets(text):
    """kept for callers that want character (code-point) offsets."""
    return offset_tables(text)[0]


def split_rule_id(full_id):
    """CATEGORY/GROUP/N -> (rule_id, category_id); plain ids (builtins) have
    no category — spellchecker rules map to TYPOS like LanguageTool does."""
    parts = full_id.split("/")
    if len(parts) >= 2:
        return parts[1], parts[0].upper()
    if full_id.startswith("MORFOLOGIK") or full_id.startswith("HUNSPELL"):
        return full_id, "TYPOS"
    return full_id, "MISC"


CATEGORY_NAMES = {
    "TYPOS": "Possible Typo",
    "GRAMMAR": "Grammar",
    "MISC": "Miscellaneous",
    "REDUNDANCY": "Redundant Phrases",
    "STYLE": "Style",
    "CASING": "Capitalization",
    "PUNCTUATION": "Punctuation",
    "TYPOGRAPHY": "Typography",
    "COLLOQUIALISMS": "Colloquialisms",
    "CONFUSED_WORDS": "Confused Words",
}

ISSUE_TYPES = {
    "TYPOS": "misspelling",
    "CASING": "casing",
    "PUNCTUATION": "typographical",
    "TYPOGRAPHY": "typographical",
    "STYLE": "style",
    "REDUNDANCY": "redundancy",
    "COLLOQUIALISMS": "colloquialisms",
    "CONFUSED_WORDS": "inconsistency",
}


def _fake_lt_fields(rule_id, category_id, replacement):
    """nlprule hits carry no human-facing text; synthesize message,
    description and issueType so extension tooltips render sensibly."""
    description = rule_id.replace("_", " ").strip().capitalize()
    if replacement:
        message = f'Did you mean: "{replacement}"?'
        short = replacement
    else:
        message = f"Possible error ({rule_id})"
        short = rule_id
    return message, short, description


CONTEXT_SIZE = 40  # LT ContextTools default window (± units around the match)


def sentence_span(text, start, end):
    """Sentence-snapped [left, right) span around a match (code-point idx),
    used for the display-only `sentence` field."""
    left = start
    for m in re.finditer(r"[.!?]+[\s]|[.!?]+$", text[:start]):
        left = max(left, m.end())
    right = end
    m = re.search(r"[\s][.!?]+$", text[end:])
    if m:
        right = end + m.start()
    return left, right


def context_of(text_u16, start, end, u16_len):
    """LT 6.6 ContextTools.getContext semantics, in UTF-16 code units:
    a ±CONTEXT_SIZE window around the match with '\n' replaced by ' ' and
    a '...' prefix/postfix exactly when the window extends past an edge
    (clipped at <0 / >len — equality keeps the ellipsis, like Java)."""
    start_content = start - CONTEXT_SIZE
    prefix = "..."
    if start_content < 0:
        prefix = ""
        start_content = 0
    end_content = end + CONTEXT_SIZE
    postfix = "..."
    if end_content > u16_len:
        postfix = ""
        end_content = u16_len
    window = text_u16[start_content * 2 : end_content * 2].decode("utf-16-le")
    return {
        "text": prefix + window.replace("\n", " ") + postfix,
        "offset": len(prefix) + (start - start_content),
        "length": end - start,
    }


def to_lt_matches(text, raw_hits):
    byte_offs, u16_offs = offset_tables(text)
    text_u16 = text.encode("utf-16-le")
    u16_len = len(text_u16) // 2
    matches = []
    for full_id, bstart, bend, repls in raw_hits:
        rule_id, cat_id = split_rule_id(full_id)
        start = bisect.bisect_left(byte_offs, bstart)
        end = bisect.bisect_left(byte_offs, bend)
        if end <= start:
            continue
        start_u16 = u16_offs[start]
        end_u16 = u16_offs[end]
        repls = list(repls)
        message, short, description = _fake_lt_fields(rule_id, cat_id, repls[0] if repls else "")
        replacements = [{"value": r, "shortValue": r} for r in repls]
        sleft, sright = sentence_span(text, start, end)
        matches.append(
            {
                "message": message,
                "shortMessage": short,
                "replacements": replacements,
                "offset": start_u16,
                "length": end_u16 - start_u16,
                "context": context_of(text_u16, start_u16, end_u16, u16_len),
                "sentence": text[sleft:sright],
                "type": {"typeName": "Other"},
                "rule": {
                    "id": rule_id,
                    "description": description,
                    "issueType": ISSUE_TYPES.get(cat_id, "grammar"),
                    "category": {
                        "id": cat_id,
                        "name": CATEGORY_NAMES.get(cat_id, cat_id.replace("_", " ").title()),
                    },
                },
                "ignoreTopInsets": False,
                "contextForSureMatch": 0,
            }
        )
    return matches


def _csv(params, key):
    v = params.get(key, [""])[0]
    # LT uses comma-separated lists; the extension also sends semicolons
    return {x for x in re.split(r"[,;]", v) if x}


class Handler(BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def _send(self, code, payload):
        body = json.dumps(payload, ensure_ascii=False).encode("utf-8")
        self.send_response(code)
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Content-Length", str(len(body)))
        self.send_header("Access-Control-Allow-Origin", "*")
        self.end_headers()
        self.wfile.write(body)

    # -- param plumbing ---------------------------------------------------

    def _params(self):
        """Merge query string and body (form-encoded, multipart or JSON)."""
        params = {}
        qs = urlsplit(self.path).query
        if qs:
            params.update(parse_qs(qs, keep_blank_values=True))
        length = int(self.headers.get("Content-Length", "0") or 0)
        if not length:
            return params
        body = self.rfile.read(length)
        ctype = (self.headers.get("Content-Type") or "").lower()
        if "json" in ctype:
            try:
                obj = json.loads(body or b"{}")
                for k, v in obj.items():
                    params[k] = [str(v)]
            except json.JSONDecodeError:
                pass
        elif "multipart/form-data" in ctype:
            # the LT browser extension posts multipart/form-data (fetch
            # FormData); parse it with the stdlib email parser
            msg = email.message_from_bytes(
                b"Content-Type: "
                + self.headers.get("Content-Type", "").encode("latin-1", "replace")
                + b"\r\n\r\n"
                + body
            )
            for part in msg.get_payload() if msg.is_multipart() else []:
                name = part.get_param("name", header="content-disposition")
                if name:
                    value = part.get_payload(decode=True) or b""
                    params[name] = [value.decode("utf-8", "replace")]
        else:
            params.update(parse_qs(body.decode("utf-8", "replace"), keep_blank_values=True))
        return params

    # -- handlers ---------------------------------------------------------

    def do_GET(self):
        path = urlsplit(self.path).path
        if path in ("/v2/languages", "/languages"):
            langs = []
            for l in LANGS:
                for variant in LANG_VARIANTS.get(l, [LANG_LONG.get(l, l)]):
                    langs.append(
                        {
                            "name": LANG_NAMES.get(l, l),
                            "code": l,
                            "longCode": variant,
                            "nativeName": LANG_NAMES.get(l, l),
                        }
                    )
            self._send(200, langs)
        elif path == "/health":
            self._send(
                200,
                {"status": "ok", "loaded": [l for (l, _p), w in WORKERS.items() if w.loaded()]},
            )
        else:
            self._send(404, {"error": "not found"})

    def do_POST(self):
        path = urlsplit(self.path).path
        if path not in ("/v2/check", "/check"):
            self._send(404, {"error": "not found"})
            return
        params = self._params()
        text = params.get("text", [""])[0]
        if not text and "data" in params:
            # the LT browser extension sends the content as the `data` param.
            # Observed shapes: {"text": "..."} (v10 extension) and the
            # annotation array {"annotation": [{"text": ...}, {"markup":
            # "<b>"}]} (LT HTTP API docs); offsets refer to the concatenated
            # text parts in the annotation case.
            raw_data = params["data"][0]
            try:
                ann = json.loads(raw_data)
                if isinstance(ann, str):  # double-encoded JSON
                    ann = json.loads(ann)
                if isinstance(ann, dict) and isinstance(ann.get("text"), str):
                    text = ann["text"]
                else:
                    segments = (
                        ann
                        if isinstance(ann, list)
                        else ann.get("annotation", []) if isinstance(ann, dict) else []
                    )
                    text = "".join(
                        p.get("text", "") for p in segments if isinstance(p, dict)
                    )
                if not text:
                    sys.stderr.write(
                        "data parsed but no text: %r\n" % raw_data[:300]
                    )
            except (json.JSONDecodeError, AttributeError, TypeError) as e:
                sys.stderr.write(
                    "data parse failed (%s): %r\n" % (e, raw_data[:300])
                )
                text = ""
        if not text:
            sys.stderr.write(
                "400 missing text: ctype=%r keys=%r\n"
                % (self.headers.get("Content-Type"), sorted(params))
            )
            self._send(
                400,
                {"error": 'missing "text"', "message": 'missing "text"'},
            )
            return
        requested = params.get("language", params.get("lang", [""]))[0]
        preferred = tuple(
            v.split("-")[0]
            for v in re.split(r"[,;]", params.get("preferredVariants", [""])[0])
            if v and v.split("-")[0] in LANGS
        )
        lang = normalize_lang(requested, text, preferred)
        if lang is None:
            sys.stderr.write(
                "400 unsupported language %r: keys=%r\n" % (requested, sorted(params))
            )
            self._send(
                400,
                {
                    "message": f"Unsupported language {requested!r}",
                    "error": f"Unsupported language {requested!r}",
                    "supported": LANGS,
                },
            )
            return
        try:
            # LT `level=picky` activates tags="picky" rules; route to the
            # matching worker (spawned with NLPRULE_PICKY=1)
            picky = str(params.get("level", [""])[0]).lower() == "picky"
            raw = WORKERS[(lang, picky)].check(text)
        except TimeoutError as e:
            self._send(504, {"error": str(e)})
            return
        except RuntimeError as e:
            self._send(500, {"error": str(e)})
            return

        if path == "/check":  # native simple endpoint
            self._send(200, {"language": lang, "text": text, "matches": to_lt_matches(text, raw)})
            return

        # /v2/check: apply rule/category filters the extension may send
        disabled_rules = _csv(params, "disabledRules")
        disabled_cats = _csv(params, "disabledCategories")
        enabled_rules = _csv(params, "enabledRules")
        enabled_cats = _csv(params, "enabledCategories")
        enabled_only = params.get("enabledOnly", ["false"])[0].lower() == "true"
        filtered = []
        for full_id, bstart, bend, repl in raw:
            rule_id, cat_id = split_rule_id(full_id)
            if rule_id in disabled_rules or cat_id in disabled_cats:
                continue
            if enabled_only and rule_id not in enabled_rules and cat_id not in enabled_cats:
                continue
            filtered.append((full_id, bstart, bend, repl))

        matches = to_lt_matches(text, filtered)
        # echo the client's code ("ca-ES") when one was given; the extension
        # maps response languages against its dropdown by long code
        code_out = requested if requested and requested != "auto" else LANG_LONG.get(lang, lang)
        lang_entry = {
            "name": LANG_NAMES.get(lang, lang),
            "code": code_out,
            "detectedLanguage": {
                "name": LANG_NAMES.get(lang, lang),
                "code": LANG_LONG.get(lang, lang),
            },
        }
        self._send(
            200,
            {
                "software": {
                    "name": "nlprule",
                    "version": VERSION,
                    "apiVersion": 1,
                    "premium": True,
                    "premiumHint": "You have a premium account",
                    "status": "",
                },
                "warnings": {"incompleteResults": False},
                "language": lang_entry,
                "matches": matches,
            },
        )

    def log_message(self, fmt, *args):
        sys.stderr.write("%s - %s\n" % (self.address_string(), fmt % args))


if __name__ == "__main__":
    server = ThreadingHTTPServer(("0.0.0.0", PORT), Handler)
    print(
        f"nlprule HTTP front-end (LT-compatible /v2/check) on :{PORT}, "
        f"langs={LANGS}, storage={STORAGE}",
        flush=True,
    )
    server.serve_forever()
