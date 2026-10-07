#!/bin/sh
# Entrypoint for the nlprule Docker image.
#
# Usage:
#   <lang>                        check stdin (one sentence per line, JSONL out)
#   dump-rules <lang>             print the loaded-rule inventory as JSON
#   <tokenizer.bin> <rules.bin> [lang]   run check_server on explicit bins
#   http                          run the HTTP front-end (service image only)
#   sh | bash                     drop into a shell (all binaries on PATH)
#
# Rule binaries live under $NLPRULE_STORAGE (default /storage). Set
# NLPRULE_ENABLE="CAT/GROUP/N,..." to activate default-off rules.
set -e

STORAGE="${NLPRULE_STORAGE:-/storage}"

if [ "$#" -eq 0 ]; then
    echo "usage: entrypoint <lang> | dump-rules <lang> | <tokenizer.bin> <rules.bin> [lang] | sh" >&2
    exit 2
fi

case "$1" in
    sh|bash)
        exec "$@"
        ;;
    http)
        exec python3 /opt/nlprule-http/http_server.py
        ;;
    dump-rules)
        if [ "$#" -ne 2 ]; then
            echo "dump-rules needs exactly one language code" >&2
            exit 2
        fi
        exec check_server --dump-rules "$STORAGE/${2}_rules.bin" "$2"
        ;;
esac

if [ -f "$1" ]; then
    # explicit tokenizer/rules paths (e.g. bins mounted somewhere else)
    exec check_server "$@"
fi

lang="$1"
shift
exec check_server "$STORAGE/${lang}_tokenizer.bin" "$STORAGE/${lang}_rules.bin" "$lang"
