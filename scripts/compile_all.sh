#!/usr/bin/env bash
# Compile and test nlprule binaries for all languages in a data root.
# Usage: ./compile_all.sh [data_root] [storage_dir]
set -u
DATA_ROOT="${1:-../data2}"
STORAGE="${2:-storage}"
LOGDIR=/tmp/nlprule_logs
mkdir -p "$STORAGE" "$LOGDIR"

printf "%-26s %10s %10s %8s %8s\n" "LANG" "XML_RULES" "RUNNABLE" "PASSING" "PASS%"
for dir in "$DATA_ROOT"/*/; do
    lang=$(basename "$dir")
    [ -f "$dir/lang_code.txt" ] || continue

    # count XML rules (rules + rules inside groups)
    xml_rules=$(python3 -c "
import re, sys
try:
    xml = open('$dir/grammar.xml', encoding='utf-8', errors='replace').read()
    print(len(re.findall(r'<rule(?:group)?[ >]', xml)))
except FileNotFoundError:
    print(0)
")

    RUST_LOG=INFO ./target/release/compile \
        --build-dir "$dir" \
        --tokenizer-out "$STORAGE/${lang}_tokenizer.bin" \
        --rules-out "$STORAGE/${lang}_rules.bin" > "$LOGDIR/${lang}.log" 2>&1

    if [ $? -ne 0 ]; then
        printf "%-26s %10s %10s\n" "$lang" "$xml_rules" "COMPILE_ERR"
        continue
    fi

    RUST_LOG=WARN ./target/release/test \
        --tokenizer "$STORAGE/${lang}_tokenizer.bin" \
        --rules "$STORAGE/${lang}_rules.bin" > "$LOGDIR/${lang}_test.log" 2>&1

    runnable=$(grep -oE "Runnable rules: [0-9]+" "$LOGDIR/${lang}_test.log" | grep -oE "[0-9]+")
    passing=$(grep -oE "Rules passing tests: [0-9]+" "$LOGDIR/${lang}_test.log" | grep -oE "[0-9]+$")
    runnable=${runnable:-0}
    passing=${passing:-0}
    if [ "$runnable" -gt 0 ]; then
        pct=$(python3 -c "print(f'{100.0 * $passing / $runnable:.1f}')")
    else
        pct=0
    fi
    printf "%-26s %10s %10s %8s %8s\n" "$lang" "$xml_rules" "$runnable" "$passing" "$pct"
done
