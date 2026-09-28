use crate::{rules::RulesLangOptions, tokenizer::TokenizerLangOptions};
use crate::{tokenizer::tag::TaggerLangOptions, types::*};
use lazy_static::lazy_static;

lazy_static! {
    static ref TOKENIZER_LANG_OPTIONS: DefaultHashMap<String, TokenizerLangOptions> = {
        serde_json::from_slice(include_bytes!(concat!(
            env!("OUT_DIR"),
            "/",
            "tokenizer_configs.json"
        )))
        .expect("tokenizer configs must be valid JSON")
    };
}

lazy_static! {
    static ref RULES_LANG_OPTIONS: DefaultHashMap<String, RulesLangOptions> = {
        serde_json::from_slice(include_bytes!(concat!(
            env!("OUT_DIR"),
            "/",
            "rules_configs.json"
        )))
        .expect("rules configs must be valid JSON")
    };
}

lazy_static! {
    static ref TAGGER_LANG_OPTIONS: DefaultHashMap<String, TaggerLangOptions> = {
        serde_json::from_slice(include_bytes!(concat!(
            env!("OUT_DIR"),
            "/",
            "tagger_configs.json"
        )))
        .expect("tagger configs must be valid JSON")
    };
}

/// Gets the tokenizer language options for the language code
pub(crate) fn tokenizer_lang_options(lang_code: &str) -> Option<TokenizerLangOptions> {
    TOKENIZER_LANG_OPTIONS.get(lang_code).cloned()
}

/// Gets the rules language options for the language code
pub(crate) fn rules_lang_options(lang_code: &str) -> Option<RulesLangOptions> {
    RULES_LANG_OPTIONS.get(lang_code).cloned()
}

/// Gets the tagger language options for the language code
pub(crate) fn tagger_lang_options(lang_code: &str) -> Option<TaggerLangOptions> {
    TAGGER_LANG_OPTIONS.get(lang_code).cloned()
}

pub(crate) use regex::from_java_regex;

mod regex {
    use std::collections::HashMap;

    use lazy_static::lazy_static;
    use regex_syntax::ast::{
        print::Printer, Ast, Class, ClassBracketed, ClassPerlKind, ClassSet, ClassSetItem,
        ClassSetUnion, ErrorKind, Flag, FlagsItemKind, Literal, LiteralKind, Position, Span,
    };

    use crate::compile::Error;

    fn zero_span() -> Span {
        Span {
            start: Position::new(0, 0, 0),
            end: Position::new(0, 0, 0),
        }
    }

    fn to_lower_literal(literal: &Literal) -> Option<Literal> {
        let chars: Vec<_> = literal.c.to_lowercase().collect();

        match &chars[..] {
            [c] => {
                let mut out = literal.clone();
                out.c = *c;

                Some(out)
            }
            _ => None,
        }
    }

    fn to_upper_literal(literal: &Literal) -> Option<Literal> {
        let chars: Vec<_> = literal.c.to_uppercase().collect();

        match &chars[..] {
            [c] => {
                let mut out = literal.clone();
                out.c = *c;

                Some(out)
            }
            _ => None,
        }
    }

    fn literal_to_union(literal: &Literal) -> Option<ClassSetUnion> {
        let lower = to_lower_literal(literal)?;
        let upper = to_upper_literal(literal)?;

        Some(ClassSetUnion {
            span: zero_span(),
            items: if lower == upper {
                vec![ClassSetItem::Literal(lower)]
            } else {
                vec![ClassSetItem::Literal(lower), ClassSetItem::Literal(upper)]
            },
        })
    }

    /// Returns a case insensitive version of the given `ClassSetItem`.
    fn to_i_item(root: &ClassSetItem) -> ClassSetItem {
        match root {
            ClassSetItem::Literal(literal) => {
                if let Some(union) = literal_to_union(literal) {
                    ClassSetItem::Union(union)
                } else {
                    ClassSetItem::Literal(literal.clone())
                }
            }
            ClassSetItem::Range(range) => {
                assert!(range.is_valid()); // would have returned an error otherwise

                match (to_upper_literal(&range.start), to_lower_literal(&range.end)) {
                    (Some(start), Some(end)) => {
                        let mut out = range.clone();
                        out.start = start;
                        out.end = end;
                        ClassSetItem::Range(out)
                    }
                    _ => ClassSetItem::Range(range.clone()),
                }
            }
            ClassSetItem::Union(union) => {
                let mut union = union.clone();

                union.items = union.items.iter().map(to_i_item).collect();
                ClassSetItem::Union(union)
            }
            ClassSetItem::Bracketed(bracketed) => {
                let mut bracketed = bracketed.clone();
                bracketed.kind = to_i_class_set(&bracketed.kind);
                ClassSetItem::Bracketed(bracketed)
            }
            ClassSetItem::Empty(_)
            | ClassSetItem::Ascii(_)
            | ClassSetItem::Unicode(_)
            | ClassSetItem::Perl(_) => root.clone(),
        }
    }

    /// Returns a case insensitive version of the given `ClassSet`.
    fn to_i_class_set(root: &ClassSet) -> ClassSet {
        match root {
            ClassSet::Item(item) => ClassSet::Item(to_i_item(item)),
            ClassSet::BinaryOp(op) => {
                let mut op = op.clone();
                op.rhs = to_i_class_set(&op.rhs).into();
                op.lhs = to_i_class_set(&op.lhs).into();
                ClassSet::BinaryOp(op)
            }
        }
    }

    /// "Fixes" the AST by:
    /// * removing case insensitive and unicode flags since their behavior is not consistent
    ///     e. g. (?i)\p{Lu} is equivalent to \p{L} in `fancy_regex` and to \p{Lu} in Java / Oniguruma
    /// * manually making the case insensitive parts case insensitive instead. This is done by:
    ///     * for each literal which has a single-char uppercase and lowercase variant, replace the literal
    ///         by a set of the uppercase and lowercase variant of the union e. g. "a" to "[aA]".
    ///     * uppercasing range start and lowercasing range end e.g. [A-Z] to [A-z].
    ///         This is also only done for chars with single-char upper- / lowercase variants.
    /// * disallowing nested quantifiers
    fn fix_ast(root: &Ast, mut case_sensitive: bool) -> Result<(Ast, bool), Error> {
        let ast = match root {
            Ast::Alternation(alternation) => {
                let mut alternation = alternation.clone();

                alternation.asts = alternation
                    .asts
                    .iter()
                    .map(|x| {
                        let (ast, i) = fix_ast(x, case_sensitive)?;
                        case_sensitive = i;
                        Ok(ast)
                    })
                    .collect::<Result<_, Error>>()?;
                Ast::Alternation(alternation)
            }
            Ast::Concat(concat) => {
                let mut concat = concat.clone();

                concat.asts = concat
                    .asts
                    .iter()
                    .map(|x| {
                        let (ast, i) = fix_ast(x, case_sensitive)?;
                        case_sensitive = i;
                        Ok(ast)
                    })
                    .collect::<Result<_, Error>>()?;
                Ast::Concat(concat)
            }
            Ast::Class(class) => match &class {
                Class::Bracketed(bracketed) => {
                    let mut bracketed = bracketed.clone();
                    if !case_sensitive {
                        bracketed.kind = to_i_class_set(&bracketed.kind);
                    }
                    Ast::Class(Class::Bracketed(bracketed))
                }
                Class::Perl(perl) => {
                    // \s is rewritten to a literal space; \S (negated) must
                    // keep its class semantics — it previously collapsed to
                    // ' ' too, silently breaking patterns like `(.*\S)(-)`
                    if matches!(perl.kind, ClassPerlKind::Space) && !perl.negated {
                        Ast::Literal(Literal {
                            span: zero_span(),
                            kind: LiteralKind::Verbatim,
                            c: ' ',
                        })
                    } else {
                        Ast::Class(class.clone())
                    }
                }
                Class::Unicode(_) => Ast::Class(class.clone()),
            },
            Ast::Group(group) => {
                let mut group = group.clone();
                group.ast = fix_ast(&group.ast, case_sensitive)?.0.into();
                Ast::Group(group)
            }
            Ast::Repetition(repetition) => {
                let mut repetition = repetition.clone();
                repetition.ast = fix_ast(&repetition.ast, case_sensitive)?.0.into();
                // disallow nested quantifiers because of inconsistent behavior
                if matches!(*repetition.ast, Ast::Repetition(_)) {
                    return Err(Error::Unexpected(
                        "nested quantifiers in regex are not allowed.".into(),
                    ));
                }

                Ast::Repetition(repetition)
            }
            Ast::Literal(literal) if !case_sensitive => {
                if let Some(union) = literal_to_union(literal) {
                    Ast::Class(Class::Bracketed(ClassBracketed {
                        span: zero_span(),
                        negated: false,
                        kind: ClassSet::Item(ClassSetItem::Union(union)),
                    }))
                } else {
                    Ast::Literal(literal.clone())
                }
            }
            Ast::Literal(_) => root.clone(),
            Ast::Flags(flags) => {
                let mut flags = flags.clone();

                if let Some(i) = flags.flags.flag_state(Flag::CaseInsensitive) {
                    case_sensitive = !i;
                }

                flags.flags.items = flags
                    .flags
                    .items
                    .into_iter()
                    .filter(|flag| {
                        !matches!(
                            flag.kind,
                            FlagsItemKind::Flag(Flag::CaseInsensitive)
                            // we completely ignore the unicode flag, might not be sound
                                | FlagsItemKind::Flag(Flag::Unicode)
                        )
                    })
                    .collect();

                if !flags.flags.items.is_empty()
                    && matches!(
                        flags.flags.items[flags.flags.items.len() - 1].kind,
                        FlagsItemKind::Negation
                    )
                {
                    flags.flags.items.pop();
                }

                if flags.flags.items.is_empty() {
                    Ast::Empty(zero_span())
                } else {
                    Ast::Flags(flags)
                }
            }
            Ast::Dot(_) | Ast::Assertion(_) | Ast::Empty(_) => root.clone(),
        };
        Ok((ast, case_sensitive))
    }

    lazy_static! {
        static ref LOOKAROUND_MAP: HashMap<&'static str, &'static str> = {
            let mut map = HashMap::new();
            map.insert("(?!", r"(?P<__NEGATIVE_LOOKAHEAD_$1>");
            map.insert("(?<!", r"(?P<__NEGATIVE_LOOKBEHIND_$1>");
            map.insert("(?=", r"(?P<__POSITIVE_LOOKAHEAD_$1>");
            map.insert("(?<=", r"(?P<__POSITIVE_LOOKBEHIND_$1>");
            map
        };
    }

    /// Does a good effort of converting Java Regexes to regular expressions
    /// usable by `oniguruma` / `fancy-regex`.
    pub fn from_java_regex(
        in_regex: &str,
        case_sensitive: bool,
        full_match: bool,
    ) -> Result<String, Error> {
        let mut regex = in_regex.to_owned();
        // decode Java surrogate pairs (`\ud83c\udc00`) to astral code
        // points (`\u{1f000}`); regex-syntax rejects lone surrogates
        // (de KOMMA_ZWISCHEN emoji classes)
        {
            let mut out = String::with_capacity(regex.len());
            let b = regex.as_bytes();
            let mut i = 0usize;
            while i < b.len() {
                if b[i] == b'\\'
                    && i + 1 < b.len()
                    && (b[i + 1] == b'u' || b[i + 1] == b'U')
                    && i + 6 <= b.len()
                {
                    let hex = &regex[i + 2..i + 6];
                    if let Ok(cp) = u32::from_str_radix(hex, 16) {
                        if (0xD800..=0xDBFF).contains(&cp)
                            && i + 12 <= b.len()
                            && &regex[i + 6..i + 8] == "\\u"
                        {
                            if let Ok(lo) = u32::from_str_radix(&regex[i + 8..i + 12], 16) {
                                if (0xDC00..=0xDFFF).contains(&lo) {
                                    let c = 0x10000 + ((cp - 0xD800) << 10) + (lo - 0xDC00);
                                    // oniguruma rejects `\u{...}` escapes: emit
                                    // the astral character literally instead
                                    if let Some(ch) = char::from_u32(c) {
                                        out.push(ch);
                                    }
                                    i += 12;
                                    continue;
                                }
                            }
                        }
                        // lone surrogate: replace with the replacement char
                        if (0xD800..=0xDFFF).contains(&cp) {
                            out.push('\u{fffd}');
                            i += 6;
                            continue;
                        }
                    }
                }
                let ch_len = utf8_char_len(b[i]);
                out.push_str(&regex[i..i + ch_len]);
                i += ch_len;
            }
            regex = out;
        }
        // drop quantifiers applied to zero-width anchors: Java tolerates
        // `PRP$?` (postag classes), oniguruma rejects it
        {
            let b = regex.as_bytes();
            let mut out = String::with_capacity(regex.len());
            let mut i = 0usize;
            let mut class_depth = 0usize;
            while i < b.len() {
                let c = b[i];
                if c == b'\\' && i + 1 < b.len() {
                    // `\b*`, `\A+`, ...: keep the anchor, drop quantifier(s).
                    // ONLY actual zero-width anchors — NOT arbitrary escaped
                    // chars (`\d+`, `\-?`, ... must keep their quantifier)
                    let is_anchor = matches!(b[i + 1], b'b' | b'B' | b'A' | b'z' | b'Z');
                    if is_anchor
                        && class_depth == 0
                        && i + 2 < b.len()
                        && (b[i + 2] == b'?'
                            || b[i + 2] == b'*'
                            || b[i + 2] == b'+')
                    {
                        out.push_str(&regex[i..i + 2]);
                        i += 2;
                        // consume the quantifier and an optional laziness `?`
                        i += if i + 1 < b.len() && b[i + 1] == b'?' { 2 } else { 1 };
                        continue;
                    }
                    let seq_len = utf8_char_len(b[i + 1]) + 1;
                    out.push_str(&regex[i..i + seq_len]);
                    i += seq_len;
                    continue;
                }
                if c == b'[' && class_depth == 0 {
                    class_depth += 1;
                } else if c == b']' && class_depth > 0 {
                    class_depth -= 1;
                } else if class_depth == 0 && (c == b'$' || c == b'^') {
                    if i + 1 < b.len() && (b[i + 1] == b'?' || b[i + 1] == b'*' || b[i + 1] == b'+') {
                        out.push(c as char);
                        i += 1;
                        // consume the quantifier and an optional laziness `?`
                        i += if i + 1 < b.len() && b[i + 1] == b'?' { 2 } else { 1 };
                        continue;
                    }
                }
                let ch_len = utf8_char_len(c);
                out.push_str(&regex[i..i + ch_len]);
                i += ch_len;
            }
            regex = out;
        }
        // strip Java-only inline flags (e.g. `d` = UNIX_LINES in en
        // ID_CASING `(?id)`): regex-syntax rejects them outright
        {
            let b = regex.as_bytes();
            let mut out = String::with_capacity(regex.len());
            let mut i = 0usize;
            while i < b.len() {
                if b[i] == b'(' && i + 2 < b.len() && b[i + 1] == b'?'
                    && b[i + 2] != b'='
                    && b[i + 2] != b'!'
                    && b[i + 2] != b'<'
                    && b[i + 2] != b'P'
                {
                    if let Some(rel) = regex[i..].find(|c| c == ')' || c == ':') {
                        let end = i + rel;
                        let flags = &regex[i + 2..end];
                        // empty flags (`(?)`) are a Java no-op group —
                        // they appear verbatim in en LIFE_LIVE/ID_CASING
                        let is_flags = flags
                            .chars()
                            .all(|c| c.is_ascii_alphabetic() || c == '-');
                        if is_flags {
                            let kept: String = flags
                                .chars()
                                .filter(|c| matches!(c, 'i' | 'm' | 's' | 'x' | 'U' | 'u' | '-'))
                                .collect();
                            if kept.replace('-', "").is_empty() {
                                // no usable flags left: `(?d)` → "", `(?d:` → "(?:"
                                if b[end] == b')' {
                                    i = end + 1;
                                    continue;
                                }
                                out.push_str("(?:");
                                i = end + 1;
                                continue;
                            }
                            if kept != *flags {
                                out.push_str("(?");
                                out.push_str(&kept);
                                out.push(if b[end] == b')' { ')' } else { ':' });
                                i = end + 1;
                                continue;
                            }
                        }
                    }
                }
                let ch_len = utf8_char_len(b[i]);
                out.push_str(&regex[i..i + ch_len]);
                i += ch_len;
            }
            regex = out;
        }
        // pre-normalize Java-only syntax that the regex-syntax AST parser
        // rejects outright:
        // - possessive quantifiers (X*+ X++ X?+ X{n,m}+) -> greedy
        // - a dangling `(?-)` (no flags negated) -> removed
        // (regex already holds the input after the surrogate/anchor passes)
        // repeatedly de-possessivize the leftmost possessive quantifier
        loop {
            let mut possessive: Option<(usize, usize)> = None;
            for pat in ["*+", "++", "?+"] {
                if let Some(pos) = regex.find(pat) {
                    if possessive.map_or(true, |(p, _)| pos < p) {
                        possessive = Some((pos, pat.len()));
                    }
                }
            }
            // brace quantifiers: `{n}`, `{n,}`, `{n,m}` followed by `+`
            // (e.g. pl SKROTOWCE `\p{Lu}{2}+[i]*`)
            let bytes = regex.as_bytes();
            let mut i = 0usize;
            while i < bytes.len() {
                if bytes[i] == b'{' {
                    let mut j = i + 1;
                    let mut digit_or_comma = false;
                    while j < bytes.len()
                        && (bytes[j].is_ascii_digit() || bytes[j] == b',')
                    {
                        digit_or_comma = true;
                        j += 1;
                    }
                    if digit_or_comma && j < bytes.len() && bytes[j] == b'}' && j + 1 < bytes.len() && bytes[j + 1] == b'+' {
                        let pos = j + 1;
                        if possessive.map_or(true, |(p, _)| pos < p) {
                            possessive = Some((pos, 1));
                        }
                    }
                }
                i += 1;
            }
            match possessive {
                Some((pos, len)) => {
                    let quant = if len == 2 {
                        regex[pos..pos + len].chars().next().unwrap()
                    } else {
                        // brace possessive: drop just the `+`
                        regex.remove(pos);
                        continue;
                    };
                    regex.replace_range(pos..pos + len, &quant.to_string());
                }
                None => break,
            }
        }
        regex = regex.replace("(?-)", "");
        // quantifier immediately followed by a brace quantifier (pl
        // JEDNOSTKA_LICZBA `\d+{1,4}r`): drop the redundant first
        // quantifier, keeping the more specific brace repetition
        {
            let b = regex.as_bytes();
            let mut out = String::with_capacity(regex.len());
            let mut i = 0usize;
            let mut class_depth = 0usize;
            while i < b.len() {
                let c = b[i];
                if c == b'\\' && i + 1 < b.len() {
                    // advance past the backslash AND the full escaped
                    // character (it may be multi-byte, e.g. `\—`)
                    let next_len = utf8_char_len(b[i + 1]);
                    out.push_str(&regex[i..i + 1 + next_len]);
                    i += 1 + next_len;
                    continue;
                }
                if c == b'[' && class_depth == 0 {
                    class_depth += 1;
                } else if c == b']' && class_depth > 0 {
                    class_depth -= 1;
                }
                if class_depth == 0 && (c == b'+' || c == b'*' || c == b'?') {
                    // look ahead for `{n}` / `{n,}` / `{n,m}`
                    if i + 1 < b.len() && b[i + 1] == b'{' {
                        let mut j = i + 2;
                        let mut digits = false;
                        while j < b.len() && (b[j].is_ascii_digit() || b[j] == b',') {
                            digits = true;
                            j += 1;
                        }
                        if digits && j < b.len() && b[j] == b'}' {
                            // skip the redundant quantifier character
                            i += 1;
                            continue;
                        }
                    }
                }
                let ch_len = utf8_char_len(b[i]);
                out.push_str(&regex[i..i + ch_len]);
                i += ch_len;
            }
            regex = out;
        }
        // Java script property syntax `\p{IsLatin}` → `\p{Latin}` (oniguruma
        // does not accept the `Is` prefix; nl SPATIE_NA)
        regex = regex.replace("\\p{Is", "\\p{").replace("\\P{Is", "\\P{");
        // Java octal escapes `\0n`/`\0nn` are not backreferences but literal
        // code points (pl BRAK_KROPKI `[...\02]`); regex-syntax rejects
        // `\0N` as a backreference so convert to hex escapes first
        {
            let mut converted = String::with_capacity(regex.len());
            let chars: Vec<(usize, char)> = regex.char_indices().collect();
            let mut idx = 0usize;
            while idx < chars.len() {
                let (_, c) = chars[idx];
                if c == '\\'
                    && idx + 1 < chars.len()
                    && chars[idx + 1].1 == '0'
                {
                    let mut j = idx + 2;
                    let mut val: u32 = 0;
                    let mut digits = 0;
                    while j < chars.len() && digits < 2 && ('0'..='7').contains(&chars[j].1) {
                        val = val * 8 + chars[j].1.to_digit(8).unwrap();
                        j += 1;
                        digits += 1;
                    }
                    converted.push_str(&format!("\\x{{{:x}}}", val));
                    idx = j;
                    continue;
                }
                converted.push(c);
                idx += 1;
            }
            regex = converted;
        }
        // duplicate inline flags, e.g. `(?ii)` (Java tolerates, regex-syntax
        // does not)
        loop {
            let reduced = regex.replace("(?ii)", "(?i)");
            if reduced == regex {
                break;
            }
            regex = reduced;
        }
        let mut prev_error_start = None;

        let mut ast = loop {
            let mut ast_parser = regex_syntax::ast::parse::Parser::new();
            match ast_parser.parse(&regex) {
                Err(error) => {
                    let start = error.span().start.offset;
                    let end = error.span().end.offset;

                    // exit if an error already occured at the same position
                    // to prevent infinitely looping
                    if let Some(prev_error_start) = prev_error_start {
                        if prev_error_start == start {
                            break Err(error);
                        }
                    }

                    match error.kind() {
                        ErrorKind::EscapeUnrecognized => {
                            // remove the backslash
                            regex =
                                format!("{}{}", &regex[..start], &regex[start + '\\'.len_utf8()..]);
                        }
                        ErrorKind::UnsupportedLookAround => {
                            // replace unsupported lookaround syntax with a placeholder named group
                            if let Some(placeholder) = LOOKAROUND_MAP.get(&regex[start..end]) {
                                regex = format!(
                                    "{}{}{}",
                                    &regex[..start],
                                    placeholder.replace(r"$1", &start.to_string()),
                                    &regex[end..]
                                );
                            } else {
                                break Err(error);
                            }
                        }
                        _ => break Err(error),
                    }

                    prev_error_start = Some(start);
                }
                Ok(ast) => break Ok(ast),
            }
        }?;

        ast = fix_ast(&ast, case_sensitive)?.0;

        let mut printer = Printer::new();
        let mut out = String::new();
        printer.print(&ast, &mut out).unwrap();

        // undo the placeholder named group replacement for lookaround
        for (original, placeholder) in LOOKAROUND_MAP.iter() {
            if let Some(index) = prev_error_start {
                for i in 0..(index + 1) {
                    out = out.replace(&placeholder.replace("$1", &i.to_string()), original);
                }
            }
        }

        out = hoist_lookbehind_groups(&out);
        // an inline flag group emptied by the case-insensitive rewrite
        // prints as `(?)` (en ID_CASING `(?i)id` → `(?)id`): a no-op, drop it
        while out.contains("(?)") {
            out = out.replace("(?)", "");
        }
        // regex-syntax's printer emits `\xC1` byte escapes for non-ASCII
        // literals; in oniguruma UTF-8 mode a lone `\xC1` byte is an
        // incomplete multibyte sequence (pt LP_PARONYMS). Emit the literal
        // character instead.
        {
            let b = out.as_bytes();
            let mut res = String::with_capacity(out.len());
            let mut i = 0usize;
            while i < b.len() {
                if b[i] == b'\\'
                    && i + 4 <= b.len()
                    && b[i + 1] == b'x'
                    && b[i + 2].is_ascii_hexdigit()
                    && b[i + 3].is_ascii_hexdigit()
                    && !(i + 4 < b.len() && b[i + 4] == b'{')
                    && !(i + 4 < b.len() && b[i + 4].is_ascii_hexdigit())
                {
                    let hex = &out[i + 2..i + 4];
                    if let Ok(cp) = u8::from_str_radix(hex, 16) {
                        if cp >= 0x80 {
                            if let Some(c) = char::from_u32(cp as u32) {
                                res.push(c);
                                i += 4;
                                continue;
                            }
                        }
                    }
                }
                let ch_len = utf8_char_len(b[i]);
                res.push_str(&out[i..i + ch_len]);
                i += ch_len;
            }
            out = res;
        }

        if full_match {
            out = format!("^(?:{})$", out);
        }

        Ok(out)
    }

    /// Oniguruma only allows *top-level* alternation in look-behind
    /// (`(?<!a|bc)` is fine) but rejects a capture group wrapping the
    /// alternation (`(?<!([a-vyz]|[a-vyz]\d))` → "invalid pattern in
    /// look-behind"). Hoist the group out as an empty capture `()` so the
    /// alternation becomes top-level; the empty group keeps capture
    /// numbering stable for later `\N` references.
    fn hoist_lookbehind_groups(regex: &str) -> String {
        let bytes = regex.as_bytes();
        let mut out = String::with_capacity(regex.len());
        let mut i = 0usize;
        while i < bytes.len() {
            // candidates: "(?<=" or "(?<!"
            if bytes[i] == b'('
                && i + 4 < bytes.len()
                && bytes[i + 1] == b'?'
                && bytes[i + 2] == b'<'
                && (bytes[i + 3] == b'=' || bytes[i + 3] == b'!')
                && bytes[i + 4] == b'('
            {
                let group_open = i + 4;
                if let Some(group_close) = scan_matching_paren(bytes, group_open) {
                    let lb_close = group_close + 1;
                    if lb_close < bytes.len() && bytes[lb_close] == b')' {
                        let inner = &regex[group_open + 1..group_close];
                        if has_top_level_alternation(inner) {
                            out.push_str("()");
                            out.push_str(if bytes[i + 3] == b'!' {
                                "(?<!"
                            } else {
                                "(?<="
                            });
                            out.push_str(inner);
                            out.push(')');
                            i = lb_close + 1;
                            continue;
                        }
                    }
                }
            }
            // copy one full UTF-8 character
            let ch_len = utf8_char_len(bytes[i]);
            out.push_str(&regex[i..i + ch_len]);
            i += ch_len;
        }
        out
    }

    fn utf8_char_len(b: u8) -> usize {
        if b < 0x80 {
            1
        } else if b >> 5 == 0b110 {
            2
        } else if b >> 4 == 0b1110 {
            3
        } else {
            4
        }
    }

    /// Index of the `)` matching the `(` at `open` (escapes and character
    /// classes are respected).
    fn scan_matching_paren(bytes: &[u8], open: usize) -> Option<usize> {
        let mut depth = 0usize;
        let mut class_depth = 0usize;
        let mut i = open;
        while i < bytes.len() {
            match bytes[i] {
                b'\\' => {
                    i += 2;
                    continue;
                }
                b'[' if class_depth == 0 => class_depth += 1,
                b']' if class_depth > 0 => class_depth -= 1,
                b'(' if class_depth == 0 => depth += 1,
                b')' if class_depth == 0 => {
                    depth -= 1;
                    if depth == 0 {
                        return Some(i);
                    }
                }
                _ => {}
            }
            i += 1;
        }
        None
    }

    fn has_top_level_alternation(inner: &str) -> bool {
        let bytes = inner.as_bytes();
        let mut depth = 0usize;
        let mut class_depth = 0usize;
        let mut i = 0usize;
        while i < bytes.len() {
            match bytes[i] {
                b'\\' => {
                    i += 2;
                    continue;
                }
                b'[' if class_depth == 0 => class_depth += 1,
                b']' if class_depth > 0 => class_depth -= 1,
                b'(' if class_depth == 0 => depth += 1,
                b')' if class_depth == 0 => depth -= 1,
                b'|' if depth == 0 && class_depth == 0 => return true,
                _ => {}
            }
            i += 1;
        }
        false
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn i_flag_removed() {
            assert_eq!(
                from_java_regex(r"(?iu)\p{Lu}\p{Ll}+", false, false).unwrap(),
                r"\p{Lu}\p{Ll}+"
            )
        }

        #[test]
        fn i_flag_used() {
            assert_eq!(
                from_java_regex(r"(?i)a(?-i)b", false, false).unwrap(),
                r"[aA]b"
            )
        }

        #[test]
        fn positive_lookbehind() {
            assert_eq!(
                from_java_regex(r"(?i)(?<=x)(?-i)s", false, false).unwrap(),
                r"(?<=[xX])s"
            )
        }


        #[test]
        fn sk_spojovnik_regexes() {
            for pat in [r"(.*\S)(-)", r".*\S", r".*\p{L}"] {
                let conv = from_java_regex(pat, true, true);
                assert!(conv.is_ok(), "conversion failed for {:?}", pat);
                let conv = conv.unwrap();
                let re = crate::utils::regex::Regex::new(conv);
                assert!(re.try_compile().is_ok(), "compile failed for {:?}", pat);
                let subject = if pat.contains(r"\p{L}") { "test" } else { "test-" };
                assert!(re.is_match(subject), "no match on '{:?}' for {:?}", subject, pat);
            }
            // \s still rewrites to a literal space (LT semantics)
            assert_eq!(
                from_java_regex(r".*\s", true, true).unwrap(),
                "^(?:.* )$"
            );
        }

        #[test]
        fn nested_quantifiers() {
            assert!(from_java_regex(r"[0-9,.]*{1,}", false, false).is_err())
        }
    }
}

#[cfg(test)]
mod fr_regex_tests {
    use super::*;

    #[test]
    fn fr_det_postag_regex() {
        let re = from_java_regex(r"(P\+)?D .*", true, true).unwrap();
        let compiled = crate::utils::regex::Regex::new(re.clone()).try_compile();
        assert!(compiled.is_ok(), "compile failed for {}", re);
        let r = crate::utils::regex::Regex::new(re);
        assert!(r.is_match("D f s"), "no match on D f s: {}", r.as_str());
        assert!(r.is_match("P+D f s"));
    }
}
