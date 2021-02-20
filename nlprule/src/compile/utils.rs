use std::sync::Arc;

use crate::types::*;
use crate::{rules::RulesOptions, tokenizer::TokenizerOptions};
use lazy_static::lazy_static;

lazy_static! {
    static ref TOKENIZER_CONFIGS: DefaultHashMap<String, Arc<TokenizerOptions>> = {
        serde_json::from_slice(include_bytes!(concat!(
            env!("OUT_DIR"),
            "/",
            "tokenizer_configs.json"
        )))
        .expect("tokenizer configs must be valid JSON")
    };
}

lazy_static! {
    static ref RULES_CONFIGS: DefaultHashMap<String, Arc<RulesOptions>> = {
        serde_json::from_slice(include_bytes!(concat!(
            env!("OUT_DIR"),
            "/",
            "rules_configs.json"
        )))
        .expect("rules configs must be valid JSON")
    };
}

/// Gets the tokenizer language options for the language code
pub(crate) fn tokenizer_options(lang_code: &str) -> Option<Arc<TokenizerOptions>> {
    TOKENIZER_CONFIGS.get(lang_code).cloned()
}

/// Gets the rules language options for the language code
pub(crate) fn rules_options(lang_code: &str) -> Option<Arc<RulesOptions>> {
    RULES_CONFIGS.get(lang_code).cloned()
}

pub use regex::from_java_regex;

mod regex {
    use std::collections::HashMap;

    use lazy_static::lazy_static;
    use regex_syntax::ast::{
        print::Printer, Ast, Class, ClassBracketed, ClassPerlKind, ClassSet, ClassSetItem,
        ClassSetUnion, ErrorKind, Flag, FlagsItemKind, Literal, LiteralKind, Position, Span,
    };

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

    fn fix_ast(root: &Ast, mut case_sensitive: bool) -> (Ast, bool) {
        let ast = match root {
            Ast::Alternation(alternation) => {
                let mut alternation = alternation.clone();

                alternation.asts = alternation
                    .asts
                    .iter()
                    .map(|x| {
                        let (ast, i) = fix_ast(x, case_sensitive);
                        case_sensitive = i;
                        ast
                    })
                    .collect();
                Ast::Alternation(alternation)
            }
            Ast::Concat(concat) => {
                let mut concat = concat.clone();

                concat.asts = concat
                    .asts
                    .iter()
                    .map(|x| {
                        let (ast, i) = fix_ast(x, case_sensitive);
                        case_sensitive = i;
                        ast
                    })
                    .collect();
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
                    if matches!(perl.kind, ClassPerlKind::Space) {
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
                group.ast = fix_ast(&group.ast, case_sensitive).0.into();
                Ast::Group(group)
            }
            Ast::Repetition(repetition) => {
                let mut repetition = repetition.clone();
                repetition.ast = fix_ast(&repetition.ast, case_sensitive).0.into();
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
        (ast, case_sensitive)
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

    pub fn from_java_regex(
        in_regex: &str,
        case_sensitive: bool,
        full_match: bool,
    ) -> Result<String, regex_syntax::Error> {
        let mut regex = in_regex.to_owned();
        let mut prev_error_start = None;

        let mut ast = loop {
            let mut ast_parser = regex_syntax::ast::parse::Parser::new();
            match ast_parser.parse(&regex) {
                Err(error) => {
                    let start = error.span().start.offset;
                    let end = error.span().end.offset;

                    if let Some(prev_error_start) = prev_error_start {
                        if prev_error_start == start {
                            break Err(error);
                        }
                    }

                    match error.kind() {
                        ErrorKind::EscapeUnrecognized => {
                            regex = format!("{}{}", &regex[..start], &regex[start + 1..]);
                        }
                        ErrorKind::UnsupportedLookAround => {
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

        ast = fix_ast(&ast, case_sensitive).0;

        let mut printer = Printer::new();
        let mut out = String::new();
        printer.print(&ast, &mut out).unwrap();

        for (original, placeholder) in LOOKAROUND_MAP.iter() {
            if let Some(index) = prev_error_start {
                for i in 0..(index + 1) {
                    out = out.replace(&placeholder.replace("$1", &i.to_string()), original);
                }
            }
        }

        if full_match {
            out = format!("^(?:{})$", out);
        }

        Ok(out)
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn i_flag_removed() {
            assert_eq!(
                from_java_regex(r"(?iu)\p{Lu}\p{Ll}+", false, false).unwrap(),
                r"(?u)\p{Lu}\p{Ll}+"
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
    }
}
