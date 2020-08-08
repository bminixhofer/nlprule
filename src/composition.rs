use crate::tokenizer::Token;
use regex::Regex;

pub trait Match<T: ?Sized> {
    fn is_match(&self, input: &T) -> bool;
}

pub struct RegexMatcher {
    regex: Regex,
}

impl Match<[&str]> for RegexMatcher {
    fn is_match(&self, input: &[&str]) -> bool {
        input.iter().any(|x| self.regex.is_match(x))
    }
}

impl Match<str> for RegexMatcher {
    fn is_match(&self, input: &str) -> bool {
        self.regex.is_match(input)
    }
}

impl RegexMatcher {
    pub fn new(regex: Regex) -> Self {
        RegexMatcher { regex }
    }
}

pub struct StringMatcher {
    string: String,
}

impl Match<[&str]> for StringMatcher {
    fn is_match(&self, input: &[&str]) -> bool {
        input.iter().any(|x| *x == self.string)
    }
}

impl Match<str> for StringMatcher {
    fn is_match(&self, input: &str) -> bool {
        input == self.string
    }
}

impl StringMatcher {
    pub fn new(string: String) -> Self {
        StringMatcher { string }
    }
}

pub struct Quantifier {
    min: usize,
    max: usize,
}

impl Quantifier {
    pub fn new(min: usize, max: usize) -> Self {
        assert!(max >= min);
        Quantifier { min, max }
    }
}

pub trait Atom {
    fn is_match<'a>(&self, input: &Token<'a>) -> bool;
}

pub struct TrueAtom {}

impl Atom for TrueAtom {
    fn is_match(&self, _input: &Token) -> bool {
        true
    }
}

impl TrueAtom {
    pub fn new() -> Self {
        TrueAtom {}
    }
}

impl Default for TrueAtom {
    fn default() -> Self {
        TrueAtom::new()
    }
}

pub struct MatchAtom<O: ?Sized, M: Match<O>, A: for<'a> Fn(&'a Token<'a>) -> &'a O> {
    matcher: M,
    access: A,
    phantom: std::marker::PhantomData<O>,
}

impl<O: ?Sized, M: Match<O>, A: for<'a> Fn(&'a Token<'a>) -> &'a O> Atom for MatchAtom<O, M, A> {
    fn is_match(&self, input: &Token) -> bool {
        self.matcher.is_match((self.access)(input))
    }
}

impl<O: ?Sized, M: Match<O>, A: for<'a> Fn(&'a Token<'a>) -> &'a O> MatchAtom<O, M, A> {
    pub fn new(matcher: M, access: A) -> Self {
        MatchAtom {
            matcher,
            access,
            phantom: std::marker::PhantomData,
        }
    }
}

#[derive(Debug)]
pub struct Group<'a> {
    pub char_start: usize,
    pub char_end: usize,
    pub tokens: Vec<&'a Token<'a>>,
}

impl<'a> Group<'a> {
    fn empty() -> Self {
        Group {
            char_start: 0,
            char_end: 0,
            tokens: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct MatchGraph<'a> {
    pub groups: Vec<Group<'a>>,
}

pub struct Composition {
    atoms: Vec<(Box<dyn Atom>, Quantifier)>,
}

impl Composition {
    pub fn new(atoms: Vec<(Box<dyn Atom>, Quantifier)>) -> Self {
        Composition { atoms }
    }

    fn next_can_match(&self, item: &Token, index: usize) -> bool {
        if index == self.atoms.len() - 1 {
            return true;
        }

        let next_required_pos = match self.atoms[index + 1..].iter().position(|x| x.1.min > 0) {
            Some(pos) => index + 1 + pos + 1,
            None => self.atoms.len(),
        };

        self.atoms[index + 1..next_required_pos]
            .iter()
            .any(|x| x.0.is_match(item))
    }

    pub fn apply<'a>(&self, tokens: &[&'a Token<'a>]) -> Option<MatchGraph<'a>> {
        let mut position = 0;

        let mut cur_count = 0;
        let mut cur_atom_idx = 0;

        let mut graph = MatchGraph {
            groups: self
                .atoms
                .iter()
                .map(|_| Group::empty())
                .collect::<Vec<_>>(),
        };

        let maybe_graph = loop {
            if cur_atom_idx >= self.atoms.len() {
                break Some(graph);
            }

            let atom = &self.atoms[cur_atom_idx];

            if cur_count >= atom.1.max {
                cur_atom_idx += 1;
                cur_count = 0;
                if cur_atom_idx >= self.atoms.len() {
                    break Some(graph);
                }
                continue;
            }

            if position >= tokens.len() {
                break None;
            }

            if cur_count >= atom.1.min && self.next_can_match(&tokens[position], cur_atom_idx) {
                cur_atom_idx += 1;
                cur_count = 0;
            } else if atom.0.is_match(tokens[position]) {
                graph.groups[cur_atom_idx].tokens.push(tokens[position]);

                position += 1;
                cur_count += 1;
            } else {
                break None;
            }
        };

        if let Some(mut graph) = maybe_graph {
            let mut start = graph
                .groups
                .iter()
                .find_map(|x| {
                    if x.tokens.is_empty() {
                        None
                    } else {
                        Some(x.tokens[0].char_span.0)
                    }
                })
                .expect("graph must contain at least one token");

            for group in &mut graph.groups {
                if !group.tokens.is_empty() {
                    group.char_start = group.tokens[0].char_span.0;
                    start = group.tokens[group.tokens.len() - 1].char_span.1;
                } else {
                    group.char_start = start;
                }
            }

            let mut end = graph
                .groups
                .iter()
                .rev()
                .find_map(|x| {
                    if x.tokens.is_empty() {
                        None
                    } else {
                        Some(x.tokens[0].char_span.1)
                    }
                })
                .expect("graph must contain at least one token");

            for group in &mut graph.groups.iter_mut().rev() {
                if !group.tokens.is_empty() {
                    group.char_end = group.tokens[group.tokens.len() - 1].char_span.1;
                    end = group.tokens[0].char_span.0;
                } else {
                    group.char_end = end;
                }
            }

            Some(graph)
        } else {
            None
        }
    }
}