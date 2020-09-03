use crate::tokenizer::Token;
use regex::Regex;
use std::collections::HashMap;

pub trait Match<T: ?Sized>: Send + Sync {
    fn is_match(&self, input: &T) -> bool;
}

pub struct RegexMatcher {
    regex: Regex,
}

impl Match<[String]> for RegexMatcher {
    fn is_match(&self, input: &[String]) -> bool {
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

impl Match<[String]> for StringMatcher {
    fn is_match(&self, input: &[String]) -> bool {
        input.iter().any(|x| *x == self.string)
    }
}

impl Match<Option<String>> for StringMatcher {
    fn is_match(&self, input: &Option<String>) -> bool {
        input.as_ref().map(|x| *x == self.string).unwrap_or(false)
    }
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

pub struct GenericMatcher<T> {
    value: T,
}

impl<T: Eq + Send + Sync> Match<T> for GenericMatcher<T> {
    fn is_match(&self, input: &T) -> bool {
        input == &self.value
    }
}

impl<T> GenericMatcher<T> {
    pub fn new(value: T) -> Self {
        GenericMatcher { value }
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

pub trait Atom: Send + Sync {
    fn is_match(&self, input: &[&Token], position: usize) -> bool;
}

pub struct TrueAtom {}

impl Atom for TrueAtom {
    fn is_match(&self, _input: &[&Token], _position: usize) -> bool {
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

pub struct AndAtom {
    atoms: Vec<Box<dyn Atom>>,
}

impl AndAtom {
    pub fn new(atoms: Vec<Box<dyn Atom>>) -> Self {
        AndAtom { atoms }
    }
}

impl Atom for AndAtom {
    fn is_match(&self, input: &[&Token], position: usize) -> bool {
        self.atoms.iter().all(|x| x.is_match(input, position))
    }
}

pub struct OrAtom {
    atoms: Vec<Box<dyn Atom>>,
}

impl OrAtom {
    pub fn new(atoms: Vec<Box<dyn Atom>>) -> Self {
        OrAtom { atoms }
    }
}

impl Atom for OrAtom {
    fn is_match(&self, input: &[&Token], position: usize) -> bool {
        self.atoms.iter().any(|x| x.is_match(input, position))
    }
}

pub struct NotAtom {
    atom: Box<dyn Atom>,
}

impl NotAtom {
    pub fn new(atom: Box<dyn Atom>) -> Self {
        NotAtom { atom }
    }
}

impl Atom for NotAtom {
    fn is_match(&self, input: &[&Token], position: usize) -> bool {
        !self.atom.is_match(input, position)
    }
}

pub struct OffsetAtom {
    atom: Box<dyn Atom>,
    offset: isize,
}

impl Atom for OffsetAtom {
    fn is_match(&self, input: &[&Token], position: usize) -> bool {
        let new_position = position as isize + self.offset;

        if new_position < 0 || (new_position as usize) >= input.len() {
            false
        } else {
            self.atom.is_match(input, new_position as usize)
        }
    }
}

impl OffsetAtom {
    pub fn new(atom: Box<dyn Atom>, offset: isize) -> Self {
        OffsetAtom { atom, offset }
    }
}

pub struct MatchAtom<O: ?Sized + Send + Sync, M: Match<O>, A: for<'a> Fn(&'a Token) -> &'a O> {
    matcher: M,
    access: A,
    phantom: std::marker::PhantomData<O>,
}

impl<O: ?Sized + Send + Sync, M: Match<O>, A: for<'a> Fn(&'a Token) -> &'a O + Sync + Send> Atom
    for MatchAtom<O, M, A>
{
    fn is_match(&self, input: &[&Token], position: usize) -> bool {
        self.matcher.is_match((self.access)(input[position]))
    }
}

impl<O: ?Sized + Send + Sync, M: Match<O>, A: for<'a> Fn(&'a Token) -> &'a O> MatchAtom<O, M, A> {
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
    pub tokens: Vec<&'a Token>,
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
    groups: Vec<Group<'a>>,
    id_to_idx: HashMap<usize, usize>,
}

impl<'a> MatchGraph<'a> {
    fn empty_from_parts(parts: &[Part]) -> Self {
        let mut groups = Vec::new();
        let mut id_to_idx = HashMap::new();
        let mut current_id = 0;

        for (i, part) in parts.iter().enumerate() {
            if part.visible {
                id_to_idx.insert(current_id, i);
                current_id += 1;
            }
            groups.push(Group::empty());
        }

        MatchGraph { groups, id_to_idx }
    }

    pub fn by_index(&self, index: usize) -> &Group<'a> {
        &self.groups[index]
    }

    pub fn by_id(&self, id: usize) -> Option<&Group<'a>> {
        Some(&self.groups[*self.id_to_idx.get(&id)?])
    }

    pub fn is_empty(&self) -> bool {
        self.groups.is_empty()
    }

    pub fn len(&self) -> usize {
        self.groups.len()
    }
}

pub struct Part {
    pub atom: Box<dyn Atom>,
    pub quantifier: Quantifier,
    pub visible: bool,
}

impl Part {
    pub fn new(atom: Box<dyn Atom>, quantifier: Quantifier, visible: bool) -> Self {
        Part {
            atom,
            quantifier,
            visible,
        }
    }
}

pub struct Composition {
    pub parts: Vec<Part>,
}

impl Composition {
    pub fn new(parts: Vec<Part>) -> Self {
        Composition { parts }
    }

    fn next_can_match(&self, tokens: &[&Token], position: usize, index: usize) -> bool {
        if index == self.parts.len() - 1 {
            return false;
        }

        let next_required_pos = match self.parts[index + 1..]
            .iter()
            .position(|x| x.quantifier.min > 0)
        {
            Some(pos) => index + 1 + pos + 1,
            None => self.parts.len(),
        };

        self.parts[index + 1..next_required_pos]
            .iter()
            .any(|x| x.atom.is_match(tokens, position))
    }

    pub fn apply<'a>(&self, tokens: &[&'a Token], start: usize) -> Option<MatchGraph<'a>> {
        let mut position = start;

        let mut cur_count = 0;
        let mut cur_atom_idx = 0;

        // NB: if this impacts performance: could be moved to constructor, then cloned (but maybe lifetime issue)
        let mut graph = MatchGraph::empty_from_parts(&self.parts);

        let mut is_match = loop {
            if cur_atom_idx >= self.parts.len() {
                break true;
            }

            let part = &self.parts[cur_atom_idx];

            if cur_count >= part.quantifier.max {
                cur_atom_idx += 1;
                cur_count = 0;
                if cur_atom_idx >= self.parts.len() {
                    break false;
                }
                continue;
            }

            if position >= tokens.len() {
                break false;
            }

            if cur_count >= part.quantifier.min
                && self.next_can_match(&tokens, position, cur_atom_idx)
            {
                cur_atom_idx += 1;
                cur_count = 0;
            } else if part.atom.is_match(tokens, position) {
                graph.groups[cur_atom_idx].tokens.push(tokens[position]);

                position += 1;
                cur_count += 1;
            } else {
                break false;
            }
        };

        // NB: maybe better way to solve this (probably more logically well-defined matching)
        is_match = is_match
            || self.parts[cur_atom_idx..]
                .iter()
                .all(|x| x.quantifier.min == 0);

        if is_match {
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

            for group in graph.groups.iter_mut() {
                if !group.tokens.is_empty() {
                    group.char_start = group.tokens[0].char_span.0;
                    group.char_end = group.tokens[group.tokens.len() - 1].char_span.1;
                    start = group.tokens[group.tokens.len() - 1].char_span.1;
                } else {
                    group.char_end = start;
                }
            }

            for group in graph.groups.iter_mut().rev() {
                if !group.tokens.is_empty() {
                    end = group.tokens[group.tokens.len() - 1].char_span.0;
                } else {
                    group.char_start = end;
                }
            }

            Some(graph)
        } else {
            None
        }
    }
}
