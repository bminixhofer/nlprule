use crate::Token;

pub trait Match<T: ?Sized> {
    fn is_match(&self, input: &T) -> bool;
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

        match self.atoms[index + 1..].iter().position(|x| x.1.min > 0) {
            Some(position) => self.atoms[index + 1 + position].0.is_match(item),
            None => true,
        }
    }

    pub fn apply(&self, sequence: &[Token]) -> bool {
        let mut position = 0;

        let mut cur_count = 0;
        let mut cur_atom_idx = 0;

        loop {
            if cur_atom_idx >= self.atoms.len() {
                break true;
            }

            let atom = &self.atoms[cur_atom_idx];

            if cur_count >= atom.1.max {
                cur_atom_idx += 1;
                cur_count = 0;
                if cur_atom_idx >= self.atoms.len() {
                    break true;
                }
                continue;
            }

            if position >= sequence.len() {
                break false;
            }

            if cur_count >= atom.1.min && self.next_can_match(&sequence[position], cur_atom_idx) {
                cur_atom_idx += 1;
                cur_count = 0;
            } else if atom.0.is_match(&sequence[position]) {
                position += 1;
                cur_count += 1;
            } else {
                break false;
            }
        }
    }
}
