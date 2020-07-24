use serde::de::Deserialize;
use serde_value::Value;

pub struct KeyMatcher<'a, T> {
    matcher: Box<dyn Match<T>>,
    key: Vec<Accessor<'a>>,
}

impl<'a, 'de, T: Deserialize<'de>> Match<Value> for KeyMatcher<'a, T> {
    fn is_match(&self, other: &Value) -> bool {
        self.matcher.is_match(&access(other, &self.key))
    }
}

pub trait Match<T> {
    fn is_match(&self, other: &T) -> bool;

    fn with_key<'a>(self, key: Vec<Accessor<'a>>) -> KeyMatcher<'a, T>
    where
        Self: 'static + Sized,
    {
        KeyMatcher {
            matcher: Box::new(self),
            key,
        }
    }
}

impl<T: PartialEq> Match<T> for T {
    fn is_match(&self, other: &T) -> bool {
        other == self
    }
}

pub enum Accessor<'a> {
    Int(usize),
    String(&'a str),
}

fn access<'de, T: Deserialize<'de>>(value: &Value, key: &[Accessor]) -> T {
    if key.is_empty() {
        return T::deserialize(value.clone()).unwrap();
    }

    match &key[0] {
        Accessor::Int(position) => {
            if let Value::Seq(values) = value {
                access(&values[*position], &key[1..])
            } else {
                unimplemented!()
            }
        }
        Accessor::String(name) => {
            let name = Value::String(name.to_string());

            if let Value::Map(values) = value {
                access(&values.get(&name).unwrap(), &key[1..])
            } else {
                unimplemented!()
            }
        }
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

pub struct Rule<T> {
    atoms: Vec<(Box<dyn Match<T>>, Quantifier)>,
}

impl<T> Rule<T> {
    pub fn new(atoms: Vec<(Box<dyn Match<T>>, Quantifier)>) -> Self {
        Rule { atoms }
    }

    fn next_can_match(&self, item: &T, index: usize) -> bool {
        if index == self.atoms.len() - 1 {
            return true;
        }

        match self.atoms[index + 1..].iter().position(|x| x.1.min > 0) {
            Some(position) => self.atoms[index + 1 + position].0.is_match(item),
            None => true,
        }
    }

    pub fn apply(&self, sequence: &[T]) -> bool {
        let mut position = 0;

        let mut cur_count = 0;
        let mut cur_atom_idx = 0;

        loop {
            if cur_atom_idx >= self.atoms.len() {
                break true;
            }

            let matcher = &self.atoms[cur_atom_idx].0;
            let quantifier = &self.atoms[cur_atom_idx].1;

            if cur_count >= quantifier.max {
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

            if cur_count >= quantifier.min && self.next_can_match(&sequence[position], cur_atom_idx)
            {
                cur_atom_idx += 1;
                cur_count = 0;
            } else if matcher.is_match(&sequence[position]) {
                position += 1;
                cur_count += 1;
            } else {
                break false;
            }
        }
    }
}

pub struct RuleSet<T> {
    rules: Vec<Rule<T>>,
}

impl<T> RuleSet<T> {
    pub fn apply(&self, sequence: &[T]) {
        for rule in &self.rules {
            rule.apply(sequence);
        }
    }
}
