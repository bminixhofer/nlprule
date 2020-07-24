use serde::de::Deserialize;
use serde_value::Value;

pub struct KeyMatcher<'a, 'de, T: Deserialize<'de>> {
    matcher: Box<dyn Match<'de, T>>,
    key: Vec<Accessor<'a>>,
}

impl<'a, 'de, T: Deserialize<'de>> Match<'de, Value> for KeyMatcher<'a, 'de, T> {
    fn is_match(&self, other: &Value) -> bool {
        self.matcher.is_match(&access(other, &self.key))
    }
}

pub trait Match<'de, T: Deserialize<'de>> {
    fn is_match(&self, other: &T) -> bool;

    fn with_key<'a>(self, key: Vec<Accessor<'a>>) -> KeyMatcher<'a, 'de, T>
    where
        Self: 'static + Sized,
    {
        KeyMatcher {
            matcher: Box::new(self),
            key,
        }
    }
}

impl<'de, T: Deserialize<'de> + PartialEq> Match<'de, T> for T {
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

pub struct Rule<'de, T> {
    atoms: Vec<(Box<dyn Match<'de, T>>, Quantifier)>,
}

impl<'de, T: Deserialize<'de>> Rule<'de, T> {
    pub fn new(atoms: Vec<(Box<dyn Match<'de, T>>, Quantifier)>) -> Self {
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

pub struct RuleSet<'de, T> {
    rules: Vec<Rule<'de, T>>,
}

impl<'de, T: Deserialize<'de>> RuleSet<'de, T> {
    pub fn apply(&self, sequence: &[T]) {
        for rule in &self.rules {
            rule.apply(sequence);
        }
    }
}
