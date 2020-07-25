use seqrule::{Atom, Quantifier, Rule};
use serde::Serialize;

#[derive(Serialize)]
struct MyData {
    name: String,
}

fn main() {
    let sequence = vec!["a", "b", "c", "c", "c", "d", "e"];
    let rule = Rule::new(vec![
        Atom::new(Box::new("a"), Quantifier::new(1, 1)),
        Atom::new(Box::new("x"), Quantifier::new(0, 5)),
        Atom::new(Box::new("y"), Quantifier::new(0, 1)),
        Atom::new(Box::new("b"), Quantifier::new(2, 2)),
    ]);

    println!("{:#?}", rule.apply(&sequence));
}
