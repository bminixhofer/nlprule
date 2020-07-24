use seqrule::{Match, Quantifier, Rule};
use serde::Serialize;

#[derive(Serialize)]
struct MyData {
    name: String,
}

fn main() {
    let sequence = vec!["a", "b", "c", "c", "c", "d", "e"];
    let rule = Rule::new(vec![
        (Box::new("a") as Box<dyn Match<_>>, Quantifier::new(1, 1)),
        (Box::new("x") as Box<dyn Match<_>>, Quantifier::new(0, 5)),
        (Box::new("y") as Box<dyn Match<_>>, Quantifier::new(0, 1)),
        (Box::new("b") as Box<dyn Match<_>>, Quantifier::new(2, 2)),
    ]);

    println!("{:#?}", rule.apply(&sequence));
}
