use nlprule::structure::RuleStructure;
use std::collections::HashMap;

struct Rule {}

impl From<RuleStructure> for Rule {
    fn from(structure: RuleStructure) -> Rule {
        unimplemented!()
    }
}

fn main() {
    let rules = nlprule::structure::read_rules("data/grammar.canonic.xml");
    let mut errors: HashMap<String, usize> = HashMap::new();

    let rules = rules
        .into_iter()
        .filter_map(|x| match x {
            Ok(rule) => Some(rule),
            Err(err) => {
                errors
                    .entry(format!("{}", err))
                    .and_modify(|x| *x += 1)
                    .or_insert(1);
                None
            }
        })
        .collect::<Vec<_>>();

    let mut errors: Vec<(String, usize)> = errors.into_iter().collect();
    errors.sort_by_key(|x| -(x.1 as i32));

    println!("Top errors: {:#?}", &errors[..5]);
    println!("{:#?}", rules[0]);
}
