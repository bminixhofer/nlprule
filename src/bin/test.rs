use clap::Clap;
use nlprule::rule::Rule;
use std::collections::HashMap;
use std::convert::TryFrom;

#[derive(Clap)]
#[clap(
    version = "1.0",
    author = "Benjamin Minixhofer <bminixhofer@gmail.com>"
)]
struct Opts {
    ids: Vec<String>,
}

fn main() {
    env_logger::init();
    let opts = Opts::parse();
    let ids = opts.ids.iter().map(|x| x.as_str()).collect::<Vec<_>>();

    let rules = nlprule::structure::read_rules("data/grammar.canonic.xml");
    let rules: Vec<_> = rules
        .into_iter()
        .filter(|x| match x {
            Ok((_, id)) => ids.is_empty() || ids.contains(&id.as_str()),
            Err(_) => true,
        })
        .collect();

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

    println!(
        "Top errors: {:#?}",
        &errors[..std::cmp::min(10, errors.len())]
    );
    println!("Parsed rules: {}", rules.len());

    let rules: Vec<_> = rules
        .into_iter()
        .filter_map(
            |(rule_structure, id)| match Rule::try_from(rule_structure) {
                Ok(mut rule) => {
                    rule.set_id(id);
                    Some(rule)
                }
                Err(_) => None,
            },
        )
        .collect();

    println!("Runnable rules: {}", rules.len());
    println!(
        "Rules passing tests: {}",
        rules
            .iter()
            .fold(0, |count, rule| count + rule.test() as usize)
    );
}
