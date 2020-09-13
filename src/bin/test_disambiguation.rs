use clap::Clap;
use nlprule::rule::DisambiguationRule;
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

    let rules = nlprule::structure::read_disambiguation_rules(format!(
        "data/disambiguation.{}.canonic.xml",
        std::env::var("RULE_LANG").unwrap()
    ));
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
            Ok(rule) => {
                if errors.is_empty() {
                    Some(rule)
                } else {
                    None
                }
            }
            Err(err) => {
                if errors.is_empty() {
                    println!("First error: {}", err);
                }

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

    println!("Errors: {:#?}", &errors);
    println!("Parsed rules: {}", rules.len());

    let rules: Vec<_> = rules
        .into_iter()
        .filter_map(
            |(rule_structure, id)| match DisambiguationRule::try_from(rule_structure) {
                Ok(mut rule) => {
                    rule.set_id(id);
                    Some(rule)
                }
                Err(_) => None,
            },
        )
        .collect();

    println!("Last ID: {}", rules[rules.len() - 1].id);
    println!("Runnable rules: {}", rules.len());

    let mut passes = 0;

    for rule in rules {
        if rule.test() {
            passes += 1;
        } else {
            break;
        }
    }

    println!("Rules passing tests: {}", passes);
}
