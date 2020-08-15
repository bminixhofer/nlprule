use super::{Token, INFLECTER};

pub fn adapt_tokens_en(mut tokens: Vec<Token>) -> Vec<Token> {
    for i in 0..tokens.len() {
        if tokens.get(i - 1).map(|x| x.text == "'").unwrap_or(false) {
            let to_inflect = match tokens[i].text {
                "m" => Some(vec!["am"]),
                "ll" => Some(vec!["will"]),
                "ve" => Some(vec!["have"]),
                "s" => Some(vec!["is"]),
                "d" => Some(vec!["had", "would"]),
                "t" => Some(vec!["not"]),
                _ => None,
            };

            if let Some(to_inflect) = to_inflect {
                let (inflections, lower_inflections) = to_inflect
                    .iter()
                    .map(|x| INFLECTER.get_inflections(x))
                    .fold((Vec::new(), Vec::new()), |mut acc, x| {
                        acc.0.extend(x.0.into_iter());
                        acc.1.extend(x.1.into_iter());

                        (acc.0, acc.1)
                    });

                tokens[i].inflections = inflections;
                tokens[i].lower_inflections = lower_inflections;
            }
        }
    }

    tokens
}

pub fn adapt_tokens(tokens: Vec<Token>) -> Vec<Token> {
    match std::env::var("RULE_LANG").unwrap().as_str() {
        "en" => adapt_tokens_en(tokens),
        _ => tokens,
    }
}
