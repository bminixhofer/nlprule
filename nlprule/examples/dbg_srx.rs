fn main() {
    let args: Vec<String> = std::env::args().collect();
    let text = &args[2];
    let srx: srx::SRX = std::fs::read_to_string(&args[1]).unwrap().parse().unwrap();
    let rules = srx.language_rules(args[3].clone());
    let splits = rules.split_ranges(text);
    for s in splits {
        println!("{:?}", &text[s]);
    }
}
