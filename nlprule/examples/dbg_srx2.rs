fn main() {
    let args: Vec<String> = std::env::args().collect();
    let srx: srx::SRX = std::fs::read_to_string(&args[1]).unwrap().parse().unwrap();
    let lang = srx::Language(args[2].clone());
    if let Some(errs) = srx.errors().get(&lang) {
        for e in errs {
            println!("ERR: {}", e);
        }
    } else {
        println!("no errors");
    }
}
