use nlprule::Tokenizer;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let tokenizer = Tokenizer::new(&args[1]).unwrap();
    let text = &args[2];
    for sentence in tokenizer.pipe(text) {
        println!("sentence: {:?}", sentence.span().byte());
        for token in sentence.iter() {
            println!(
                "  word={:?} span={:?} chunks={:?}",
                token.word(),
                token.span().byte(),
                token.chunks()
            );
        }
    }
}
