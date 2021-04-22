use criterion::{black_box, criterion_group, criterion_main, Criterion};
use nlprule::{Rules, Tokenizer};
use std::time::Duration;

fn parse_tokenizer(c: &mut Criterion) {
    c.bench_function("load tokenizer", |b| {
        b.iter(|| Tokenizer::new(black_box("../storage/en_tokenizer.bin")).unwrap())
    });
}

fn parse_rules(c: &mut Criterion) {
    c.bench_function("load rules", |b| {
        b.iter(|| Rules::new(black_box("../storage/en_rules.bin")).unwrap())
    });
}

fn no_warmup_criterion() -> Criterion {
    Criterion::default()
        .sample_size(20)
        .warm_up_time(Duration::from_nanos(1))
}

criterion_group!(
name = parse;
config = no_warmup_criterion();
targets =
    parse_rules,
    parse_tokenizer,
);

criterion_main!(parse);
