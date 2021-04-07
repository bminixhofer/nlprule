use criterion::{black_box, criterion_group, criterion_main, Criterion};
use nlprule::{Rules, Tokenizer};

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("load tokenizer", |b| {
        b.iter(|| Tokenizer::new(black_box("../storage/en_tokenizer.bin")).unwrap())
    });

    c.bench_function("load rules", |b| {
        b.iter(|| Rules::new(black_box("../storage/en_rules.bin")).unwrap())
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
