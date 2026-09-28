//! Benchmark for the nlprule pipeline: binary load time and per-sentence
//! rule-application latency, compared against the LanguageTool HTTP server
//! by `build/bench_server.py`.

use nlprule::{Rules, Tokenizer};
use std::io::{self, BufRead};

fn rss_mb() -> f64 {
    if let Ok(statm) = std::fs::read_to_string("/proc/self/statm") {
        let pages: Vec<&str> = statm.split_whitespace().collect();
        if pages.len() >= 2 {
            let kb: f64 = pages[1].parse().unwrap_or(0.0);
            return kb * 4096.0 / 1024.0 / 1024.0;
        }
    }
    0.0
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let tokenizer_path = &args[1];
    let rules_path = &args[2];
    let repeats: usize = args
        .get(3)
        .and_then(|x| x.parse().ok())
        .unwrap_or(1);

    let sentences: Vec<String> = io::stdin()
        .lock()
        .lines()
        .map(|l| l.unwrap())
        .filter(|l| !l.trim().is_empty())
        .collect();

    let t0 = std::time::Instant::now();
    let tokenizer = Tokenizer::new(tokenizer_path).expect("tokenizer");
    let rules = Rules::new(rules_path).expect("rules");
    let load_ms = t0.elapsed().as_millis() as u64;
    let load_rss = rss_mb();

    // warmup (first sentence pays lazy regex compilation etc.)
    for text in sentences.iter().take(10) {
        for sentence in tokenizer.pipe(text) {
            let _ = rules.apply(&sentence).len();
        }
    }

    let mut latencies_us: Vec<u128> = Vec::with_capacity(sentences.len() * repeats);
    let mut matches_total = 0usize;
    let t1 = std::time::Instant::now();
    for _ in 0..repeats {
        for text in &sentences {
            let start = std::time::Instant::now();
            let mut hits = 0usize;
            for sentence in tokenizer.pipe(text) {
                hits += rules.apply(&sentence).len();
            }
            matches_total += hits;
            latencies_us.push(start.elapsed().as_micros());
        }
    }
    let wall_ms = t1.elapsed().as_millis() as u64;

    latencies_us.sort_unstable();
    let q = |p: f64| -> u64 {
        if latencies_us.is_empty() {
            return 0;
        }
        let idx = ((latencies_us.len() as f64 - 1.0) * p).round() as usize;
        (latencies_us[idx] as f64 / 1000.0).round() as u64
    };

    println!(
        "{}",
        serde_json::json!({
            "sentences": sentences.len(),
            "repeats": repeats,
            "load_ms": load_ms,
            "load_rss_mb": (load_rss * 10.0).round() / 10.0,
            "rss_mb": (rss_mb() * 10.0).round() / 10.0,
            "wall_ms": wall_ms,
            "throughput_sps": ((sentences.len() * repeats) as f64 / (wall_ms as f64 / 1000.0)).round(),
            "mean_ms": (latencies_us.iter().sum::<u128>() as f64
                / latencies_us.len() as f64 / 1000.0 * 10.0).round() / 10.0,
            "p50_ms": q(0.5),
            "p95_ms": q(0.95),
            "matches": matches_total,
        })
    );
}
