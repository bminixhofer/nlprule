//! Converts OpenNLP maxent model dumps (from build/dump_opennlp/DumpOpenNlp.java)
//! into nlprule's `chunker.json` format.
//!
//! Usage: opennlp_to_chunker token.dump pos.dump chunk.dump out.json

use serde_json::json;

struct DumpedModel {
    outcome_labels: Vec<String>,
    // predicate string -> [(outcome idx, weight)]
    predicates: Vec<(String, Vec<(usize, f32)>)>,
}

fn read_dump(path: &str) -> DumpedModel {
    let content = std::fs::read_to_string(path).expect("dump file");
    let mut lines = content.lines();
    let header = lines.next().expect("header");
    assert!(header.starts_with("outcomes\t"));
    let outcome_labels: Vec<String> = header["outcomes\t".len()..]
        .split('\t')
        .map(|x| x.to_string())
        .collect();

    let mut predicates = Vec::new();
    for line in lines {
        if line.is_empty() {
            continue;
        }
        let mut parts = line.splitn(2, '\t');
        let b64 = parts.next().unwrap();
        let weights_str = parts.next().unwrap();
        let pred = String::from_utf8(
            base64_decode(b64.as_bytes()).expect("valid base64"),
        )
        .expect("valid utf8");
        let weights: Vec<(usize, f32)> = weights_str
            .split(' ')
            .map(|w| {
                let mut kv = w.splitn(2, ':');
                (
                    kv.next().unwrap().parse().unwrap(),
                    kv.next().unwrap().parse().unwrap(),
                )
            })
            .collect();
        predicates.push((pred, weights));
    }

    DumpedModel {
        outcome_labels,
        predicates,
    }
}

fn base64_decode(input: &[u8]) -> Result<Vec<u8>, String> {
    const TABLE: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    let mut lookup = [255u8; 256];
    for (i, &c) in TABLE.iter().enumerate() {
        lookup[c as usize] = i as u8;
    }
    let mut out = Vec::with_capacity(input.len() / 4 * 3);
    let mut buf: u32 = 0;
    let mut bits = 0u32;
    for &c in input {
        if c == b'=' {
            break;
        }
        let v = lookup[c as usize];
        if v == 255 {
            continue;
        }
        buf = (buf << 6) | v as u32;
        bits += 6;
        if bits >= 8 {
            bits -= 8;
            out.push((buf >> bits) as u8);
        }
    }
    Ok(out)
}

fn model_to_json(model: &DumpedModel) -> serde_json::Map<String, serde_json::Value> {
    let mut pmap = serde_json::Map::new();
    for (pred, weights) in &model.predicates {
        pmap.insert(
            pred.clone(),
            json!({
                "parameters": weights.iter().map(|(_, w)| w).collect::<Vec<_>>(),
                "outcomes": weights.iter().map(|(o, _)| o).collect::<Vec<_>>(),
            }),
        );
    }

    let mut out = serde_json::Map::new();
    out.insert(
        "outcome_labels".into(),
        json!(model.outcome_labels),
    );
    out.insert("pmap".into(), serde_json::Value::Object(pmap));
    out
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let token = read_dump(&args[1]);
    let pos = read_dump(&args[2]);
    let chunk = read_dump(&args[3]);

    let out = json!({
        "chunk_model": model_to_json(&chunk),
        "pos_model": model_to_json(&pos),
        "pos_tagdict": {},
        "token_model": model_to_json(&token),
    });

    std::fs::write(
        &args[4],
        serde_json::to_string(&out).expect("serialize"),
    )
    .expect("write");
    eprintln!(
        "wrote {} (token preds: {}, pos preds: {}, chunk preds: {})",
        args[4],
        token.predicates.len(),
        pos.predicates.len(),
        chunk.predicates.len()
    );
}
