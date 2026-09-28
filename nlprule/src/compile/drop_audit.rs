//! Fast XML drop audit (dev tool): runs the structure pipeline over every
//! data2 grammar.xml and prints which rules fail to deserialize or fail
//! regex extraction — without the expensive tagger/synthesizer work.
//!
//! Run with:
//!   RUST_LOG=warn cargo test --release -p nlprule drop_audit -- --nocapture

use super::structure;

#[test]
fn drop_audit() {
    let root = match std::env::var("DROP_AUDIT_ROOT") {
        Ok(x) => std::env::temp_dir().join(x).display().to_string(), // unused branch guard
        Err(_) => "/home/alex/Documentos/Github/nlprule/data2".to_string(),
    };
    let root = std::env::var("DROP_AUDIT_ROOT")
        .unwrap_or_else(|_| "/home/alex/Documentos/Github/nlprule/data2".to_string());

    struct StderrLog;
    impl log::Log for StderrLog {
        fn enabled(&self, md: &log::Metadata) -> bool {
            md.level() <= log::Level::Warn
        }
        fn log(&self, record: &log::Record) {
            if self.enabled(record.metadata()) {
                eprintln!("[{}] {}", record.level(), record.args());
            }
        }
        fn flush(&self) {}
    }
    let _ = log::set_boxed_logger(Box::new(StderrLog));
    log::set_max_level(log::LevelFilter::Warn);

    let mut langs: Vec<String> = std::fs::read_dir(&root)
        .expect("read data2 root")
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.is_dir() && p.join("grammar.xml").exists())
        .map(|p| p.file_name().unwrap().to_string_lossy().to_string())
        .collect();
    langs.sort();

    let mut total_err = 0usize;
    let mut total_regex = 0usize;

    for lang in &langs {
        let path = std::path::Path::new(&root).join(lang).join("grammar.xml");
        let (rules, regex_defs) = structure::read_rules(&path);
        let n_err = rules.iter().filter(|r| r.is_err()).count();
        total_err += n_err;
        total_regex += regex_defs.len();

        if let Ok(find) = std::env::var("DROP_AUDIT_FIND") {
            for r in rules.iter().filter_map(|r| r.as_ref().ok()) {
                let has_group = r
                    .1
                    .as_ref()
                    .map_or(false, |g| g.id.contains(&find));
                if r.0.id.as_deref().map_or(false, |i| i.contains(find.as_str()))
                    || has_group
                {
                    eprintln!(
                        "   FIND {lang}: rule id={:?} group={:?} name={:?}",
                        r.0.id,
                        r.1.as_ref().map(|g| g.id.clone()),
                        r.0.name
                    );
                }
            }
        }

        eprintln!(
            "== {lang}: {} ok, {n_err} ERR, {} regex defs",
            rules.len() - n_err,
            regex_defs.len()
        );
        for e in rules.iter().filter(|r| r.is_err()) {
            if let Err(x) = e {
                eprintln!("   ERR {x}");
            }
        }
    }

    eprintln!("TOTAL deserialize errors: {total_err}, regex defs: {total_regex}");
}



