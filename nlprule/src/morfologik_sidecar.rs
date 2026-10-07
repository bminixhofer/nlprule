//! Persistent Java sidecar giving the EXACT morfologik speller suggestions
//! LT computes, for languages where the port's FST distance metric cannot
//! reproduce morfologik's encoded-automaton candidate set (el: SUFFIX-encoded
//! dictionary — LT's list mixes candidates the port ranks as distance 2+).
//!
//! Mirrors the libhunspell FFI approach: delegate to LT's own engine with
//! LT's own data; fall back to the FST path when java/paths are missing.
//!
//! Protocol (scripts/ElSuggest.java): one word per line in, one JSON array
//! of the first N suggestions in morfologik order out.

use std::io::{BufRead, BufReader, Write};
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};
use std::sync::Mutex;

pub struct MorfologikSidecar {
    child: Child,
    stdin: ChildStdin,
    stdout: BufReader<ChildStdout>,
}

static SIDECAR: Mutex<Option<Option<MorfologikSidecar>>> = Mutex::new(None);

fn find_java() -> Option<String> {
    for candidate in [
        "/home/alex/apps/jdk-17.0.20.1+1/bin/java",
        "/usr/bin/java",
        "java",
    ] {
        if candidate == "java" || std::path::Path::new(candidate).exists() {
            return Some(candidate.to_string());
        }
    }
    None
}

fn home_path(rel: &str) -> Option<String> {
    let home = std::env::var("HOME").ok()?;
    let p = format!("{home}/{rel}");
    std::path::Path::new(&p).exists().then_some(p)
}

fn sidecar_command(lang: &str) -> Option<(String, Vec<String>)> {
    // (dict, info, cap) per language; only languages whose LT suggestions
    // provably diverge from the port's FST metric are wired (el, probed
    // live: LT mixes char-distance tiers through its SUFFIX-encoded
    // automaton).
    let (dict, info) = match lang {
        "el" => (
            home_path("apps/lt/LanguageTool-6.6/org/languagetool/resource/el/hunspell/el_GR.dict")?,
            home_path("apps/lt/LanguageTool-6.6/org/languagetool/resource/el/hunspell/el_GR.info")?,
        ),
        _ => return None,
    };
    let java = find_java()?;
    let lt_libs = home_path("apps/lt/LanguageTool-6.6/libs")?;
    // ElSuggest.class lives in <repo>/scripts — resolve from cwd (fronts run
    // check_server from the nlprule-fork dir) or the env override
    let class_dir = std::env::var("NLPRULE_SIDECAR_DIR")
        .ok()
        .or_else(|| {
            let cwd = std::env::current_dir().ok()?;
            cwd.parent().map(|p| p.join("scripts").to_string_lossy().into_owned())
        })
        .or_else(|| home_path("Documentos/Github/nlprule/scripts"))?;
    let cp = format!("{class_dir}:{lt_libs}/morfologik-speller.jar:{lt_libs}/morfologik-stemming.jar:{lt_libs}/morfologik-fsa.jar");
    if !std::path::Path::new(&format!("{class_dir}/ElSuggest.class")).exists() {
        return None;
    }
    Some((
        java,
        vec![
            "-cp".into(),
            cp,
            "ElSuggest".into(),
            dict,
            info,
            "18".into(),
        ],
    ))
}

impl MorfologikSidecar {
    fn spawn(lang: &str) -> Option<MorfologikSidecar> {
        let (java, args) = sidecar_command(lang)?;
        let mut child = Command::new(java)
            .args(&args)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .ok()?;
        let stdin = child.stdin.take()?;
        let stdout = BufReader::new(child.stdout.take()?);
        Some(MorfologikSidecar {
            child,
            stdin,
            stdout,
        })
    }

    /// Exact LT suggestion list for `word`, or None if the sidecar is
    /// unavailable/failed (caller falls back to the FST path).
    pub fn suggest(lang: &str, word: &str) -> Option<Vec<String>> {
        if word.is_empty() || word.len() > 80 {
            return None;
        }
        let mut guard = SIDECAR.lock().unwrap_or_else(|e| e.into_inner());
        // (re)spawn lazily; a dead sidecar disables the path for good
        let slot = guard.get_or_insert_with(|| None);
        if slot.is_none() {
            *slot = MorfologikSidecar::spawn(lang);
        }
        let sidecar = slot.as_mut()?;
        if sidecar
            .stdin
            .write_all(word.as_bytes())
            .and_then(|_| sidecar.stdin.write_all(b"\n"))
            .and_then(|_| sidecar.stdin.flush())
            .is_err()
        {
            let _ = sidecar.child.kill();
            *slot = None;
            return None;
        }
        let mut line = String::new();
        match sidecar.stdout.read_line(&mut line) {
            Ok(0) | Err(_) => {
                let _ = sidecar.child.kill();
                *slot = None;
                None
            }
            Ok(_) => Some(parse_json_array(&line)),
        }
    }
}

fn parse_json_array(line: &str) -> Vec<String> {
    let line = line.trim();
    let inner = line
        .strip_prefix('[')
        .and_then(|s| s.strip_suffix(']'))
        .unwrap_or("");
    if inner.is_empty() {
        return Vec::new();
    }
    inner
        .split("\",\"")
        .map(|s| s.trim_matches('"').to_string())
        .collect()
}
