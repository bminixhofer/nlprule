//! Runtime hunspell integration for LT's hunspell-dictionary languages
//! (da/sv/eo/km/ar/gl): delegate `is_known` + suggestions to the system
//! libhunspell via dlopen, using the very `.aff`/`.dic` files LT 6.6 ships
//! (embedded in the rules bin at compile time, materialized to a temp dir
//! at first use). Verified empirically: system libhunspell-1.7 with LT's
//! dictionaries reproduces LT 6.6's suggestion lists (da 4/4, sv 2/2,
//! eo 10/11, km 27/33 exact in a live probe); the port's FST approximation
//! diverges on ngram/phonetic/compound candidates.
//!
//! Everything degrades gracefully: if the library or the data is missing,
//! callers fall back to the FST speller path.

use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::io::Write;
use std::os::raw::{c_char, c_int, c_void};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

/// Embedded hunspell dictionary data (compile-time, from
/// `data2/<lang>/filters/hunspell/`): the `.aff`+`.dic` LT ships plus
/// the words LT adds to the speller's user dictionary (ignore/spelling).
#[derive(serde::Serialize, serde::Deserialize, Default, Clone)]
pub struct HunspellData {
    pub aff: Vec<u8>,
    pub dic: Vec<u8>,
    pub extra_words: Vec<String>,
}

impl HunspellData {
    pub fn is_present(&self) -> bool {
        !self.aff.is_empty() && !self.dic.is_empty()
    }
}

const RTLD_NOW: c_int = 2;

extern "C" {
    fn dlopen(filename: *const c_char, flag: c_int) -> *mut c_void;
    fn dlsym(handle: *mut c_void, symbol: *const c_char) -> *mut c_void;
}

type Hunhandle = *mut c_void;
type CreateFn = unsafe extern "C" fn(*const c_char, *const c_char) -> Hunhandle;
type DestroyFn = unsafe extern "C" fn(Hunhandle);
type SpellFn = unsafe extern "C" fn(Hunhandle, *const c_char) -> c_int;
type SuggestFn = unsafe extern "C" fn(Hunhandle, *mut *mut *mut c_char, *const c_char) -> c_int;
type FreeListFn = unsafe extern "C" fn(Hunhandle, *mut *mut *mut c_char, c_int);
type AddFn = unsafe extern "C" fn(Hunhandle, *const c_char) -> c_int;

struct Api {
    _lib: *mut c_void,
    create: CreateFn,
    destroy: DestroyFn,
    spell: SpellFn,
    suggest: SuggestFn,
    free_list: FreeListFn,
    add: AddFn,
}

unsafe impl Send for Api {}
unsafe impl Sync for Api {}

static API: Lazy<Option<Api>> = Lazy::new(|| unsafe {
    for soname in [
        "libhunspell-1.7.so.0\0",
        "libhunspell-1.7.so\0",
        "libhunspell-1.6.so.0\0",
    ] {
        let lib = dlopen(soname.as_ptr().cast::<c_char>(), RTLD_NOW);
        if lib.is_null() {
            continue;
        }
        macro_rules! sym {
            ($name:literal) => {{
                let p = dlsym(lib, concat!($name, "\0").as_ptr().cast::<c_char>());
                if p.is_null() {
                    continue;
                }
                std::mem::transmute(p)
            }};
        }
        return Some(Api {
            _lib: lib,
            create: sym!("Hunspell_create"),
            destroy: sym!("Hunspell_destroy"),
            spell: sym!("Hunspell_spell"),
            suggest: sym!("Hunspell_suggest"),
            free_list: sym!("Hunspell_free_list"),
            add: sym!("Hunspell_add"),
        });
    }
    None
});

/// Whether a usable system libhunspell was found.
pub fn available() -> bool {
    API.is_some()
}

struct Handle {
    raw: Hunhandle,
}

unsafe impl Send for Handle {}
unsafe impl Sync for Handle {}

impl Drop for Handle {
    fn drop(&mut self) {
        if let Some(api) = API.as_ref() {
            unsafe { (api.destroy)(self.raw) };
        }
    }
}

static HANDLES: Lazy<Mutex<HashMap<String, Arc<Mutex<Option<Handle>>>>>> =
    Lazy::new(|| Mutex::new(HashMap::new()));

fn materialize(lang: &str, data: &HunspellData, tag: &str, bytes: &[u8]) -> Option<PathBuf> {
    let mut dir = std::env::temp_dir();
    dir.push(format!("nlprule-hunspell-{}", lang));
    std::fs::create_dir_all(&dir).ok()?;
    let path = dir.join(tag);
    if std::fs::metadata(&path).and_then(|m| Ok(m.len())).ok() == Some(bytes.len() as u64) {
        return Some(path); // already materialized (same size heuristic)
    }
    let mut tmp = path.clone().into_os_string();
    tmp.push(".tmp");
    let mut f = std::fs::File::create(&tmp).ok()?;
    f.write_all(bytes).ok()?;
    f.flush().ok()?;
    std::fs::rename(&tmp, &path).ok()?;
    Some(path)
}

fn with_handle<R>(
    lang: &str,
    data: &HunspellData,
    f: impl FnOnce(&Api, Hunhandle) -> R,
) -> Option<R> {
    let api = API.as_ref()?;
    if !data.is_present() {
        return None;
    }
    let slot = {
        let mut map = HANDLES.lock().ok()?;
        map.entry(lang.to_string())
            .or_insert_with(|| Arc::new(Mutex::new(None)))
            .clone()
    };
    let mut guard = slot.lock().ok()?;
    if guard.is_none() {
        // materialize both files first: hunspell parses the .aff at create
        let aff = materialize(lang, data, "dict.aff", &data.aff)?;
        let dic = materialize(lang, data, "dict.dic", &data.dic)?;
        let aff = CString::new(aff.to_string_lossy().into_owned()).ok()?;
        let dic = CString::new(dic.to_string_lossy().into_owned()).ok()?;
        let raw = unsafe { (api.create)(aff.as_ptr(), dic.as_ptr()) };
        if raw.is_null() {
            return None;
        }
        let handle = Handle { raw };
        // LT adds the ignore/spelling words to the speller's user
        // dictionary (they become correctly spelled and suggestible)
        for word in &data.extra_words {
            if let Ok(cw) = CString::new(word.as_str()) {
                unsafe { (api.add)(handle.raw, cw.as_ptr()) };
            }
        }
        *guard = Some(handle);
    }
    let raw = guard.as_ref().map(|h| h.raw)?;
    drop(guard);
    Some(f(api, raw))
}

/// Whether `word` is correctly spelled per the embedded hunspell
/// dictionary (aff rules apply: capitalization, compounding, affixes).
pub fn spell(lang: &str, data: &HunspellData, word: &str) -> Option<bool> {
    let cw = CString::new(word).ok()?;
    with_handle(lang, data, |api, raw| unsafe {
        (api.spell)(raw, cw.as_ptr()) != 0
    })
}

/// LT-ordered hunspell suggestions for `word`.
pub fn suggest(lang: &str, data: &HunspellData, word: &str) -> Option<Vec<String>> {
    let cw = CString::new(word).ok()?;
    with_handle(lang, data, |api, raw| unsafe {
        let mut list: *mut *mut c_char = std::ptr::null_mut();
        let n = (api.suggest)(raw, &mut list, cw.as_ptr());
        let mut out = Vec::with_capacity(n.max(0) as usize);
        for i in 0..n.max(0) as usize {
            let p = *list.add(i);
            if p.is_null() {
                continue;
            }
            if let Ok(s) = CStr::from_ptr(p).to_str() {
                out.push(s.to_string());
            }
        }
        if n > 0 {
            (api.free_list)(raw, &mut list, n);
        }
        out
    })
}
