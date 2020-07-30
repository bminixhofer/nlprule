pub fn fix_regex(regex: &str) -> String {
    format!("^({})$", regex)
}
