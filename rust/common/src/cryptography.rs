#[must_use]
pub fn md5_for_string(input: &str) -> String {
    let digest = md5::compute(input);
    format!("{digest:x}")
}
