#[must_use]
#[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
pub fn factors(n: u32) -> Vec<u32> {
    let upper_bound = f64::from(n).sqrt().floor() as u32;
    let mut results: Vec<_> = Vec::new();

    for i in 1..=upper_bound {
        let divides = n % i;
        let b = n / i;
        if divides == 0 {
            results.push(i);
            if i != b {
                results.push(b);
            }
        }
    }

    results
}
