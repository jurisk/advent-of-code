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

// https://en.wikipedia.org/wiki/Modular_exponentiation#Pseudocode
#[must_use]
pub fn mod_pow(mut base: u64, mut exponent: u64, modulus: u64) -> u64 {
    if modulus == 1 {
        return 0;
    }
    let mut result = 1;
    base %= modulus;
    while exponent > 0 {
        if exponent % 2 == 1 {
            result = result * base % modulus;
        }
        exponent >>= 1;
        base = base * base % modulus;
    }
    result
}

#[must_use]
pub fn sum_of_standard_arithmetic_progression(n: u64) -> u64 {
    sum_of_arithmetic_progression(1, 1, n)
}

#[must_use]
// https://en.wikipedia.org/wiki/Arithmetic_progression#Sum
pub fn sum_of_arithmetic_progression(a1: u64, d: u64, n: u64) -> u64 {
    if n == 0 {
        0
    } else {
        let an = a1 + (n - 1) * d;
        (n * (a1 + an)) / 2
    }
}

#[must_use]
pub fn gcd(a: i64, b: i64) -> i64 {
    if b == 0 {
        a
    } else {
        gcd(b, a % b)
    }
}

#[must_use]
pub fn lcm(a: i64, b: i64) -> i64 {
    a.abs() * b.abs() / gcd(a, b)
}
