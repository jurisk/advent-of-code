use num_traits::Num;

#[must_use]
#[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
pub fn factors(n: u32) -> Vec<u32> {
    let upper_bound = f64::from(n).sqrt().floor() as u32;
    let mut results: Vec<_> = Vec::new();

    for i in 1 ..= upper_bound {
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
    if b == 0 { a } else { gcd(b, a % b) }
}

#[must_use]
pub fn lcm(a: i64, b: i64) -> i64 {
    a.abs() * b.abs() / gcd(a, b)
}

#[must_use]
pub fn solve_two_variable_integer_linear_equation_system<N: Num + Copy>(
    a: N,
    b: N,
    c: N,
    d: N,
    e: N,
    f: N,
) -> Option<(N, N)> {
    let div = |dividend: N, divisor: N| -> Option<N> {
        (dividend % divisor == N::zero()).then(|| dividend / divisor)
    };

    let det = a * d - b * c;
    if det == N::zero() {
        None
    } else {
        let x = div(e * d - b * f, det);
        let y = div(a * f - e * c, det);
        match (x, y) {
            (Some(x), Some(y)) => Some((x, y)),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve_two_variable_integer_linear_equation_system() {
        assert_eq!(
            solve_two_variable_integer_linear_equation_system(2, 1, 3, -1, 15, 5),
            Some((4, 7))
        );
        assert_eq!(
            solve_two_variable_integer_linear_equation_system(1, 2, 1, 2, 5, 6),
            None
        );
    }
}
