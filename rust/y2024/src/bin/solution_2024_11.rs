use memoize::memoize;

type N = u64;

const DATA: [N; 8] = [6_563_348, 67, 395, 0, 6, 4425, 89567, 739_318];

type R = usize;

#[expect(
    clippy::cast_possible_truncation,
    clippy::cast_sign_loss,
    clippy::cast_precision_loss
)]
fn halves(n: N) -> Option<(N, N)> {
    let digits = (n as f64).log10() as usize + 1;
    (digits % 2 == 0).then_some({
        let half_length = digits / 2;
        let divisor = 10u64.pow(half_length as u32);
        let left = n / divisor;
        let right = n % divisor;
        (left, right)
    })
}

#[memoize]
#[allow(clippy::collapsible_else_if)]
fn solve_one(n: N, blinks: usize) -> R {
    if blinks == 0 {
        1
    } else {
        if n == 0 {
            solve_one(1, blinks - 1)
        } else {
            match halves(n) {
                Some((left, right)) => solve_one(left, blinks - 1) + solve_one(right, blinks - 1),
                None => solve_one(n * 2024, blinks - 1),
            }
        }
    }
}

fn solve(data: &[N], blinks: usize) -> R {
    data.iter().map(|n| solve_one(*n, blinks)).sum()
}

fn main() {
    let result_1 = solve(&DATA, 25);
    println!("Part 1: {result_1}");

    let result_2 = solve(&DATA, 75);
    println!("Part 2: {result_2}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve(&DATA, 25), 184_927);
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve(&DATA, 75), 220_357_186_726_677);
    }
}
