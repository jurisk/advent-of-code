use advent_of_code_common::parsing::Error;
use advent_of_code_common::parsing::parse_comma_separated_vec;

const DATA: &str = include_str!("../../resources/07.txt");

fn cost_1(a: u64, b: u64) -> u64 {
    a.max(b) - a.min(b)
}

fn cost_2(a: u64, b: u64) -> u64 {
    let n = cost_1(a, b);
    (n * (n + 1)) / 2
}

fn solve<F>(input: &str, cost: F) -> Result<u64, Error>
where
    F: Fn(u64, u64) -> u64,
{
    let positions: Vec<u64> = parse_comma_separated_vec(input)?;

    let min = *positions.iter().min().ok_or("Failed to get min")?;
    let max = *positions.iter().max().ok_or("Failed to get max")?;

    (min ..= max)
        .map(|q| positions.iter().map(|&x| cost(x, q)).sum())
        .min()
        .ok_or_else(|| "Failed to get min".to_string())
}

fn solve_1(input: &str) -> Result<u64, Error> {
    solve(input, cost_1)
}

fn solve_2(input: &str) -> Result<u64, Error> {
    solve(input, cost_2)
}

fn main() {
    let result_1 = solve_1(DATA);
    println!("Part 1: {result_1:?}");

    let result_2 = solve_2(DATA);
    println!("Part 2: {result_2:?}");
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = "16,1,2,0,4,2,7,1,2,14";

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(TEST_DATA), Ok(37));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(DATA), Ok(356_992));
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve_2(TEST_DATA), Ok(168));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(DATA), Ok(101_268_110));
    }
}
