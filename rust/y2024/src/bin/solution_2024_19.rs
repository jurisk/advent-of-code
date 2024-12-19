use std::convert::identity;

use advent_of_code_common::parsing::{
    Error, parse_lines_to_vec, parse_pair_separated_by_double_newline, parse_separated_vec,
};

const DATA: &str = include_str!("../../resources/19.txt");

type R = usize;
type Input = (Vec<String>, Vec<String>);

fn parse(input: &str) -> Result<Input, Error> {
    parse_pair_separated_by_double_newline(
        input,
        |left| parse_separated_vec(left, ", "),
        parse_lines_to_vec,
    )
}

fn ways(towel: &str, stripes: &[String]) -> R {
    let mut dp = vec![0; towel.len() + 1];
    dp[towel.len()] = 1;
    for i in (0 .. towel.len()).rev() {
        for stripe in stripes {
            if towel[i ..].starts_with(stripe) {
                dp[i] += dp[i + stripe.len()];
            }
        }
    }
    dp[0]
}

fn solve<F>(data: &Input, count_f: F) -> R
where
    F: Fn(R) -> R,
{
    let (stripes, towels) = data;
    let mut result = 0;
    for towel in towels {
        let r = ways(towel, stripes);
        let r = count_f(r);
        result += r;
    }
    result
}

fn solve_1(data: &Input) -> R {
    solve(data, |x| x.min(1))
}

fn solve_2(data: &Input) -> R {
    solve(data, identity)
}

fn main() -> Result<(), Error> {
    let data = parse(DATA)?;

    let result_1 = solve_1(&data);
    println!("Part 1: {result_1}");

    let result_2 = solve_2(&data);
    println!("Part 2: {result_2}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = include_str!("../../resources/19-test-00.txt");

    fn test_data() -> Input {
        parse(TEST_DATA).unwrap()
    }

    fn real_data() -> Input {
        parse(DATA).unwrap()
    }

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(&test_data()), 6);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(&real_data()), 374);
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve_2(&test_data()), 16);
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(&real_data()), 1_100_663_950_563_322);
    }
}
