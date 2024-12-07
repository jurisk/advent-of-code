use std::str::FromStr;

use advent_of_code_common::parsing::{
    Error, parse_lines_to_vec, parse_pair_unsafe, parse_separated_vec, parse_str,
};

const DATA: &str = include_str!("../../resources/07.txt");

type N = u64;
type R = u64;

#[derive(Clone)]
struct Equation {
    result:  N,
    numbers: Vec<N>,
}

impl FromStr for Equation {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parsed = parse_pair_unsafe(s, ": ", parse_str, |s| parse_separated_vec(s, " "))?;
        let (result, numbers) = parsed;
        Ok(Equation { result, numbers })
    }
}

type Data = Vec<Equation>;

fn parse(input: &str) -> Result<Data, Error> {
    parse_lines_to_vec(input)
}

#[allow(
    clippy::cast_possible_truncation,
    clippy::cast_sign_loss,
    clippy::cast_precision_loss
)]
fn concat(a: N, b: N) -> N {
    let b_length = (b as f64).log10() as u32 + 1;
    a * 10u64.pow(b_length) + b
}

fn try_op<const ALLOW_PIPE: bool>(result: N, op: fn(N, N) -> N, numbers: &mut [N]) -> bool {
    match numbers.first().copied() {
        Some(a) => {
            match numbers.get(1).copied() {
                Some(b) => {
                    numbers[1] = op(a, b);
                    let obtained = validate::<ALLOW_PIPE>(result, &mut numbers[1 ..]);
                    numbers[1] = b;
                    obtained
                },
                None => result == a,
            }
        },

        None => result == 0,
    }
}

fn validate<const ALLOW_PIPE: bool>(result: N, numbers: &mut [N]) -> bool {
    try_op::<ALLOW_PIPE>(result, |a, b| a + b, numbers)
        || try_op::<ALLOW_PIPE>(result, |a, b| a * b, numbers)
        || (ALLOW_PIPE && try_op::<ALLOW_PIPE>(result, concat, numbers))
}

fn solve<const ALLOW_PIPE: bool>(data: &mut Data) -> N {
    let mut result = 0;
    for equation in data.iter_mut() {
        if validate::<ALLOW_PIPE>(equation.result, &mut equation.numbers) {
            result += equation.result;
        }
    }
    result
}

fn solve_1(data: &mut Data) -> R {
    solve::<false>(data)
}

fn solve_2(data: &mut Data) -> R {
    solve::<true>(data)
}

fn main() -> Result<(), Error> {
    let mut data = parse(DATA)?;

    let result_1 = solve_1(&mut data.clone());
    println!("Part 1: {result_1}");

    let result_2 = solve_2(&mut data);
    println!("Part 2: {result_2}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = include_str!("../../resources/07-test-00.txt");

    fn test_data() -> Data {
        parse(TEST_DATA).unwrap()
    }

    fn real_data() -> Data {
        parse(DATA).unwrap()
    }

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(&mut test_data()), 3749);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(&mut real_data()), 1_038_838_357_795);
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve_2(&mut test_data()), 11387);
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(&mut real_data()), 254_136_560_217_241);
    }
}
