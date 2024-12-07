use std::str::FromStr;
use advent_of_code_common::parsing::{Error, parse_lines_to_vec, parse_pair_unsafe, parse_separated_vec, parse_str};

const DATA: &str = include_str!("../../resources/07.txt");

type N = u64;
type R = u64;

struct Equation {
    result: N,
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

#[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss, clippy::cast_precision_loss)]
fn concat(a: N, b: N) -> N {
    let b_length = (b as f64).log10() as u32 + 1;
    a * 10u64.pow(b_length) + b
}

fn validate(allow_pipe: bool, result: N, numbers: &[N]) -> bool {
    match numbers {
        [] => false,
        [a] => result == *a,
        [a, b, t @ ..] => {
            let mut options = vec![
                a * b,
                a + b,
            ];
            if allow_pipe {
                options.push(concat(*a, *b));
            }

            options.iter().any(|r| {
                let combined = [vec![*r], t.to_vec()].concat();
                validate(allow_pipe, result, &combined)
            })
        }
    }
}

fn solve(data: &Data, allow_pipes: bool) -> N {
    data.iter().filter(|equation| {
        let Equation { result, numbers } = equation;
        validate(allow_pipes, *result, numbers)
    }).map(|equation| equation.result).sum()
}

fn solve_1(data: &Data) -> R {
    solve(data, false)
}

fn solve_2(data: &Data) -> R {
    solve(data, true)
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

    const TEST_DATA: &str = include_str!("../../resources/07-test-00.txt");

    fn test_data() -> Data {
        parse(TEST_DATA).unwrap()
    }

    fn real_data() -> Data {
        parse(DATA).unwrap()
    }

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(&test_data()), 3749);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(&real_data()), 1_038_838_357_795);
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve_2(&test_data()), 11387);
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(&real_data()), 254_136_560_217_241);
    }
}
