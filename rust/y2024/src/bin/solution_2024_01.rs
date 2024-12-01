use std::collections::HashMap;

use advent_of_code_common::parsing::{Error, parse_lines_to_vec_passing_parser, parse_str};
use itertools::Itertools;

const DATA: &str = include_str!("../../resources/01.txt");

type N = i32;
type Data = (Vec<N>, Vec<N>);

fn parse_line(input: &str) -> Result<(N, N), Error> {
    let parts: Result<Vec<N>, Error> = input.split_whitespace().map(parse_str).collect();
    let parts = parts?;
    parts
        .into_iter()
        .collect_tuple::<(N, N)>()
        .ok_or("Expected 2 parts".to_string())
}

fn parse(input: &str) -> Result<Data, Error> {
    let data: Vec<(N, N)> = parse_lines_to_vec_passing_parser(input, parse_line)?;
    Ok(data.into_iter().unzip())
}

fn solve_1(data: &Data) -> N {
    let (mut a, mut b) = data.clone();
    a.sort_unstable();
    b.sort_unstable();
    a.into_iter().zip(b).map(|(x, y)| (x - y).abs()).sum()
}

#[expect(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
fn solve_2(data: &Data) -> N {
    let (a, b) = data;
    let counts: HashMap<&N, usize> = b.iter().counts();

    a.iter()
        .map(|x| *x * (counts.get(x).copied().unwrap_or_default() as N))
        .sum()
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

    const TEST_DATA: &str = include_str!("../../resources/01-test-00.txt");

    fn test_data() -> Data {
        parse(TEST_DATA).unwrap()
    }

    fn real_data() -> Data {
        parse(DATA).unwrap()
    }

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(&test_data()), 11);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(&real_data()), 1_530_215);
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve_2(&test_data()), 31);
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(&real_data()), 26_800_609);
    }
}
