extern crate core;

use std::ops::RangeInclusive;

use advent_of_code_common::parsing::{
    Error, parse_lines_to_vec_passing_parser, parse_space_separated_vec,
};
use itertools::Itertools;

const DATA: &str = include_str!("../../resources/02.txt");

type N = i32;
type R = usize;
type Row = Vec<N>;
type Data = Vec<Row>;

const VALID_DIFF_RANGES: [RangeInclusive<N>; 2] = [(1 as N) ..= 3, (-3 as N) ..= -1];

fn parse(input: &str) -> Result<Data, Error> {
    parse_lines_to_vec_passing_parser(input, parse_space_separated_vec)
}

fn valid_diffs_1(row: &[N], range: &RangeInclusive<N>) -> bool {
    row.iter()
        .tuple_windows()
        .map(|(a, b)| b - a)
        .all(|diff| range.contains(&diff))
}

#[allow(dead_code)]
fn valid_diffs_2_straightforward(row: &[N], range: &RangeInclusive<N>) -> bool {
    valid_diffs_1(row, range)
        || (0 .. row.len())
            .any(|idx| {
                let mut row = row.to_vec();
                row.remove(idx);
                valid_diffs_1(row.as_slice(), range)
            })
}

fn valid_diffs_2_dp(row: &[N], range: &RangeInclusive<N>) -> bool {
    let valid_neighbours: Vec<_> = row
        .iter()
        .tuple_windows()
        .map(|(a, b)| b - a)
        .map(|diff| range.contains(&diff))
        .collect();

    let valid_jumps: Vec<_> = row
        .iter()
        .tuple_windows()
        .map(|(a, _, b)| b - a)
        .map(|diff| range.contains(&diff))
        .collect();

    let invalid_neighbours = valid_neighbours.iter().filter(|x| !**x).count();

    if invalid_neighbours == 0 {
        // Safe without removing any level.
        true
    } else if invalid_neighbours == 1 {
        let a = valid_neighbours
            .into_iter()
            .position(|x| !x)
            .expect("Expected to find the invalid position");
        let b = a + 1;

        if a == 0 || b == row.len() - 1 {
            // Safe if either end is removed.
            true
        } else {
            // Safe if a particular middle level is removed
            valid_jumps[a - 1] || valid_jumps[b - 1]
        }
    } else if invalid_neighbours == 2 {
        let a = valid_neighbours
            .iter()
            .position(|x| !*x)
            .expect("Expected to find the invalid position");
        let b = a + 1;
        if valid_neighbours[b] {
            // Not consecutive two neighbours
            false
        } else {
            valid_jumps[a]
        }
    } else {
        // Unsafe regardless of which level is removed.
        false
    }
}

fn safe<F>(row: &Row, check: F) -> bool
where
    F: Fn(&[N], &RangeInclusive<N>) -> bool,
{
    VALID_DIFF_RANGES
        .into_iter()
        .any(|range| check(row.as_slice(), &range))
}

fn solve<F>(data: &Data, check: F) -> R
where
    F: Fn(&[N], &RangeInclusive<N>) -> bool + Copy,
{
    data.iter().filter(|row| safe(row, check)).count()
}

fn solve_1(data: &Data) -> R {
    solve(data, valid_diffs_1)
}

fn solve_2(data: &Data) -> R {
    solve(data, valid_diffs_2_dp)
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

    const TEST_DATA: &str = include_str!("../../resources/02-test-00.txt");

    fn test_data() -> Data {
        parse(TEST_DATA).unwrap()
    }

    fn real_data() -> Data {
        parse(DATA).unwrap()
    }

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(&test_data()), 2);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(&real_data()), 341);
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve_2(&test_data()), 4);
    }

    #[test]
    fn test_solve_2_real_straightforward() {
        assert_eq!(solve(&real_data(), valid_diffs_2_straightforward), 404);
    }

    #[test]
    fn test_solve_2_real_dp() {
        assert_eq!(solve(&real_data(), valid_diffs_2_dp), 404);
    }

    #[test]
    fn test_17_16_14_9_11() {
        let row = vec![17, 16, 14, 9, 11];
        assert!(valid_diffs_2_dp(&row, &VALID_DIFF_RANGES[1]));
    }
}
