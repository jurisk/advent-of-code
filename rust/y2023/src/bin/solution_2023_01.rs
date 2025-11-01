use std::collections::HashMap;

use advent_of_code_common::parsing::{Error, parse_lines_to_vec};

const DATA: &str = include_str!("../../resources/01.txt");

type Data = Vec<String>;
type N = u32;

#[expect(clippy::cast_lossless)]
fn digits_dictionary() -> HashMap<String, N> {
    (0 ..= 9)
        .map(|n| (vec![(b'0' + n) as char].into_iter().collect(), n as N))
        .collect()
}

fn words_dictionary() -> HashMap<String, N> {
    let words = vec![
        "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
    ];

    words
        .into_iter()
        .map(ToString::to_string)
        .zip(1 ..= 9)
        .collect()
}

fn to_digits(mut s: &str, dictionary: &HashMap<String, N>) -> Vec<N> {
    let mut results = vec![];

    while !s.is_empty() {
        // https://en.wikipedia.org/wiki/Aho%E2%80%93Corasick_algorithm would be relevant if the dictionary was larger
        if let Some((_, &n)) = dictionary.iter().find(|&(key, _)| s.starts_with(key)) {
            results.push(n);
        }

        s = &s[1 ..];
    }

    results
}

fn solve(data: Data, dictionary: &HashMap<String, N>) -> N {
    data.into_iter()
        .map(|s| to_digits(&s, dictionary))
        .map(|v| v.first().unwrap() * 10 + v.last().unwrap())
        .sum()
}

fn solve_1(data: Data) -> N {
    solve(data, &digits_dictionary())
}

fn solve_2(data: Data) -> N {
    let mut merged_dictionary = digits_dictionary();
    merged_dictionary.extend(words_dictionary());
    solve(data, &merged_dictionary)
}

fn part_1(input: &str) -> Result<N, Error> {
    let data = parse_lines_to_vec(input)?;
    Ok(solve_1(data))
}

fn part_2(input: &str) -> Result<N, Error> {
    let data = parse_lines_to_vec(input)?;
    Ok(solve_2(data))
}

fn main() -> Result<(), Error> {
    let result_1 = part_1(DATA)?;
    println!("Part 1: {result_1}");

    let result_2 = part_2(DATA)?;
    println!("Part 2: {result_2}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA_1: &str = include_str!("../../resources/01-test-1.txt");
    const TEST_DATA_2: &str = include_str!("../../resources/01-test-2.txt");

    #[test]
    fn test_solve_1_test() {
        assert_eq!(part_1(TEST_DATA_1), Ok(142));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(54081));
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(part_2(TEST_DATA_2), Ok(281));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(54649));
    }
}
