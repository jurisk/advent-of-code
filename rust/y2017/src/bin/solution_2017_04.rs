use std::collections::{BTreeSet, HashSet};
use std::str::FromStr;

use advent_of_code_common::parsing::{Error, parse_lines_to_vec};

#[derive(Debug)]
struct Password {
    words: Vec<String>,
}

impl FromStr for Password {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let words = s.split(' ').map(ToString::to_string).collect();
        Ok(Password { words })
    }
}

impl Password {
    fn is_valid_1(&self) -> bool {
        let set: HashSet<&String> = self.words.iter().collect();
        set.len() == self.words.len()
    }

    fn is_valid_2(&self) -> bool {
        let set: HashSet<BTreeSet<char>> = self.words.iter().map(|w| w.chars().collect()).collect();
        set.len() == self.words.len()
    }
}

type Data = Vec<Password>;

fn parse(input: &str) -> Result<Data, Error> {
    parse_lines_to_vec(input)
}

fn solve<F>(data: &Data, f: F) -> usize
where
    F: Fn(&Password) -> bool,
{
    data.iter().filter(|&x| f(x)).count()
}

fn solve_1(data: &Data) -> usize {
    solve(data, Password::is_valid_1)
}

fn solve_2(data: &Data) -> usize {
    solve(data, Password::is_valid_2)
}

fn part_1(input: &str) -> Result<usize, Error> {
    parse(input).map(|input| solve_1(&input))
}

fn part_2(input: &str) -> Result<usize, Error> {
    parse(input).map(|input| solve_2(&input))
}

const DATA: &str = include_str!("../../resources/04.txt");

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

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(451));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(223));
    }
}
