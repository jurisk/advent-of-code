use advent_of_code_common::parsing::Error;
use itertools::FoldWhile::{Continue, Done};
use itertools::Itertools;
use num_enum::TryFromPrimitive;

#[derive(Copy, Clone, TryFromPrimitive, Debug)]
#[repr(u8)]
enum Command {
    Up   = b'(',
    Down = b')',
}

impl Command {
    fn diff(self) -> i32 {
        match self {
            Command::Up => 1,
            Command::Down => -1,
        }
    }
}

type Data = Vec<Command>;

fn parse(input: &str) -> Result<Data, Error> {
    input
        .chars()
        .map(|ch| Command::try_from(ch as u8).map_err(|e| format!("{e}")))
        .collect()
}

type Output1 = i32;
type Output2 = usize;

fn solve_1(data: &Data) -> Output1 {
    data.iter().map(|c| c.diff()).sum()
}

fn solve_2(data: &Data) -> Output2 {
    let (index, result) = data
        .iter()
        .fold_while((0, 0), |(idx, floor), b| {
            let new_floor = floor + b.diff();
            if new_floor < 0 {
                Done((idx + 1, new_floor))
            } else {
                Continue((idx + 1, new_floor))
            }
        })
        .into_inner();

    if result < 0 {
        index
    } else {
        panic!("Basement never reached in ${data:?}")
    }
}

fn part_1(input: &str) -> Result<Output1, Error> {
    parse(input).map(|input| solve_1(&input))
}

fn part_2(input: &str) -> Result<Output2, Error> {
    parse(input).map(|input| solve_2(&input))
}

const DATA: &str = include_str!("../../resources/01.txt");

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

    const TEST_DATA: &str = include_str!("../../resources/01-test.txt");

    #[test]
    fn test_solve_1_test() {
        assert_eq!(part_1(TEST_DATA), Ok(-1));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(74));
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(part_2(TEST_DATA), Ok(5));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(1795));
    }
}
