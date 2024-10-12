use std::str::FromStr;

use advent_of_code_common::parsing::{
    Error, parse_lines_to_vec, parse_str, split_into_two_strings,
};

#[derive(Debug)]
struct Layer {
    depth: usize,
    range: usize,
}

impl Layer {
    fn location_at_time(&self, time: usize) -> usize {
        let q = self.range - 1;
        let without_full_cycles = time % (q * 2);
        if without_full_cycles > q {
            q - (without_full_cycles - q)
        } else {
            without_full_cycles
        }
    }

    fn catches(&self, delay: usize) -> bool {
        let time = self.depth + delay;
        let location = self.location_at_time(time);
        location == 0
    }
}

impl FromStr for Layer {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (a, b) = split_into_two_strings(s, ": ")?;
        let depth = parse_str(&a)?;
        let range = parse_str(&b)?;
        let result = Layer { depth, range };
        Ok(result)
    }
}

type Data = Vec<Layer>;

fn severity_with_delay(layers: &Data, delay: usize) -> usize {
    let mut result = 0;

    for layer in layers {
        if layer.catches(delay) {
            result += layer.range * layer.depth;
        }
    }

    result
}

fn is_caught(layers: &Data, delay: usize) -> bool {
    for layer in layers {
        if layer.catches(delay) {
            return true;
        }
    }

    false
}

fn solve_1(layers: &Data) -> usize {
    severity_with_delay(layers, 0)
}

fn solve_2(layers: &Data) -> Option<usize> {
    (0 .. usize::MAX).find(|&delay| !is_caught(layers, delay))
}

fn parse(input: &str) -> Result<Data, Error> {
    parse_lines_to_vec(input)
}

fn part_1(input: &str) -> Result<usize, Error> {
    parse(input).map(|graph| solve_1(&graph))
}

fn part_2(input: &str) -> Result<Option<usize>, Error> {
    parse(input).map(|graph| solve_2(&graph))
}

const DATA: &str = include_str!("../../resources/13.txt");

fn main() -> Result<(), Error> {
    let result_1 = part_1(DATA)?;
    println!("Part 1: {result_1}");

    let result_2 = part_2(DATA)?;
    println!("Part 2: {result_2:?}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST: &str = include_str!("../../resources/13-test.txt");

    #[test]
    fn location_test() {
        let layer = Layer { depth: 0, range: 4 };
        let locations: Vec<_> = (0 ..= 8)
            .into_iter()
            .map(|x| layer.location_at_time(x))
            .collect();
        assert_eq!(locations, vec![0, 1, 2, 3, 2, 1, 0, 1, 2]);
    }

    #[test]
    fn test_solve_1_test() {
        assert_eq!(part_1(TEST), Ok(24));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(748));
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(part_2(TEST), Ok(Some(10)));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(Some(3873662)));
    }
}
