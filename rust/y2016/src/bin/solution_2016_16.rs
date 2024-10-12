use advent_of_code_common::parsing::{Error, convert_error};
use itertools::Itertools;
use num_enum::TryFromPrimitive;

use crate::Digit::{One, Zero};

#[repr(u8)]
#[derive(TryFromPrimitive, Eq, PartialEq, Copy, Clone)]
enum Digit {
    Zero = b'0',
    One  = b'1',
}

fn parse(input: &str) -> Result<Vec<Digit>, Error> {
    input
        .chars()
        .map(|x| convert_error(&x.to_string(), Digit::try_from(x as u8)))
        .collect()
}

fn fill_to(data: &[Digit], size: usize) -> Vec<Digit> {
    if data.len() > size {
        data[.. size].to_vec()
    } else {
        let other: Vec<Digit> = data
            .iter()
            .rev()
            .map(|d| {
                match d {
                    Zero => One,
                    One => Zero,
                }
            })
            .collect();
        let new = [data.to_vec(), vec![Zero], other].concat();
        fill_to(&new, size)
    }
}

fn checksum(data: &[Digit]) -> Vec<Digit> {
    let result: Vec<Digit> = data
        .iter()
        .tuples()
        .map(|(a, b)| if a == b { One } else { Zero })
        .collect();
    if result.len() % 2 == 0 {
        checksum(&result)
    } else {
        result
    }
}

fn solve(data: &[Digit], size: usize) -> Vec<Digit> {
    assert!(data.len() <= size);
    let filled = fill_to(data, size);
    checksum(&filled)
}

fn process(input: &str, size: usize) -> Result<String, Error> {
    let result = parse(input).map(|x| solve(&x, size))?;
    let result = result.iter().map(|d| *d as u8 as char).collect();
    Ok(result)
}

const DATA: &str = "10010000000110000";

fn main() -> Result<(), Error> {
    let result_1 = process(DATA, 272)?;
    println!("Part 1: {result_1:?}");

    let result_2 = process(DATA, 35_651_584)?;
    println!("Part 2: {result_2:?}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST: &str = "10000";

    #[test]
    fn test_solve_1_test() {
        assert_eq!(process(TEST, 20), Ok("01100".to_string()));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(process(DATA, 272), Ok("10010110010011110".to_string()));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(
            process(DATA, 35_651_584),
            Ok("01101011101100011".to_string())
        );
    }
}
