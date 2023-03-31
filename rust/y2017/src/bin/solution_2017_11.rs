use std::cmp::max;

use advent_of_code_common::coords_hex::CoordsHex;
use advent_of_code_common::direction_hex::DirectionHex;
use advent_of_code_common::parsing::{parse_separated_vec, Error};

fn solve_1(directions: Vec<DirectionHex>) -> i16 {
    let mut location: CoordsHex<i16> = CoordsHex::origin();
    for direction in directions {
        location = location.move_in_direction(direction);
    }
    location.manhattan_distance_to_origin()
}

fn solve_2(directions: Vec<DirectionHex>) -> i16 {
    let mut location: CoordsHex<i16> = CoordsHex::origin();
    let mut max_distance = 0;
    for direction in directions {
        location = location.move_in_direction(direction);
        max_distance = max(max_distance, location.manhattan_distance_to_origin());
    }
    max_distance
}

fn parse(input: &str) -> Result<Vec<DirectionHex>, Error> {
    parse_separated_vec(input, ",")
}

fn part_1(input: &str) -> Result<i16, Error> {
    parse(input).map(solve_1)
}

fn part_2(input: &str) -> Result<i16, Error> {
    parse(input).map(solve_2)
}

const DATA: &str = include_str!("../../resources/11.txt");

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
    fn test_solve_1_test_1() {
        assert_eq!(part_1("ne,ne,ne"), Ok(3));
    }

    #[test]
    fn test_solve_1_test_2() {
        assert_eq!(part_1("ne,ne,sw,sw"), Ok(0));
    }

    #[test]
    fn test_solve_1_test_3() {
        assert_eq!(part_1("ne,ne,s,s"), Ok(2));
    }

    #[test]
    fn test_solve_1_test_4() {
        assert_eq!(part_1("se,sw,se,sw,sw"), Ok(3));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(698));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(1435));
    }
}
