use advent_of_code_common::parsing::{Error, parse_matrix};
use num_enum::{IntoPrimitive, TryFromPrimitive};
use pathfinding::matrix::Matrix;

use crate::Square::{CucumberMovingEast, CucumberMovingSouth, Empty};

const DATA: &str = include_str!("../../resources/25.txt");

#[repr(u8)]
#[derive(Debug, Eq, PartialEq, Clone, TryFromPrimitive, IntoPrimitive, Copy)]
enum Square {
    CucumberMovingEast  = b'>',
    CucumberMovingSouth = b'v',
    Empty               = b'.',
}

type SeaFloor = Matrix<Square>;

fn parse_square(ch: char) -> Result<Square, Error> {
    Square::try_from(ch as u8).map_err(|err| format!("{err:?}"))
}

fn step<F>(sea_floor: &SeaFloor, move_cucumber: Square, adjust_coord: F) -> SeaFloor
where
    F: Fn((usize, usize)) -> (usize, usize),
{
    let mut result = Matrix::new(sea_floor.rows, sea_floor.columns, Empty);

    for r in 0 .. sea_floor.rows {
        for c in 0 .. sea_floor.columns {
            let coords = (r, c);
            let coords_plus_1 = adjust_coord(coords);
            match sea_floor[coords] {
                x if x == move_cucumber => {
                    if sea_floor[coords_plus_1] == Empty {
                        result[coords] = Empty;
                        result[coords_plus_1] = move_cucumber;
                    } else {
                        result[coords] = move_cucumber;
                    }
                },
                Empty => {},
                x => {
                    result[coords] = x;
                },
            }
        }
    }

    result
}

fn step_right(sea_floor: &SeaFloor) -> SeaFloor {
    step(sea_floor, CucumberMovingEast, |(r, c)| {
        (r, (c + 1) % sea_floor.columns)
    })
}

fn step_down(sea_floor: &SeaFloor) -> SeaFloor {
    step(sea_floor, CucumberMovingSouth, |(r, c)| {
        ((r + 1) % sea_floor.rows, c)
    })
}

fn do_step(sea_floor: &SeaFloor) -> SeaFloor {
    step_down(&step_right(sea_floor))
}

fn print_sea_floor(step: usize, sea_floor: &SeaFloor) {
    println!("After {step}");
    for r in 0 .. sea_floor.rows {
        let row: String = (0 .. sea_floor.columns)
            .map(|c| {
                let as_u8: u8 = sea_floor[(r, c)].into();
                as_u8 as char
            })
            .collect();
        println!("{row}");
    }
    println!("\n\n");
}

fn solve(input: &str) -> Result<usize, Error> {
    let mut sea_floor: SeaFloor = parse_matrix(input, parse_square)?;
    let mut steps = 0;

    loop {
        let after = do_step(&sea_floor);
        steps += 1;

        if sea_floor == after {
            print_sea_floor(steps, &after);
            return Ok(steps);
        }

        sea_floor = after;
    }
}

fn main() {
    let result_1 = solve(DATA);
    println!("Part 1: {result_1:?}");
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = include_str!("../../resources/25-test.txt");

    #[test]
    fn test_solve_test() {
        assert_eq!(solve(TEST_DATA), Ok(58));
    }

    #[test]
    fn test_solve_real() {
        assert_eq!(solve(DATA), Ok(474));
    }
}
