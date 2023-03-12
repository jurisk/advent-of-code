use std::str::FromStr;

use advent_of_code_common::coords2d::Coords2D;
use advent_of_code_common::parsing::{parse_lines_to_vec, parse_str, Error};
use pathfinding::matrix::Matrix;

use crate::Command::{Rect, RotateColumn, RotateRow};

#[derive(Debug, Copy, Clone)]
enum Command {
    Rect { a: usize, b: usize },
    RotateRow { y: usize, by: usize },
    RotateColumn { x: usize, by: usize },
}

impl Command {
    fn apply_to_matrix(self, matrix: &mut Matrix<bool>) {
        match self {
            Rect { a, b } => {
                for x in 0 .. a {
                    for y in 0 .. b {
                        if let Some(v) = matrix.get_mut((y, x)) {
                            *v = true;
                        }
                    }
                }
            },
            RotateRow { y, by } => {
                let n = matrix.columns;
                let vec: Vec<bool> = (0 .. n)
                    .map(|x| matrix.get((y, x)).unwrap())
                    .copied()
                    .collect();
                for (x, value) in vec.iter().enumerate() {
                    let new_x = (x + by) % n;
                    if let Some(v) = matrix.get_mut((y, new_x)) {
                        *v = *value;
                    }
                }
            },
            RotateColumn { x, by } => {
                let n = matrix.rows;
                let vec: Vec<bool> = (0 .. n)
                    .map(|y| matrix.get((y, x)).unwrap())
                    .copied()
                    .collect();
                for (y, value) in vec.iter().enumerate() {
                    let new_y = (y + by) % n;
                    if let Some(v) = matrix.get_mut((new_y, x)) {
                        *v = *value;
                    }
                }
            },
        }
    }
}

type Data = Vec<Command>;

impl FromStr for Command {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(value) = s.strip_prefix("rect ") {
            let v: Vec<&str> = value.splitn(2, 'x').collect();
            let a = parse_str(v[0])?;
            let b = parse_str(v[1])?;
            Ok(Rect { a, b })
        } else if let Some(value) = s.strip_prefix("rotate row y=") {
            let v: Vec<&str> = value.splitn(2, " by ").collect();
            let y = parse_str(v[0])?;
            let by = parse_str(v[1])?;
            Ok(RotateRow { y, by })
        } else if let Some(value) = s.strip_prefix("rotate column x=") {
            let v: Vec<&str> = value.splitn(2, " by ").collect();
            let x = parse_str(v[0])?;
            let by = parse_str(v[1])?;
            Ok(RotateColumn { x, by })
        } else {
            Err(format!("Unrecognized {}", s))
        }
    }
}

fn solve_1(data: &Data, dimensions: &Coords2D<usize>) -> usize {
    let mut matrix = Matrix::new(dimensions.y, dimensions.x, false);

    for c in data {
        println!("Processing: {c:?}");
        c.apply_to_matrix(&mut matrix);
    }

    for y in 0 .. matrix.rows {
        for x in 0 .. matrix.columns {
            let cell = match matrix.get((y, x)) {
                Some(true) => 'X',
                Some(false) => ' ',
                None => '?',
            };
            print!("{}", cell)
        }
        println!()
    }

    matrix.values().filter(|x| **x).count()
}

fn parse(input: &str) -> Result<Data, Error> {
    parse_lines_to_vec(input)
}

fn part_1(input: &str, dimensions: &Coords2D<usize>) -> Result<usize, Error> {
    parse(input).map(|input| solve_1(&input, dimensions))
}

const DATA: &str = include_str!("../../resources/08.txt");
const DATA_AREA: Coords2D<usize> = Coords2D::new(50, 6);

fn main() -> Result<(), Error> {
    let result_1 = part_1(DATA, &DATA_AREA)?;
    println!("Part 1: {result_1}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST: &str = include_str!("../../resources/08-test.txt");
    const TEST_AREA: Coords2D<usize> = Coords2D::new(7, 3);

    #[test]
    fn test_solve_1_test() {
        assert_eq!(part_1(TEST, &TEST_AREA), Ok(6));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA, &DATA_AREA), Ok(110));
    }
}
