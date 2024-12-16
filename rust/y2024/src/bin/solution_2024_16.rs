use std::collections::HashSet;

use advent_of_code_common::direction::Direction;
use advent_of_code_common::grid2d::{Coords, Grid2D, MatrixGrid2D};
use advent_of_code_common::parsing::Error;
use advent_of_code_common::rotation::Rotation;
use pathfinding::prelude::{astar, astar_bag};

const DATA: &str = include_str!("../../resources/16.txt");

type R = i32;

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
struct State {
    position:  Coords,
    direction: Direction,
}

impl State {
    fn go_straight(&self) -> Self {
        Self {
            position:  self.position + self.direction,
            direction: self.direction,
        }
    }

    fn turn(&self, rotation: Rotation) -> Self {
        Self {
            position:  self.position,
            direction: self.direction.rotate(rotation),
        }
    }

    fn successors(&self, field: &MatrixGrid2D<bool>) -> Vec<(State, R)> {
        vec![
            (self.go_straight(), 1),
            (self.turn(Rotation::Left90), 1000),
            (self.turn(Rotation::Right90), 1000),
        ]
        .into_iter()
        .filter(|(s, _)| field.get(s.position) == Some(&false))
        .collect()
    }

    fn heuristic(&self, end: Coords) -> R {
        self.position.manhattan_distance(end) as R
    }
}

type Input = (State, MatrixGrid2D<bool>, Coords);

fn parse(input: &str) -> Result<Input, Error> {
    let char_field = MatrixGrid2D::parse_char_field(input);
    let start = char_field
        .find_coords_by_value(&'S')
        .ok_or("Start not found")?;
    let end = char_field
        .find_coords_by_value(&'E')
        .ok_or("End not found")?;
    let field = char_field.map_by_values(|c| {
        match c {
            '#' => true,
            'S' | 'E' | '.' => false,
            _ => panic!("Unknown character {c} in field"),
        }
    });
    Ok((
        State {
            position:  start,
            direction: Direction::East,
        },
        field,
        end,
    ))
}

fn solve_1(data: &Input) -> R {
    let (state, field, end) = data;
    let (_, result) = astar(
        state,
        |s| s.successors(field),
        |s| s.heuristic(*end),
        |c| c.position == *end,
    )
    .expect("Expected a solution");
    result
}

fn solve_2(data: &Input) -> usize {
    let (state, field, end) = data;
    let (results, _) = astar_bag(
        state,
        |s| s.successors(field),
        |s| s.heuristic(*end),
        |c| c.position == *end,
    )
    .expect("Expected a solution");

    let mut visited = HashSet::new();
    for result in results {
        for s in result {
            visited.insert(s.position);
        }
    }
    visited.len()
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

    const TEST_DATA_0: &str = include_str!("../../resources/16-test-00.txt");
    const TEST_DATA_1: &str = include_str!("../../resources/16-test-01.txt");

    fn test_data_0() -> Input {
        parse(TEST_DATA_0).unwrap()
    }

    fn test_data_1() -> Input {
        parse(TEST_DATA_1).unwrap()
    }

    fn real_data() -> Input {
        parse(DATA).unwrap()
    }

    #[test]
    fn test_solve_1_test_0() {
        assert_eq!(solve_1(&test_data_0()), 7036);
    }

    #[test]
    fn test_solve_1_test_1() {
        assert_eq!(solve_1(&test_data_1()), 11048);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(&real_data()), 74392);
    }

    #[test]
    fn test_solve_2_test_0() {
        assert_eq!(solve_2(&test_data_0()), 45);
    }

    #[test]
    fn test_solve_2_test_1() {
        assert_eq!(solve_2(&test_data_1()), 64);
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(&real_data()), 426);
    }
}
