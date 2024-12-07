use std::fmt::{Debug, Formatter};

use advent_of_code_common::coords2d::{from_u32, to_u32};
use advent_of_code_common::direction::Direction;
use advent_of_code_common::grid2d::{Coords, Grid2D, MatrixGrid2D};
use advent_of_code_common::mutable_bit_set::MutableBitSet;
use advent_of_code_common::rotation::Rotation;
use advent_of_code_common::set::Set;
use advent_of_code_common::simulate::{
    SimulationOutcome, SimulationStepResult, until_repeats_or_finishes_using_bit_set,
};

use crate::Block::{Empty, Wall};

const DATA: &str = include_str!("../../resources/06.txt");

type R = usize;
type Data = (Coords, MatrixGrid2D<Block>);

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
struct Guard {
    location:  Coords,
    direction: Direction,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Block {
    Wall,
    Empty,
}

impl Debug for Block {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Wall => write!(f, "#"),
            Empty => write!(f, "."),
        }
    }
}

impl Guard {
    fn next(&self, field: &MatrixGrid2D<Block>) -> Option<Guard> {
        let next_location = self.location + self.direction;

        field.get(next_location).map(|wall| {
            match wall {
                Wall => {
                    Guard {
                        location:  self.location,
                        direction: self.direction.rotate(Rotation::Right90),
                    }
                },
                Empty => {
                    Guard {
                        location:  next_location,
                        direction: self.direction,
                    }
                },
            }
        })
    }
}

fn parse(input: &str) -> Data {
    let char_grid: MatrixGrid2D<char> = input.parse().expect("Failed to parse input");

    let field = char_grid.map_by_values(|c| {
        match c {
            '.' | '^' => Empty,
            '#' => Wall,
            _ => panic!("Unexpected character: {c}"),
        }
    });

    let location = char_grid
        .find_coords_by_value(&'^')
        .unwrap_or_else(|| panic!("No starting location found"));

    (location, field)
}

fn guards_path(location: Coords, field: &MatrixGrid2D<Block>) -> Vec<Coords> {
    let mut guard = Guard {
        location,
        direction: Direction::North,
    };

    // TODO: Do not use a fixed grid width, use field.width() instead
    let c_to_u32 = |c| to_u32(c, 10_000);
    let u32_to_c = |u| from_u32(u, 10_000);
    let mut visited = MutableBitSet::new(c_to_u32, u32_to_c);
    visited.insert(location);

    loop {
        match guard.next(field) {
            Some(next) => {
                visited.insert(next.location);
                guard = next;
            },
            None => return visited.into_iter().collect(),
        }
    }
}

fn solve_1(location: Coords, field: &MatrixGrid2D<Block>) -> R {
    guards_path(location, field).len()
}

fn solve_2(location: Coords, field: MatrixGrid2D<Block>) -> R {
    let todos = guards_path(location, &field)
        .into_iter()
        .filter(|c| *c != location)
        .collect::<Vec<_>>();

    let mut field = field;
    let mut result = 0;

    let g_to_u32 = |g: Guard| {
        // TODO: Do not use fixed width
        let location = to_u32(g.location, 10_000);
        let direction: u32 = g.direction.into();
        location * 4 + direction
    };

    for c in todos {
        field.set(c, Wall);

        let (outcome, ..) = until_repeats_or_finishes_using_bit_set(
            Guard {
                location,
                direction: Direction::North,
            },
            |guard| {
                match guard.next(&field) {
                    None => SimulationStepResult::Finished(guard),
                    Some(next) => SimulationStepResult::Continue(next),
                }
            },
            4 * field.len(),
            g_to_u32,
        );

        if outcome == SimulationOutcome::Repeats {
            result += 1;
        }

        field.set(c, Empty);
    }

    result
}

fn main() {
    let (location, field) = parse(DATA);

    let result_1 = solve_1(location, &field);
    println!("Part 1: {result_1}");

    let result_2 = solve_2(location, field);
    println!("Part 2: {result_2}");
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = include_str!("../../resources/06-test-00.txt");

    fn test_data() -> Data {
        parse(TEST_DATA)
    }

    fn real_data() -> Data {
        parse(DATA)
    }

    #[test]
    fn test_solve_1_test() {
        let (location, field) = test_data();
        assert_eq!(solve_1(location, &field,), 41);
    }

    #[test]
    fn test_solve_1_real() {
        let (location, field) = real_data();
        assert_eq!(solve_1(location, &field,), 5162);
    }

    #[test]
    fn test_solve_2_test() {
        let (location, field) = test_data();

        assert_eq!(solve_2(location, field,), 6);
    }

    #[test]
    fn test_solve_2_real() {
        let (location, field) = real_data();
        assert_eq!(solve_2(location, field,), 1909);
    }
}
