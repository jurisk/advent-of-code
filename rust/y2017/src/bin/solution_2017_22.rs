#![allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]

use std::collections::HashMap;

use advent_of_code_common::coords2d::Coords2D;
use advent_of_code_common::direction::Direction;
use advent_of_code_common::parsing::{parse_lines_to_vec, Error};
use advent_of_code_common::rotation::Rotation;

#[derive(Default, Debug, Copy, Clone, Eq, PartialEq)]
enum NodeState {
    #[default]
    Clean,
    Weakened,
    Infected,
    Flagged,
}

impl NodeState {
    fn rotation(self) -> Rotation {
        match self {
            NodeState::Clean => Rotation::Left90,
            NodeState::Weakened => Rotation::NoRotation,
            NodeState::Infected => Rotation::Right90,
            NodeState::Flagged => Rotation::TurnAround,
        }
    }
}

type Coords = Coords2D<i32>;
type Grid = HashMap<Coords, NodeState>;

struct State {
    location:   Coords,
    direction:  Direction,
    infected:   Grid,
    infections: usize,
}

impl State {
    fn new(infected: Grid) -> Self {
        State {
            location: Coords2D::origin(),
            direction: Direction::North,
            infected,
            infections: 0,
        }
    }

    fn do_burst<F>(&mut self, transition_function: F)
    where
        F: Fn(NodeState) -> NodeState,
    {
        let current_state = *self
            .infected
            .get(&self.location)
            .unwrap_or(&NodeState::default());

        self.direction = self.direction.rotate(current_state.rotation());

        let next_state = transition_function(current_state);

        if next_state == NodeState::default() {
            _ = self.infected.remove(&self.location);
        } else {
            _ = self.infected.insert(self.location, next_state);
        }

        if next_state == NodeState::Infected {
            self.infections += 1;
        }

        self.location = self.location.move_in_direction(self.direction);
    }
}

fn parse(input: &str) -> Result<Grid, Error> {
    let lines: Vec<String> = parse_lines_to_vec(input)?;

    if lines.len() % 2 != 1 {
        return Err("Invalid number of lines".to_string());
    }

    let mut result: Grid = Grid::new();
    for (line_id, line) in lines.iter().enumerate() {
        if line.len() % 2 != 1 {
            return Err(format!("Invalid line length at line {}", line_id + 1));
        }

        for (char_id, char) in line.chars().enumerate() {
            let row = line_id as i32;
            let column = char_id as i32;
            let coords = Coords::new(
                column - (line.len() / 2) as i32,
                row - ((lines.len() / 2) as i32),
            );
            match char {
                '#' => _ = result.insert(coords, NodeState::Infected),
                '.' => {},
                _ => {
                    return Err(format!(
                        "Unrecognized char '{}' at line {}, column {}",
                        char,
                        line_id + 1,
                        char_id + 1
                    ));
                },
            }
        }
    }

    Ok(result)
}

fn solve<F>(infected: Grid, bursts: usize, transition_function: F) -> usize
where
    F: Fn(NodeState) -> NodeState,
{
    let mut state = State::new(infected);

    for _ in 0 .. bursts {
        state.do_burst(&transition_function);
    }

    state.infections
}
fn part_1(input: &str, bursts: usize) -> Result<usize, Error> {
    let infected = parse(input)?;
    Ok(solve(infected, bursts, |x| {
        match x {
            NodeState::Clean => NodeState::Infected,
            NodeState::Infected => NodeState::Clean,
            _ => panic!("Unexpected state {x:?}"),
        }
    }))
}

fn part_2(input: &str, bursts: usize) -> Result<usize, Error> {
    let infected = parse(input)?;
    Ok(solve(infected, bursts, |x| {
        match x {
            NodeState::Clean => NodeState::Weakened,
            NodeState::Weakened => NodeState::Infected,
            NodeState::Infected => NodeState::Flagged,
            NodeState::Flagged => NodeState::Clean,
        }
    }))
}

const DATA: &str = include_str!("../../resources/22.txt");

fn main() -> Result<(), Error> {
    let result_1 = part_1(DATA, 10_000)?;
    println!("Part 1: {result_1}");

    let result_2 = part_2(DATA, 10_000_000)?;
    println!("Part 2: {result_2}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST: &str = include_str!("../../resources/22-test.txt");

    #[test]
    fn test_solve_1_test_1() {
        assert_eq!(part_1(TEST, 7), Ok(5));
    }

    #[test]
    fn test_solve_1_test_2() {
        assert_eq!(part_1(TEST, 70), Ok(41));
    }

    #[test]
    fn test_solve_1_test_3() {
        assert_eq!(part_1(TEST, 10_000), Ok(5_587));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA, 10_000), Ok(5_411));
    }

    #[test]
    fn test_solve_2_test_1() {
        assert_eq!(part_2(TEST, 100), Ok(26));
    }

    #[test]
    fn test_solve_2_test_2() {
        assert_eq!(part_2(TEST, 10_000_000), Ok(2_511_944));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA, 10_000_000), Ok(2_511_416));
    }
}
