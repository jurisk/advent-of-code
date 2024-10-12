use std::collections::{HashMap, HashSet};

use advent_of_code_common::area2d::Area2D;
use advent_of_code_common::coords2d::Coords2D;
use advent_of_code_common::parsing::{Error, parse_lines_to_vec};
use recap::Recap;
use serde::Deserialize;

#[derive(Debug, Deserialize, Copy, Clone)]
enum Operation {
    #[serde(rename = "turn on")]
    TurnOn,
    #[serde(rename = "turn off")]
    TurnOff,
    #[serde(rename = "toggle")]
    Toggle,
}

type Coords = Coords2D<u16>;
type Area = Area2D<u16>;

#[derive(Debug, Deserialize, Recap, Copy, Clone)]
#[recap(
    regex = r#"^(?P<operation>[a-z\s]+) (?P<from_x>\d+),(?P<from_y>\d+) through (?P<to_x>\d+),(?P<to_y>\d+)$"#
)]
struct Command {
    operation: Operation,
    from_x:    u16,
    from_y:    u16,
    to_x:      u16,
    to_y:      u16,
}

impl Command {
    #[inline]
    fn from(self) -> Coords {
        Coords::new(self.from_x, self.from_y)
    }

    #[inline]
    fn to(self) -> Coords {
        Coords::new(self.to_x, self.to_y)
    }

    #[inline]
    fn area(self) -> Area {
        Area2D::new(self.from(), self.to())
    }
}

fn parse(input: &str) -> Result<Vec<Command>, Error> {
    parse_lines_to_vec(input)
}

fn solve_1(commands: &[Command]) -> usize {
    let mut lights: HashSet<Coords> = HashSet::new();
    for command in commands {
        for point in command.area().points() {
            match command.operation {
                Operation::TurnOn => {
                    lights.insert(point);
                },
                Operation::TurnOff => {
                    lights.remove(&point);
                },
                Operation::Toggle => {
                    if lights.contains(&point) {
                        lights.remove(&point);
                    } else {
                        lights.insert(point);
                    }
                },
            }
        }
    }

    lights.len()
}

fn solve_2(commands: &[Command]) -> u32 {
    let mut lights: HashMap<Coords, u8> = HashMap::new();
    for command in commands {
        for point in command.area().points() {
            let old_value = *lights.get(&point).unwrap_or(&0);
            let new_value = match command.operation {
                // The phrase turn on actually means that you should increase the brightness of
                // those lights by 1.
                Operation::TurnOn => old_value + 1,
                // The phrase turn off actually means that you should decrease the brightness of
                // those lights by 1, to a minimum of zero.
                Operation::TurnOff => old_value.saturating_sub(1),
                // The phrase toggle actually means that you should increase the brightness of those
                // lights by 2.
                Operation::Toggle => old_value + 2,
            };

            lights.insert(point, new_value);
        }
    }

    lights.values().map(|n| u32::from(*n)).sum()
}

fn part_1(input: &str) -> Result<usize, Error> {
    let data = parse(input)?;
    Ok(solve_1(&data))
}

fn part_2(input: &str) -> Result<u32, Error> {
    let data = parse(input)?;
    Ok(solve_2(&data))
}

const DATA: &str = include_str!("../../resources/06.txt");

fn main() -> Result<(), Error> {
    // NB - alternative solutions exist which may be more efficient, e.g. go through
    // each pixel and go through each command for it - this would avoid
    // allocating N x N memory.

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
    #[ignore]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(569999));
    }

    #[test]
    #[ignore]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(17836115));
    }
}
