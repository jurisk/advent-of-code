use std::fmt::{Debug, Formatter};

use advent_of_code_common::direction_with_diagonals::DirectionWithDiagonals;
use advent_of_code_common::grid2d::{Coords, Grid2D, MatrixGrid2D};
use advent_of_code_common::parsing::{Error, parse_lines_to_vec};
use recap::Recap;
use serde::Deserialize;

const DATA: &str = include_str!("../../resources/14.txt");

type R = usize;
type Data = Vec<PositionAndVelocityFlat>;

#[derive(Recap, Deserialize, Debug, Copy, Clone)]
#[recap(
    regex = r#"^p=(?P<position_x>[-+]?\d+),(?P<position_y>[-+]?\d+) v=(?P<velocity_x>[-+]?\d+),(?P<velocity_y>[-+]?\d+)$"#
)]
struct PositionAndVelocityFlat {
    position_x: i32,
    position_y: i32,
    velocity_x: i32,
    velocity_y: i32,
}

#[derive(Copy, Clone, Debug)]
struct Robot {
    position: Coords,
    velocity: Coords,
}

impl From<PositionAndVelocityFlat> for Robot {
    fn from(value: PositionAndVelocityFlat) -> Self {
        Self {
            position: Coords::new(value.position_x, value.position_y),
            velocity: Coords::new(value.velocity_x, value.velocity_y),
        }
    }
}

fn parse(input: &str) -> Result<Data, Error> {
    parse_lines_to_vec(input)
}

struct GameState {
    robots: Vec<Robot>,
    width:  usize,
    height: usize,
}

impl Debug for GameState {
    #[expect(clippy::cast_possible_truncation)]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let grid = self.to_grid();
        let char_grid = grid.map_by_values(|value| {
            if *value == 0 {
                '.'
            } else if *value < 10 {
                std::char::from_digit(*value as u32, 10).expect("Invalid digit")
            } else {
                'X'
            }
        });
        char_grid.fmt(f)
    }
}

#[expect(clippy::cast_possible_wrap, clippy::cast_possible_truncation)]
fn wrap_to(value: i32, max: usize) -> i32 {
    value.rem_euclid(max as i32)
}

impl GameState {
    fn step(&mut self) {
        for robot in &mut self.robots {
            robot.position += robot.velocity;
            robot.position.x = wrap_to(robot.position.x, self.width);
            robot.position.y = wrap_to(robot.position.y, self.height);
        }
    }

    #[expect(clippy::cast_sign_loss)]
    fn count_robots(&self, direction: DirectionWithDiagonals) -> usize {
        self.robots
            .iter()
            .filter(|c| {
                let left = 0 .. self.width / 2;
                let right = self.width / 2 + 1 .. self.width;
                let top = 0 .. self.height / 2;
                let bottom = self.height / 2 + 1 .. self.height;

                match direction {
                    DirectionWithDiagonals::NW => {
                        left.contains(&(c.position.x as usize))
                            && top.contains(&(c.position.y as usize))
                    },
                    DirectionWithDiagonals::NE => {
                        right.contains(&(c.position.x as usize))
                            && top.contains(&(c.position.y as usize))
                    },
                    DirectionWithDiagonals::SE => {
                        right.contains(&(c.position.x as usize))
                            && bottom.contains(&(c.position.y as usize))
                    },
                    DirectionWithDiagonals::SW => {
                        left.contains(&(c.position.x as usize))
                            && bottom.contains(&(c.position.y as usize))
                    },
                    _ => false,
                }
            })
            .count()
    }

    fn to_grid(&self) -> MatrixGrid2D<usize> {
        let mut grid = MatrixGrid2D::new(self.width, self.height, 0usize);
        for robot in &self.robots {
            grid.modify(robot.position, |value| *value += 1);
        }
        grid
    }

    #[expect(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
    fn is_christmas_tree(&self) -> bool {
        let grid = self.to_grid();
        let diffs: usize = (0 .. self.height)
            .map(|row| {
                (0 .. self.width / 2)
                    .filter(|col| {
                        let c1 = Coords::new(*col as i32, row as i32);
                        let c2 = Coords::new((self.width - col - 1) as i32, row as i32);
                        grid.get(c1) != grid.get(c2)
                    })
                    .count()
            })
            .sum();
        diffs < 150
    }
}

fn solve<F>(data: &Data, width: usize, height: usize, finish: F) -> (GameState, usize)
where
    F: Fn((usize, &GameState)) -> bool,
{
    let robots: Vec<_> = data.iter().copied().map(Robot::from).collect();
    let mut state = GameState {
        robots,
        width,
        height,
    };
    let mut counter = 0;
    while !finish((counter, &state)) {
        state.step();
        counter += 1;
    }
    (state, counter)
}

fn solve_1(data: &Data, width: usize, height: usize) -> R {
    let (state, _counter) = solve(data, width, height, |(c, _)| c >= 100);
    state.count_robots(DirectionWithDiagonals::NW)
        * state.count_robots(DirectionWithDiagonals::NE)
        * state.count_robots(DirectionWithDiagonals::SW)
        * state.count_robots(DirectionWithDiagonals::SE)
}

fn solve_2(data: &Data, width: usize, height: usize) -> R {
    let (_state, counter) = solve(data, width, height, |(_, state)| state.is_christmas_tree());
    counter
}

fn main() -> Result<(), Error> {
    let data = parse(DATA)?;

    let result_1 = solve_1(&data, 101, 103);
    println!("Part 1: {result_1}");

    let result_2 = solve_2(&data, 101, 103);
    println!("Part 2: {result_2}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = include_str!("../../resources/14-test-00.txt");

    fn test_data() -> Data {
        parse(TEST_DATA).unwrap()
    }

    fn real_data() -> Data {
        parse(DATA).unwrap()
    }

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(&test_data(), 11, 7), 12);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(&real_data(), 101, 103), 221_655_456);
    }

    #[test]
    #[ignore]
    fn test_solve_2_real() {
        assert_eq!(solve_2(&real_data(), 101, 103), 7858);
    }
}
