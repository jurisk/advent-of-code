use std::cmp::Ordering;
use std::str::FromStr;

use advent_of_code_common::direction::Direction;
use advent_of_code_common::grid2d::Coords;
use advent_of_code_common::parsing::{Error, parse_lines_to_vec};
use itertools::Itertools;
use memoize::memoize;

const DATA: &str = include_str!("../../resources/21.txt");

type R = usize;

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
enum NumericButton {
    Number(u8),
    Activate,
    Invalid,
}

impl NumericButton {
    fn coords(self) -> Coords {
        match self {
            NumericButton::Number(0) => Coords::new(1, 3),
            NumericButton::Number(1) => Coords::new(0, 2),
            NumericButton::Number(2) => Coords::new(1, 2),
            NumericButton::Number(3) => Coords::new(2, 2),
            NumericButton::Number(4) => Coords::new(0, 1),
            NumericButton::Number(5) => Coords::new(1, 1),
            NumericButton::Number(6) => Coords::new(2, 1),
            NumericButton::Number(7) => Coords::new(0, 0),
            NumericButton::Number(8) => Coords::new(1, 0),
            NumericButton::Number(9) => Coords::new(2, 0),
            NumericButton::Number(n) => {
                panic!("Invalid number {n}")
            },
            NumericButton::Activate => Coords::new(2, 3),
            NumericButton::Invalid => Coords::new(0, 3),
        }
    }

    fn path_to(self, b: Self) -> impl Iterator<Item = Vec<DirectionalButton>> {
        path_between_coords(self.coords(), b.coords(), Self::Invalid.coords())
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
enum DirectionalButton {
    Arrow(Direction),
    Activate,
    Invalid,
}

impl DirectionalButton {
    fn coords(self) -> Coords {
        match self {
            DirectionalButton::Arrow(Direction::North) => Coords::new(1, 0),
            DirectionalButton::Arrow(Direction::East) => Coords::new(2, 1),
            DirectionalButton::Arrow(Direction::South) => Coords::new(1, 1),
            DirectionalButton::Arrow(Direction::West) => Coords::new(0, 1),
            DirectionalButton::Activate => Coords::new(2, 0),
            DirectionalButton::Invalid => Coords::new(0, 0),
        }
    }

    fn path_to(self, b: Self) -> impl Iterator<Item = Vec<DirectionalButton>> {
        path_between_coords(self.coords(), b.coords(), Self::Invalid.coords())
    }
}

#[expect(clippy::cast_sign_loss)]
fn path_between_coords(
    a: Coords,
    b: Coords,
    invalid: Coords,
) -> impl Iterator<Item = Vec<DirectionalButton>> {
    let diff = b - a;
    let for_x = match diff.x.cmp(&0) {
        Ordering::Less => {
            vec![DirectionalButton::Arrow(Direction::West); -diff.x as usize]
        },
        Ordering::Equal => {
            vec![]
        },
        Ordering::Greater => {
            vec![DirectionalButton::Arrow(Direction::East); diff.x as usize]
        },
    };

    let for_y = match diff.y.cmp(&0) {
        Ordering::Less => {
            vec![DirectionalButton::Arrow(Direction::North); -diff.y as usize]
        },
        Ordering::Equal => {
            vec![]
        },
        Ordering::Greater => {
            vec![DirectionalButton::Arrow(Direction::South); diff.y as usize]
        },
    };

    let valid_path = move |path: &Vec<DirectionalButton>| -> bool {
        let mut current = a;
        for b in path {
            if let DirectionalButton::Arrow(d) = b {
                current += d.diff();
                if current == invalid {
                    return false;
                }
            }
        }
        true
    };

    vec![
        [for_x.clone(), for_y.clone(), vec![
            DirectionalButton::Activate,
        ]]
        .concat(),
        [for_y, for_x, vec![DirectionalButton::Activate]].concat(),
    ]
    .into_iter()
    .filter(valid_path)
}

#[memoize]
fn move_between_directional_buttons(
    a: DirectionalButton,
    b: DirectionalButton,
    robot_directional_keyboards: usize,
) -> R {
    move_between_buttons(a.path_to(b), robot_directional_keyboards)
}

fn move_between_buttons(
    iter: impl IntoIterator<Item = Vec<DirectionalButton>>,
    robot_directional_keyboards: usize,
) -> R {
    iter.into_iter()
        .map(|path| move_cost(path, robot_directional_keyboards))
        .min()
        .expect("Expected at least one path")
}

fn move_cost(path: Vec<DirectionalButton>, robot_directional_keyboards: usize) -> R {
    if robot_directional_keyboards == 0 {
        path.len()
    } else {
        [vec![DirectionalButton::Activate], path]
            .concat()
            .into_iter()
            .tuple_windows()
            .map(|(a, b)| move_between_directional_buttons(a, b, robot_directional_keyboards - 1))
            .sum()
    }
}

#[memoize]
fn move_between_numeric_buttons(
    a: NumericButton,
    b: NumericButton,
    robot_directional_keyboards: usize,
) -> R {
    move_between_buttons(a.path_to(b), robot_directional_keyboards)
}

type Input = Vec<Code>;

#[derive(Clone)]
struct Code {
    buttons: Vec<NumericButton>,
}

impl Code {
    fn numeric_part(&self) -> R {
        self.buttons
            .iter()
            .filter_map(|button| {
                match button {
                    NumericButton::Number(n) => Some(*n as R),
                    _ => None,
                }
            })
            .fold(0, |acc, n| acc * 10 + n)
    }

    fn best_human_presses_length(self, robot_directional_keyboards: usize) -> R {
        [vec![NumericButton::Activate], self.buttons]
            .concat()
            .into_iter()
            .tuple_windows()
            .map(|(a, b)| move_between_numeric_buttons(a, b, robot_directional_keyboards))
            .sum()
    }

    fn complexity(self, robot_directional_keyboards: usize) -> R {
        self.numeric_part() * self.best_human_presses_length(robot_directional_keyboards)
    }
}

impl FromStr for Code {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let buttons: Result<Vec<NumericButton>, Self::Err> = s
            .chars()
            .map(|c| {
                match c {
                    x if x.is_ascii_digit() => Ok(NumericButton::Number(x as u8 - b'0')),
                    'A' => Ok(NumericButton::Activate),
                    _ => Err(format!("Invalid button: {c}")),
                }
            })
            .collect();

        let buttons = buttons?;
        Ok(Code { buttons })
    }
}

fn parse(input: &str) -> Result<Input, Error> {
    parse_lines_to_vec(input)
}

fn solve(data: Input, robot_directional_keyboards: usize) -> R {
    data.into_iter()
        .map(|code| code.complexity(robot_directional_keyboards))
        .sum()
}

fn solve_1(data: Input) -> R {
    solve(data, 2)
}

fn solve_2(data: Input) -> R {
    solve(data, 25)
}

fn main() -> Result<(), Error> {
    let data = parse(DATA)?;

    let result_1 = solve_1(data.clone());
    println!("Part 1: {result_1}");

    let result_2 = solve_2(data);
    println!("Part 2: {result_2}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = include_str!("../../resources/21-test-00.txt");

    fn test_data() -> Input {
        parse(TEST_DATA).unwrap()
    }

    fn real_data() -> Input {
        parse(DATA).unwrap()
    }

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(test_data()), 126_384);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(real_data()), 270_084);
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve_2(test_data()), 154_115_708_116_294);
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(real_data()), 329_431_019_997_766);
    }
}
