use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter};

use advent_of_code_common::direction::Direction;
use advent_of_code_common::grid2d::{Coords, Grid2D, MatrixGrid2D};
use advent_of_code_common::parsing::{Error, split_into_two_segments_separated_by_double_newline};

const DATA: &str = include_str!("../../resources/15.txt");

type R = i32;

#[derive(Copy, Clone, Eq, PartialEq)]
enum Side {
    Left,
    Right,
}

impl Side {
    #[must_use]
    pub fn other_direction(self) -> Direction {
        match self {
            Side::Left => Direction::East,
            Side::Right => Direction::West,
        }
    }
}

#[derive(Copy, Clone, Default, Eq, PartialEq)]
enum Square {
    Empty,
    #[default]
    Wall,
    SmallBox,
    LargeBox(Side),
}

struct State<T: Grid2D<Square>> {
    robot: Coords,
    field: T,
}

impl<T: Grid2D<Square> + Clone> Clone for State<T> {
    fn clone(&self) -> Self {
        Self {
            robot: self.robot,
            field: self.field.clone(),
        }
    }
}

impl<T: Grid2D<Square>> State<T> {
    fn calculate_move_package(&self, c: Coords, direction: Direction) -> Option<HashSet<Coords>> {
        let next = c + direction;
        let vertical_move = matches!(direction, Direction::North | Direction::South);

        match self.field.get_or_default(next) {
            Square::Empty => Some(HashSet::new()),
            Square::Wall => None,
            Square::LargeBox(side) if vertical_move => {
                let other_c = next + side.other_direction();
                match (
                    self.calculate_move_package(next, direction),
                    self.calculate_move_package(other_c, direction),
                ) {
                    (Some(a), Some(b)) => {
                        let mut result = HashSet::new();
                        result.insert(next);
                        result.insert(other_c);
                        result.extend(a);
                        result.extend(b);
                        Some(result)
                    },
                    _ => None,
                }
            },
            Square::LargeBox(_) | Square::SmallBox => {
                self.calculate_move_package(next, direction)
                    .map(|mut result| {
                        result.insert(next);
                        result
                    })
            },
        }
    }

    fn move_many(&mut self, move_package: HashSet<Coords>, direction: Direction) {
        let mut pending = HashMap::new();
        for c in move_package {
            if let Some(existing) = self.field.get(c) {
                pending.insert(c, *existing);
            }
            self.field.set(c, Square::Empty);
        }

        for (c, v) in pending {
            self.field.set(c + direction, v);
        }
    }

    fn move_robot_if_possible(&mut self, direction: Direction) {
        let new_robot = self.robot + direction;
        if self.field.get(new_robot) == Some(&Square::Empty) {
            self.robot = new_robot;
        }
    }

    fn make_move(&mut self, direction: Direction) {
        let move_package = self
            .calculate_move_package(self.robot, direction)
            .unwrap_or_default();
        self.move_many(move_package, direction);
        self.move_robot_if_possible(direction);
    }
}

impl<T: Grid2D<Square>> Debug for State<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut char_field = self.field.map_by_values(|s| {
            match s {
                Square::Empty => '.',
                Square::Wall => '#',
                Square::SmallBox => 'O',
                Square::LargeBox(Side::Left) => '[',
                Square::LargeBox(Side::Right) => ']',
            }
        });

        char_field.set(self.robot, '@');

        char_field.fmt(f)
    }
}

type Input = (State<MatrixGrid2D<Square>>, Vec<Direction>);

fn parse(input: &str) -> Result<Input, Error> {
    let (a, b) = split_into_two_segments_separated_by_double_newline(input)?;

    let directions: Vec<Direction> = b.chars().flat_map(Direction::try_from_caret).collect();

    let char_field: MatrixGrid2D<char> = MatrixGrid2D::parse_char_field(&a);

    let robot = char_field
        .find_coords_by_value(&'@')
        .ok_or("Robot not found")?;

    let field = char_field.map_by_values(|&c| {
        match c {
            '.' | '@' => Square::Empty,
            '#' => Square::Wall,
            'O' => Square::SmallBox,
            _ => panic!("Invalid character"),
        }
    });

    let state = State { robot, field };

    let result = (state, directions);
    Ok(result)
}

fn solve_1(data: &Input) -> R {
    let (state, directions) = data;
    let mut state: State<MatrixGrid2D<Square>> = state.clone();
    println!("Initial state:\n{state:?}\n");
    for direction in directions {
        state.make_move(*direction);
    }
    println!("Final state:\n{state:?}\n");
    state
        .field
        .iter()
        .filter_map(|(coords, value)| {
            match value {
                Square::LargeBox(Side::Left) | Square::SmallBox => Some(100 * coords.y + coords.x),
                _ => None,
            }
        })
        .sum()
}

fn solve_2(data: &Input) -> R {
    let (incoming_state, directions) = data;
    let incoming_field = &incoming_state.field;
    let field = incoming_field.flat_map_by_values(2, 1, |v| {
        match v {
            Square::Empty => MatrixGrid2D::from_vec(vec![vec![Square::Empty, Square::Empty]]),
            Square::Wall => MatrixGrid2D::from_vec(vec![vec![Square::Wall, Square::Wall]]),
            Square::SmallBox => {
                MatrixGrid2D::from_vec(vec![vec![
                    Square::LargeBox(Side::Left),
                    Square::LargeBox(Side::Right),
                ]])
            },
            Square::LargeBox(_) => panic!("Unexpected LargeBox"),
        }
    });
    let new_robot = Coords::new(incoming_state.robot.x * 2, incoming_state.robot.y);
    let state = State {
        robot: new_robot,
        field,
    };
    solve_1(&(state, directions.clone()))
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

    const TEST_DATA_0: &str = include_str!("../../resources/15-test-00.txt");
    const TEST_DATA_1: &str = include_str!("../../resources/15-test-01.txt");
    const TEST_DATA_2: &str = include_str!("../../resources/15-test-02.txt");

    fn test_data_0() -> Input {
        parse(TEST_DATA_0).unwrap()
    }

    fn test_data_1() -> Input {
        parse(TEST_DATA_1).unwrap()
    }

    fn test_data_2() -> Input {
        parse(TEST_DATA_2).unwrap()
    }

    fn real_data() -> Input {
        parse(DATA).unwrap()
    }

    #[test]
    fn test_solve_1_test_0() {
        assert_eq!(solve_1(&test_data_0()), 2028);
    }

    #[test]
    fn test_solve_1_test_1() {
        assert_eq!(solve_1(&test_data_1()), 10092);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(&real_data()), 1_577_255);
    }

    #[test]
    fn test_solve_2_test_1() {
        assert_eq!(solve_2(&test_data_1()), 9021);
    }

    #[test]
    fn test_solve_2_test_2() {
        assert_eq!(solve_2(&test_data_2()), 618);
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(&real_data()), 1_597_035);
    }
}
