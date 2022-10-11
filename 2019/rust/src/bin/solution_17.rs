use advent_of_code::intcode::{parse_machine_code, Process};
use itertools::Itertools;
use num_enum::TryFromPrimitive;
use std::fmt;
use std::fmt::Formatter;
use std::ops::Add;

#[repr(u8)]
#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy, TryFromPrimitive)]
enum MapSquare {
    Empty = b'.',
    Scaffold = b'#',
    RobotPointingUp = b'^',
    RobotPointingRight = b'>',
    RobotPointingLeft = b'<',
    RobotPointingDown = b'v',
}

impl MapSquare {
    fn from_char(ch: char) -> MapSquare {
        MapSquare::try_from(ch as u8).unwrap_or_else(|_| panic!("Invalid char {}", ch))
    }

    fn is_scaffold(self) -> bool {
        !matches!(self, MapSquare::Empty)
    }

    fn is_robot(self) -> bool {
        !matches!(self, MapSquare::Scaffold | MapSquare::Empty)
    }
}

type Board = Vec<Vec<MapSquare>>;

trait BoardExt {
    fn is_intersection(&self, c: Coords) -> bool;
    fn at(&self, c: Coords) -> MapSquare;
    fn find_robot(&self) -> Coords;
}

impl BoardExt for Board {
    fn is_intersection(&self, c: Coords) -> bool {
        let mut coords: Vec<Coords> =  Direction::all().iter().map(|n| c + n.diff()).collect();
        coords.push(c);
        coords.iter().all(|x| self.at(*x).is_scaffold())
    }

    fn at(&self, c: Coords) -> MapSquare {
        self[c.y as usize][c.x as usize]
    }

    fn find_robot(&self) -> Coords {
        let robots: Vec<Coords> = (0..self.len() - 1).flat_map(|y|
            (0..self[y].len() -1).filter_map(move |x| {
                let sq = self[y][x];
                if sq.is_robot() {
                    Some(Coords { x: x as i32, y: y as i32 })
                } else {
                    None
                }
            })
        ).collect();

        assert_eq!(robots.len(), 1);
        robots[0]
    }
}

enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    fn all() -> Vec<Direction> {
        vec![
            Direction::Up,
            Direction::Down,
            Direction::Left,
            Direction::Right,
        ]
    }

    fn diff(&self) -> Coords {
        match self {
            Direction::Up => Coords { x: 0, y: 1 },
            Direction::Down => Coords { x: 0, y: -1 },
            Direction::Left => Coords { x: -1, y: 0 },
            Direction::Right => Coords { x: 1, y: 0 },
        }
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
struct Coords {
    x: i32,
    y: i32,
}

impl Add for Coords {
    type Output = Coords;

    fn add(self, rhs: Coords) -> Coords {
        Coords {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

type Route = Vec<Step>;
enum Step {
    F(usize),
    L,
    R,
}

impl Step {
    fn route_to_string(route: &Route) -> String {
        route.iter().map(|x| format!("{}", x)).join(",")
    }
}

impl fmt::Display for Step {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match *self {
            Step::F(n) => f.write_str(&format!("{}", n)),
            Step::L => write!(f, "L"),
            Step::R => write!(f, "R"),
        }
    }
}

fn parse_board(s: &str) -> Board {
    s.split('\n')
        .filter(|x| !x.is_empty())
        .map(|x| x.chars().map(MapSquare::from_char).collect())
        .collect()
}

fn sum_of_scaffold_intersection_alignment_parameters(board: &Board) -> i32 {
    let intersections: Vec<Coords> = (1..board.len() - 2)
        .flat_map(|y| {
            (1..board[y].len() - 2).filter_map(move |x| {
                let c = Coords {
                    x: x as i32,
                    y: y as i32,
                };
                if board.is_intersection(c) {
                    Some(c)
                } else {
                    None
                }
            })
        })
        .collect();

    intersections.iter().map(|x| x.x * x.y).sum()
}

fn program_as_str() -> String {
    include_str!("../../resources/17.txt").to_string()
}

fn board() -> Board {
    let mut process = Process::from_string(&program_as_str());
    process.run_to_halt();
    let board_as_str = process.output_as_ascii();
    println!("{}", &board_as_str);
    parse_board(&board_as_str)
}

fn solve_1() {
    let board = board();
    let result = sum_of_scaffold_intersection_alignment_parameters(&board);
    assert_eq!(result, 3336);
    println!("{}", result);
}

fn simple_path(board: &Board) -> Route {
    // TODO
    let robot_location = board.find_robot();
    println!("{:?} {:?}", robot_location, board.at(robot_location));
    vec![]
}

fn solve_2() {
    let board = board();
    let simple_path = simple_path(&board);
    println!("{}", Step::route_to_string(&simple_path));

    let mut program = parse_machine_code(&program_as_str());
    assert_eq!(program[0], 1);
    program[0] = 2;

    let mut process = Process::new(&program);
    process.provide_input_as_string("");
}

fn main() {
    solve_1();
    solve_2();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_small_1() {
        let data = "..#..........
..#..........
##O####...###
#.#...#...#.#
##O###O###O##
..#...#...#..
..#####...^.."
            .replace('O', "#");

        let result = sum_of_scaffold_intersection_alignment_parameters(&parse_board(&data));
        assert_eq!(result, 76);
    }

    #[test]
    fn test_simplest_path_2() {
        let data = "#######...#####
#.....#...#...#
#.....#...#...#
......#...#...#
......#...###.#
......#.....#.#
^########...#.#
......#.#...#.#
......#########
........#...#..
....#########..
....#...#......
....#...#......
....#...#......
....#####......";

        let board = parse_board(data);

        let expected_simple_path = "R,8,R,8,R,4,R,4,R,8,L,6,L,2,R,4,R,4,R,8,R,8,R,8,L,6,L,2";
        let obtained_simple_path = simple_path(&board);

        assert_eq!(
            expected_simple_path,
            Step::route_to_string(&obtained_simple_path)
        );
    }

    #[test]
    fn test_solve_1() {
        solve_1();
    }

    #[test]
    fn test_solve_2() {
        solve_2();
    }
}
