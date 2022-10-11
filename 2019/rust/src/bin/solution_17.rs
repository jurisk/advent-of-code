use advent_of_code::intcode::{parse_machine_code, Process};
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

fn find_robot(map_squares: &Vec<Vec<MapSquare>>) -> Coords {
    let robots: Vec<Coords> = (0..map_squares.len())
        .flat_map(|y| {
            (0..map_squares[y].len()).filter_map(move |x| {
                let sq = map_squares[y][x];
                if sq.is_robot() {
                    Some(Coords {
                        x: x as i32,
                        y: y as i32,
                    })
                } else {
                    None
                }
            })
        })
        .collect();

    assert_eq!(robots.len(), 1);
    robots[0]
}

struct Board {
    scaffolds: Vec<Vec<bool>>,
    robot_location: Coords,
    robot_direction: Direction,
}

impl Board {
    fn parse(s: &str) -> Board {
        let map_squares: Vec<Vec<MapSquare>> = s
            .split('\n')
            .filter(|x| !x.is_empty())
            .map(|x| x.chars().map(MapSquare::from_char).collect())
            .collect();

        let robot_location = find_robot(&map_squares);
        let robot_square = map_squares[robot_location.y as usize][robot_location.x as usize];
        let robot_direction = match robot_square {
            MapSquare::RobotPointingUp => Direction::Up,
            MapSquare::RobotPointingRight => Direction::Right,
            MapSquare::RobotPointingLeft => Direction::Left,
            MapSquare::RobotPointingDown => Direction::Down,
            sq => panic!("Unexpected map square {:?}", sq),
        };

        let scaffolds = map_squares
            .iter()
            .map(|r| r.iter().map(|sq| sq.is_scaffold()).collect())
            .collect();

        Board {
            scaffolds,
            robot_location,
            robot_direction,
        }
    }

    fn all_coordinates(&self) -> Vec<Coords> {
        (0..self.scaffolds.len())
            .flat_map(|y| {
                (0..self.scaffolds[y].len())
                    .map(|x| Coords {
                        x: x as i32,
                        y: y as i32,
                    })
                    .collect::<Vec<Coords>>()
            })
            .collect()
    }

    fn is_intersection(&self, c: Coords) -> bool {
        let mut coords: Vec<Coords> = Direction::all().iter().map(|n| c + n.diff()).collect();
        coords.push(c);
        coords.iter().all(|x| self.is_scaffold(*x))
    }

    fn is_scaffold(&self, c: Coords) -> bool {
        *self
            .scaffolds
            .get(c.y as usize)
            .map_or(&false, |r| r.get(c.x as usize).unwrap_or(&false))
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
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

struct Route {
    steps: Vec<Step>,
}

enum Step {
    F(usize),
    L,
    R,
}

impl fmt::Display for Route {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(
            &self
                .steps
                .iter()
                .map(|x| format!("{}", x))
                .collect::<Vec<_>>()
                .join(","),
        )
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

fn sum_of_scaffold_intersection_alignment_parameters(board: &Board) -> i32 {
    board
        .all_coordinates()
        .iter()
        .filter(|c| board.is_intersection(**c))
        .map(|x| x.x * x.y)
        .sum()
}

fn program_as_str() -> String {
    include_str!("../../resources/17.txt").to_string()
}

fn board() -> Board {
    let mut process = Process::from_string(&program_as_str());
    process.run_to_halt();
    let board_as_str = process.output_as_ascii();
    println!("{}", &board_as_str);
    Board::parse(&board_as_str)
}

fn solve_1() {
    let board = board();
    let result = sum_of_scaffold_intersection_alignment_parameters(&board);
    assert_eq!(result, 3336);
    println!("{}", result);
}

fn simple_path(board: &Board) -> Route {
    // TODO
    println!("{:?} {:?}", board.robot_location, board.robot_direction);
    Route { steps: vec![] }
}

fn solve_2() {
    let board = board();
    let simple_path = simple_path(&board);
    println!("{}", simple_path);

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

        let result = sum_of_scaffold_intersection_alignment_parameters(&Board::parse(&data));
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

        let board = Board::parse(data);

        let expected_simple_path = "R,8,R,8,R,4,R,4,R,8,L,6,L,2,R,4,R,4,R,8,R,8,R,8,L,6,L,2";
        let obtained_simple_path = simple_path(&board);

        assert_eq!(expected_simple_path, format!("{}", obtained_simple_path),);
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
