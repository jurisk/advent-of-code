use std::cmp::min;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Formatter;

use advent_of_code_2019::intcode::{Process, parse_machine_code};
use advent_of_code_common::coords2d::Coords2D;
use advent_of_code_common::direction::Direction;
use itertools::Itertools;
use num_enum::TryFromPrimitive;

type Coords = Coords2D<i32>;

#[repr(u8)]
#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy, TryFromPrimitive)]
enum MapSquare {
    Empty              = b'.',
    Scaffold           = b'#',
    RobotPointingUp    = b'^',
    RobotPointingRight = b'>',
    RobotPointingLeft  = b'<',
    RobotPointingDown  = b'v',
}

impl MapSquare {
    fn from_char(ch: char) -> MapSquare {
        MapSquare::try_from(ch as u8).unwrap_or_else(|_| panic!("Invalid char {ch}"))
    }

    fn is_scaffold(self) -> bool {
        !matches!(self, MapSquare::Empty)
    }

    fn is_robot(self) -> bool {
        !matches!(self, MapSquare::Scaffold | MapSquare::Empty)
    }
}

fn find_robot(map_squares: &[Vec<MapSquare>]) -> Coords {
    let robots: Vec<Coords> = (0 .. map_squares.len())
        .flat_map(|y| {
            (0 .. map_squares[y].len()).filter_map(move |x| {
                let sq = map_squares[y][x];
                if sq.is_robot() {
                    Some(Coords2D {
                        x: i32::try_from(x).unwrap(),
                        y: i32::try_from(y).unwrap(),
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

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
struct Board {
    scaffolds:       Vec<Vec<bool>>,
    robot_location:  Coords,
    robot_direction: Direction,
}

impl Board {
    #[allow(clippy::cast_sign_loss)]
    fn parse(s: &str) -> Board {
        let map_squares: Vec<Vec<MapSquare>> = s
            .lines()
            .filter(|x| !x.is_empty())
            .map(|x| x.chars().map(MapSquare::from_char).collect())
            .collect();

        let robot_location = find_robot(&map_squares);
        let robot_square = map_squares[robot_location.y as usize][robot_location.x as usize];
        let robot_direction = match robot_square {
            MapSquare::RobotPointingUp => Direction::North,
            MapSquare::RobotPointingRight => Direction::East,
            MapSquare::RobotPointingLeft => Direction::West,
            MapSquare::RobotPointingDown => Direction::South,
            sq => panic!("Unexpected map square {sq:?}"),
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
        (0 .. self.scaffolds.len())
            .flat_map(|y| {
                (0 .. self.scaffolds[y].len())
                    .map(|x| {
                        Coords2D {
                            x: i32::try_from(x).unwrap(),
                            y: i32::try_from(y).unwrap(),
                        }
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

    #[allow(clippy::cast_sign_loss)]
    fn is_scaffold(&self, c: Coords) -> bool {
        *self
            .scaffolds
            .get(c.y as usize)
            .map_or(&false, |r| r.get(c.x as usize).unwrap_or(&false))
    }

    fn pointing_at_scaffolding(&self) -> bool {
        self.is_scaffold(self.robot_location + self.robot_direction.diff())
    }

    fn left_is_scaffolding(&self) -> bool {
        self.is_scaffold(self.robot_location + self.robot_direction.rotate_left().diff())
    }

    fn right_is_scaffolding(&self) -> bool {
        self.is_scaffold(self.robot_location + self.robot_direction.rotate_right().diff())
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
enum Step {
    F(usize),
    L,
    R,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
struct Route {
    steps: Vec<Step>,
}

impl Route {
    fn compress(&self) -> Route {
        let mut steps: Vec<Step> = vec![];
        let mut current: usize = 0;
        for step in &self.steps {
            match step {
                Step::F(n) => {
                    current += n;
                },
                Step::L => {
                    if current > 0 {
                        steps.push(Step::F(current));
                        current = 0;
                    }

                    steps.push(Step::L);
                },
                Step::R => {
                    if current > 0 {
                        steps.push(Step::F(current));
                        current = 0;
                    }

                    steps.push(Step::R);
                },
            }
        }

        if current > 0 {
            steps.push(Step::F(current));
        }

        Route { steps }
    }
}

impl fmt::Display for Route {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(
            &self
                .steps
                .iter()
                .map(|x| format!("{x}"))
                .collect::<Vec<_>>()
                .join(","),
        )
    }
}

impl fmt::Display for Step {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match *self {
            Step::F(n) => f.write_str(&format!("{n}")),
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
    let board_as_str = process.read_output_as_ascii();
    println!("{}", &board_as_str);
    Board::parse(&board_as_str)
}

fn solve_1() {
    let board = board();
    let result = sum_of_scaffold_intersection_alignment_parameters(&board);
    assert_eq!(result, 3336);
    println!("{result}");
}

fn compressed_path(board: &Board) -> Route {
    simple_path(board).compress()
}

fn simple_path(input_board: &Board) -> Route {
    let mut board: Board = input_board.clone();
    let mut steps: Vec<Step> = vec![];

    loop {
        if board.pointing_at_scaffolding() {
            steps.push(Step::F(1));
            board = Board {
                scaffolds:       board.scaffolds,
                robot_location:  board.robot_location + board.robot_direction.diff(),
                robot_direction: board.robot_direction,
            }
        } else if board.left_is_scaffolding() {
            steps.push(Step::L);
            steps.push(Step::F(1));
            let new_direction = board.robot_direction.rotate_left();
            board = Board {
                scaffolds:       board.scaffolds,
                robot_location:  board.robot_location + new_direction.diff(),
                robot_direction: new_direction,
            }
        } else if board.right_is_scaffolding() {
            steps.push(Step::R);
            steps.push(Step::F(1));
            let new_direction = board.robot_direction.rotate_right();
            board = Board {
                scaffolds:       board.scaffolds,
                robot_location:  board.robot_location + new_direction.diff(),
                robot_direction: board.robot_direction.rotate_right(),
            }
        } else {
            break;
        }
    }

    Route { steps }
}

type SubroutineName = char;

#[derive(PartialEq, Eq, Debug, Clone)]
struct WithSubroutines {
    main:        Vec<SubroutineName>,
    subroutines: HashMap<SubroutineName, Route>,
}

impl WithSubroutines {
    fn empty() -> WithSubroutines {
        WithSubroutines {
            main:        vec![],
            subroutines: HashMap::new(),
        }
    }

    fn extract_subroutines(
        steps_to_allocate: &[Step],
        subroutine_names_remaining: &[SubroutineName],
        acc: WithSubroutines,
    ) -> Option<WithSubroutines> {
        // println!("steps_to_allocate: {}\nsubroutine_names_remaining: {:?}\nacc:
        // {:?}\n\n", Route { steps: Vec::from(steps_to_allocate) } ,
        // subroutine_names_remaining, acc);

        if steps_to_allocate.is_empty() {
            return Some(acc);
        }

        for (k, v) in &acc.subroutines {
            if steps_to_allocate.starts_with(&v.steps) {
                return WithSubroutines::extract_subroutines(
                    &steps_to_allocate[v.steps.len() ..],
                    subroutine_names_remaining,
                    WithSubroutines {
                        main:        [acc.main, vec![*k]].concat(),
                        subroutines: acc.subroutines,
                    },
                );
            }
        }

        if subroutine_names_remaining.is_empty() {
            return None;
        }

        let max_n = min(
            steps_to_allocate.len(),
            20 - subroutine_names_remaining.len() + 1,
        );
        for n in 3 .. max_n {
            let candidate = &steps_to_allocate[0 .. n];
            let this_name = subroutine_names_remaining[0];
            let mut new_subroutines = acc.subroutines.clone();
            new_subroutines.insert(
                this_name,
                Route {
                    steps: Vec::from(candidate),
                },
            );
            let result = WithSubroutines::extract_subroutines(
                &steps_to_allocate[n ..],
                &subroutine_names_remaining[1 ..],
                WithSubroutines {
                    main:        [acc.main.clone(), vec![this_name]].concat(),
                    subroutines: new_subroutines,
                },
            );

            if result.is_some() {
                return result;
            }
        }

        None
    }

    fn prepare_for_input(&self, subroutine_names: &[char]) -> String {
        let main_program = self.main.iter().map(ToString::to_string).join(",");
        let subroutines = subroutine_names
            .iter()
            .map(|sr| format!("{}", self.subroutines[sr]))
            .join("\n");
        format!("{main_program}\n{subroutines}\nn\n")
    }
}

fn solve_2() {
    let board = board();
    let steps = compressed_path(&board);
    println!("{steps}");

    let subroutine_names = vec!['A', 'B', 'C'];
    let with_subroutines = WithSubroutines::extract_subroutines(
        &steps.steps,
        &subroutine_names,
        WithSubroutines::empty(),
    )
    .unwrap();

    let mut program = parse_machine_code(&program_as_str());
    assert_eq!(program[0], 1);
    program[0] = 2;

    let mut process = Process::new(&program);

    let input = WithSubroutines::prepare_for_input(&with_subroutines, &subroutine_names);

    process.provide_input_as_string(&input);
    process.run_to_halt();

    let output = process.read_output();
    let result = *output.last().unwrap();
    println!("Part2: {result}");
    assert_eq!(result, 597_517);
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

    fn test(data: &str, expected_compressed_path: &str, with_subroutines_expected: &str) {
        let board = Board::parse(data);

        let obtained_compressed_path = compressed_path(&board);

        assert_eq!(
            expected_compressed_path,
            format!("{obtained_compressed_path}")
        );

        let subroutine_names = vec!['A', 'B', 'C'];
        let with_subroutines = WithSubroutines::extract_subroutines(
            &obtained_compressed_path.steps,
            &subroutine_names,
            WithSubroutines::empty(),
        )
        .unwrap();

        let obtained = WithSubroutines::prepare_for_input(&with_subroutines, &subroutine_names);

        // The test assumes a particular implementation, because multiple valid
        // solutions can exist
        assert_eq!(obtained, with_subroutines_expected);
    }

    #[test]
    fn test_complex_1() {
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

        let expected_compressed_path = "R,8,R,8,R,4,R,4,R,8,L,6,L,2,R,4,R,4,R,8,R,8,R,8,L,6,L,2";

        let with_subroutines_expected = "A,B,C,B,A,C
R,8,R,8
R,4,R,4
R,8,L,6,L,2
n
";

        test(data, expected_compressed_path, with_subroutines_expected);
    }

    #[test]
    fn test_complex_2() {
        let data = "####^..................................
#......................................
#......................................
#......................................
###########...###########..............
..........#...#........................
..........#...#........................
..........#...#........................
......#####...#........................
......#.......#........................
......#.......#.###########...#########
......#.......#.#.........#...#.......#
......#####...#####.......#...#.......#
..........#.....#.#.......#...#.......#
....#####.#.....#.#.......#####.......#
....#...#.#.....#.#...................#
....#...#.#...#####...................#
....#...#.#...#.#.....................#
....#...###########...................#
....#.....#...#.#.#...................#
###########.#####.#...............#####
#...#.......#.#...#...............#....
#...#.......#.#...#...............#....
#...#.......#.#...#...............#....
#####.......###########...........#....
..............#...#...#...........#....
..............###########.........#....
..................#...#.#.........#....
..................#####.#.........#....
........................#.........#....
........................###########....";

        println!("{data}");

        let expected_compressed_path = "L,4,L,4,L,10,R,4,R,4,L,4,L,4,R,8,R,10,L,4,L,4,L,10,R,4,R,4,L,10,R,10,L,4,L,4,L,10,R,4,R,4,L,10,R,10,R,4,L,4,L,4,R,8,R,10,R,4,L,10,R,10,R,4,L,10,R,10,R,4,L,4,L,4,R,8,R,10";

        let with_subroutines_expected = "A,B,A,C,A,C,B,C,C,B
L,4,L,4,L,10,R,4
R,4,L,4,L,4,R,8,R,10
R,4,L,10,R,10
n
";

        test(data, expected_compressed_path, with_subroutines_expected);
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
