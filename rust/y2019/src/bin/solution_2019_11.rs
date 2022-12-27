use advent_of_code_2019::intcode::Process;
use advent_of_code_common::coords2d::Coords2D;
use advent_of_code_common::direction::Direction;
use advent_of_code_common::rotation::Rotation;
use itertools::Itertools;
use num_derive::FromPrimitive;
use num_derive::ToPrimitive;
use num_traits::FromPrimitive;
use num_traits::ToPrimitive;
use std::collections::HashMap;

#[repr(u8)]
#[derive(ToPrimitive, FromPrimitive, Debug)]
enum Square {
    Black = 0,
    White = 1,
}

type Index = i32;
type Coords = Coords2D<Index>;

type Board = HashMap<Coords, Square>;

fn solve(starting_panel: Square) -> Board {
    let mut program = Process::from_string(include_str!("../../resources/11.txt"));
    let mut board: Board = HashMap::new();
    let mut position = Coords::new(0, 0);
    board.insert(position, starting_panel);
    let mut facing = Direction::North;
    loop {
        let current_colour = board.get(&position).unwrap_or(&Square::Black);
        program.provide_input(current_colour.to_i128().unwrap());
        let halted = program.run_to_halt_or_output_size(2);
        if halted {
            assert_eq!(program.output_len(), 0);
            break;
        } else {
            let colour_to_paint = Square::from_i128(program.next_output_unsafe()).unwrap();
            let rotation_to_turn = Rotation::from_i128(program.next_output_unsafe()).unwrap();
            board.insert(position, colour_to_paint);
            let new_facing = facing.rotate(rotation_to_turn);
            facing = new_facing;
            position = position.move_in_direction(facing);
        }
    }
    board
}

fn board_as_string(board: &Board) -> String {
    let coords: Vec<&Coords> = board.keys().collect();
    let x_s: Vec<Index> = coords.iter().map(|&&c| c.x).collect();
    let y_s: Vec<Index> = coords.iter().map(|&&c| c.y).collect();

    let min_x = *x_s.iter().min().unwrap();
    let min_y = *y_s.iter().min().unwrap();
    let max_x = *x_s.iter().max().unwrap();
    let max_y = *y_s.iter().max().unwrap();
    (min_y..=max_y)
        .map(|y| {
            (min_x..=max_x)
                .map(|x| {
                    let square = board.get(&Coords::new(x, y)).unwrap_or(&Square::Black);
                    match square {
                        Square::Black => '░',
                        Square::White => '█',
                    }
                })
                .join("")
                + "\n"
        })
        .join("")
}

fn solve_1() {
    let board = solve(Square::Black);
    let result = board.len();
    println!("Part 1: Panels painted: {result}");
    assert_eq!(result, 2141);
    println!("{}", board_as_string(&board));
}

fn solve_2() {
    let board = solve(Square::White);
    let obtained = board_as_string(&board);
    println!("Part 2:\n{obtained}");

    let expected = "\
░███░░███░░░░██░░██░░████░████░█░░█░████░░░
░█░░█░█░░█░░░░█░█░░█░█░░░░░░░█░█░█░░█░░░░░░
░█░░█░█░░█░░░░█░█░░░░███░░░░█░░██░░░███░░░░
░███░░███░░░░░█░█░░░░█░░░░░█░░░█░█░░█░░░░░░
░█░█░░█░░░░█░░█░█░░█░█░░░░█░░░░█░█░░█░░░░░░
░█░░█░█░░░░░██░░░██░░█░░░░████░█░░█░█░░░░░░
";

    assert_eq!(obtained, expected);
}

fn main() {
    solve_1();
    solve_2();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve_1() {
        solve_1();
    }

    #[test]
    fn test_solve_2() {
        solve_2();
    }
}
