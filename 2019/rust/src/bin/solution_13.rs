use advent_of_code::intcode::{parse_machine_code, Process};

use num_derive::FromPrimitive;
use num_traits::{signum, FromPrimitive};

#[repr(u8)]
#[derive(PartialEq, Eq, FromPrimitive, Debug)]
enum Tile {
    Empty = 0,            // No game object appears in this tile.
    Wall = 1,             // Walls are indestructible barriers.
    Block = 2,            // Blocks can be broken by the ball.
    HorizontalPaddle = 3, // The paddle is indestructible.
    Ball = 4,             // 4 is a ball tile. The ball moves diagonally and bounces off objects.
}

fn solve_1() {
    let mut program = Process::from_string(include_str!("../../resources/13.txt"));
    program.run_to_halt();
    let results = program.read_output();
    let chunked: Vec<_> = results.chunks_exact(3).collect();
    let tiles: Vec<Tile> = chunked
        .iter()
        .map(|arr| Tile::from_i128(arr[2]).unwrap())
        .collect();
    let result = tiles.iter().filter(|x| **x == Tile::Block).count();
    println!("Part 1: {result:?}");
}

fn solve_2() {
    let mut machine_code = parse_machine_code(include_str!("../../resources/13.txt"));
    machine_code[0] = 2;
    let mut program = Process::new(&machine_code);
    let mut score = 0;
    let mut ball_x = 0;
    let mut paddle_x = 0;

    loop {
        while program.unsatisfied_input() {
            let input = signum(ball_x - paddle_x);
            program.provide_input(input);
        }

        let halted = program.run_next_op();
        if halted {
            break;
        } else if program.output_len() >= 3 {
            let a = program.next_output_unsafe();
            let b = program.next_output_unsafe();
            let c = program.next_output_unsafe();

            if a == -1 && b == 0 {
                score = c;
            } else if Tile::from_i128(c).unwrap() == Tile::Ball {
                ball_x = a;
            } else if Tile::from_i128(c).unwrap() == Tile::HorizontalPaddle {
                paddle_x = a;
            }
        }
    }

    println!("Part 2: {score}");
    assert_eq!(score, 18371);
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
