use std::cmp::max;
use advent_of_code::intcode::{Entry, MachineCode, parse_machine_code, Process};

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
struct Coords {
    x: i32,
    y: i32,
}

fn machine_code() -> MachineCode {
    parse_machine_code(include_str!("../../resources/19.txt"))
}

fn beam_at_position(coords: Coords) -> bool {
    let mut process = Process::new(&machine_code());
    process.provide_input(coords.x as Entry);
    process.provide_input(coords.y as Entry);
    let halt = process.run_to_halt_or_output();
    assert!(!halt);
    process.next_output_unsafe() == 1
}

fn solve_1() {
    let coords: Vec<Coords> = (0..50).flat_map(|x| {
        (0..50).map(move |y| Coords { x, y })
    }).collect();

    let result: u32 = coords.iter().map(|c|
        u32::from(beam_at_position(*c))
    ).sum();

    println!("Part 1: {}", result);
    assert_eq!(result, 150);
}

fn find_leftmost_x(y: i32, bound_x: i32) -> i32 {
    if beam_at_position(Coords { x: 0, y }) {
        return 0;
    }

    for x in (bound_x - 1).. {
        if !beam_at_position(Coords { x, y }) && beam_at_position( Coords { x: x + 1, y }) {
            return x + 1
        }
    }

    panic!("Unexpected")
}

fn solve_2() {
    let spaceship_size = 100;

    let mut bound_x = 0;
    for y in spaceship_size.. {
        let x = find_leftmost_x(y, bound_x);
        bound_x = max(x, bound_x);
        let bottom_left = Coords { x, y };
        let top_left = Coords {
            x: bottom_left.x,
            y: bottom_left.y - spaceship_size + 1,
        };
        let top_right = Coords {
            x: top_left.x + spaceship_size - 1,
            y: top_left.y,
        };
        let bottom_right = Coords {
            x: top_right.x,
            y: bottom_left.y,
        };

        if beam_at_position(bottom_left) && beam_at_position(top_left) && beam_at_position(top_right) && beam_at_position(bottom_right) {
            let result = 10000 * top_left.x + top_left.y;
            println!("Part 2: {}", result);
            assert_eq!(result, 12_201_460);
            return;
        }
    }

    panic!("Nothing was found");
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
