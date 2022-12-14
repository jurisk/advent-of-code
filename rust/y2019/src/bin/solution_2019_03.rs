#![feature(map_try_insert)]

use crate::Command::{Down, Left, Right, Up};
use std::collections::{HashMap, HashSet};

type Number = i32;

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
struct Coords {
    x: Number,
    y: Number,
}

const ORIGIN: Coords = Coords { x: 0, y: 0 };

#[derive(Debug)]
enum Command {
    Left(Number),
    Right(Number),
    Up(Number),
    Down(Number),
}

fn parse_commands(line: &str) -> Vec<Command> {
    line.split(',')
        .map(|element| {
            let n = element[1..element.len()].parse::<Number>().unwrap();

            match element.chars().next().unwrap() {
                'U' => Up(n),
                'D' => Down(n),
                'L' => Left(n),
                'R' => Right(n),
                x => panic!("Unknown command {x} in {line}"),
            }
        })
        .collect()
}

fn visited_with_steps(commands: &[Command]) -> HashMap<Coords, Number> {
    let mut current = ORIGIN;
    let mut result: HashMap<Coords, Number> = HashMap::new();
    let mut steps: Number = 0;

    for command in commands {
        match command {
            Command::Left(n) => {
                ((current.x - n)..current.x).rev().for_each(|x| {
                    steps += 1;
                    let _ = result.try_insert(Coords { x, y: current.y }, steps);
                });

                current.x -= n;
            },

            Command::Right(n) => {
                ((current.x + 1)..=(current.x + n)).for_each(|x| {
                    steps += 1;
                    let _ = result.try_insert(Coords { x, y: current.y }, steps);
                });

                current.x += n;
            },

            Command::Up(n) => {
                ((current.y + 1)..=(current.y + n)).for_each(|y| {
                    steps += 1;
                    let _ = result.try_insert(Coords { x: current.x, y }, steps);
                });

                current.y += n;
            },

            Command::Down(n) => {
                ((current.y - n)..current.y).rev().for_each(|y| {
                    steps += 1;
                    let _ = result.try_insert(Coords { x: current.x, y }, steps);
                });

                current.y -= n;
            },
        }
    }

    result
}

fn manhattan_distance(a: Coords, b: Coords) -> Number {
    (a.x - b.x).abs() + (a.y - b.y).abs()
}

type CostF = fn(&Coords, &HashMap<Coords, Number>, &HashMap<Coords, Number>) -> Number;

fn solve(data: &str, f: CostF) -> Number {
    let lines: Vec<&str> = data.lines().filter(|x| !x.is_empty()).collect();

    assert_eq!(lines.len(), 2);

    let commands1 = parse_commands(lines[0]);
    let commands2 = parse_commands(lines[1]);

    let visited1 = visited_with_steps(&commands1);
    let visited2 = visited_with_steps(&commands2);

    let visited_coords1 = visited1.keys().copied().collect::<HashSet<Coords>>();
    let visited_coords2 = visited2.keys().copied().collect::<HashSet<Coords>>();

    visited_coords1
        .intersection(&visited_coords2)
        .map(|c| f(c, &visited1, &visited2))
        .min()
        .unwrap()
}

fn solve1(data: &str) -> Number {
    solve(data, |c, _a, _b| manhattan_distance(*c, ORIGIN))
}

fn solve2(data: &str) -> Number {
    solve(data, |c, a, b| a[c] + b[c])
}

fn main() {
    let test1 = "R8,U5,L5,D3
U7,R6,D4,L4";

    let test2 = "R75,D30,R83,U83,L12,D49,R71,U7,L72
U62,R66,U55,R34,D71,R55,D58,R83";

    let test3 = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
U98,R91,D20,R16,D67,R40,U7,R15,U6,R7";

    assert_eq!(solve1(test1), 6);
    assert_eq!(solve1(test2), 159);
    assert_eq!(solve1(test3), 135);

    let data = include_str!("../../resources/03.txt");
    let part1 = solve1(data);

    assert_eq!(part1, 258);
    println!("Part 1: {part1}");

    assert_eq!(solve2(test1), 30);
    assert_eq!(solve2(test2), 610);
    assert_eq!(solve2(test3), 410);

    let part2 = solve2(data);
    assert_eq!(part2, 12304);
    println!("Part 2: {part2}");
}
