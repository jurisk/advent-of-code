use std::collections::HashSet;

use advent_of_code_common::area2d::Area2D;
use advent_of_code_common::grid2d::Coords;
use advent_of_code_common::parsing::{Error, parse_lines_to_vec};
use itertools::Itertools;
use pathfinding::prelude::astar;

const DATA: &str = include_str!("../../resources/18.txt");
const REAL_END: Coords = Coords::new(70, 70);

type R = usize;
type Input = Vec<Coords>;

fn parse(input: &str) -> Result<Input, Error> {
    parse_lines_to_vec(input)
}

fn solution(busy: &HashSet<Coords>, end: Coords) -> Option<Vec<Coords>> {
    let area = Area2D::new(Coords::origin(), end);
    astar(
        &Coords::origin(),
        |c| {
            c.adjacent4()
                .into_iter()
                .filter(|n| area.contains(*n) && !busy.contains(n))
                .map(|n| (n, 1))
        },
        |c| c.manhattan_distance(end),
        |c| *c == end,
    )
    .map(|(path, _)| path)
}

fn solve_1(data: &Input, take: usize, end: Coords) -> R {
    let busy = data.iter().take(take).copied().collect::<HashSet<Coords>>();
    solution(&busy, end).expect("Expected a solution").len() - 1
}

fn solve_2(data: &Input, take: usize, end: Coords) -> String {
    let mut busy = data.iter().take(take).copied().collect::<HashSet<Coords>>();
    let mut path = solution(&busy, end).expect("Expected a solution");

    for c in data.iter().dropping(take) {
        busy.insert(*c);

        if path.contains(c) {
            if let Some(new_path) = solution(&busy, end) {
                path = new_path;
            } else {
                return format!("{},{}", c.x, c.y);
            }
        }
    }

    panic!("Expected a solution");
}

fn main() -> Result<(), Error> {
    let data = parse(DATA)?;

    let result_1 = solve_1(&data, 1024, REAL_END);
    println!("Part 1: {result_1}");

    let result_2 = solve_2(&data, 1024, REAL_END);
    println!("Part 2: {result_2}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use advent_of_code_common::grid2d::Coords;

    use super::*;

    const TEST_DATA: &str = include_str!("../../resources/18-test-00.txt");
    const TEST_END: Coords = Coords::new(6, 6);

    fn test_data() -> Input {
        parse(TEST_DATA).unwrap()
    }

    fn real_data() -> Input {
        parse(DATA).unwrap()
    }

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(&test_data(), 12, TEST_END), 22);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(&real_data(), 1024, REAL_END), 318);
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve_2(&test_data(), 12, TEST_END), "6,1".to_string());
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(&real_data(), 1024, REAL_END), "56,29".to_string());
    }
}
