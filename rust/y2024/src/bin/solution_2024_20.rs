use std::collections::{HashMap, HashSet};

use advent_of_code_common::grid2d::{Coords, Grid2D, MatrixGrid2D};
use advent_of_code_common::parsing::Error;
use pathfinding::prelude::dijkstra_all;

const DATA: &str = include_str!("../../resources/20.txt");

type N = i32;
type R = usize;
type Input = (Coords, MatrixGrid2D<bool>, Coords);

#[derive(Eq, PartialEq, Hash, Debug)]
struct Cheat {
    from:     Coords,
    to:       Coords,
    distance: N,
}

fn parse(input: &str) -> Result<Input, Error> {
    let char_field = MatrixGrid2D::parse_char_field(input);
    let start = char_field
        .find_coords_by_value(&'S')
        .ok_or("Start not found")?;
    let end = char_field
        .find_coords_by_value(&'E')
        .ok_or("End not found")?;
    let field = char_field.map_by_values(|c| {
        match c {
            '#' => true,
            'S' | 'E' | '.' => false,
            _ => panic!("Unknown character {c} in field"),
        }
    });
    Ok((start, field, end))
}

fn solve(data: &Input, save_at_least: N, max_cheat: N) -> R {
    let (start, field, end) = data;

    let successors = |position: &Coords| -> Vec<(Coords, N)> {
        field
            .neighbours_for(*position, false)
            .into_iter()
            .filter(|n| field.get(*n).is_some_and(|b| !*b))
            .map(|n| (n, 1))
            .collect()
    };

    let from_start: HashMap<Coords, (Coords, N)> = dijkstra_all(start, successors);
    let from_end: HashMap<Coords, (Coords, N)> = dijkstra_all(end, successors);

    let (_, cost) = from_start.get(end).expect("No path from start to end");
    let goal_cost_threshold = cost - save_at_least;

    println!("Without cheats = {cost}, goal cost threshold = {goal_cost_threshold}");

    let valid_cheat = |cheat: &Cheat| -> bool {
        let start_cost = from_start
            .get(&cheat.from)
            .map(|(_, c)| *c)
            .unwrap_or_default();
        let end_cost = from_end.get(&cheat.to).map(|(_, c)| *c).unwrap_or_default();
        start_cost + end_cost + cheat.distance <= goal_cost_threshold
    };

    let empties: Vec<_> = field
        .coords()
        .filter(|c| field.get(*c).is_some_and(|b| !*b))
        .collect();

    let mut valid_cheats = HashSet::new();

    // Could be optimised by avoiding processing all O(N^2), as if we know one coordinate, we can generate all others within maxCheat distance more efficiently
    for from in &empties {
        for to in &empties {
            let from = *from;
            let to = *to;
            let distance = from.manhattan_distance(to);
            if (1 ..= max_cheat).contains(&distance) {
                let cheat = Cheat { from, to, distance };
                if valid_cheat(&cheat) {
                    valid_cheats.insert(cheat);
                }
            }
        }
    }

    valid_cheats.len()
}

fn solve_1(data: &Input, save_at_least: N) -> R {
    solve(data, save_at_least, 2)
}

fn solve_2(data: &Input, save_at_least: N) -> R {
    solve(data, save_at_least, 20)
}

fn main() -> Result<(), Error> {
    let data = parse(DATA)?;

    let result_1 = solve_1(&data, 100);
    println!("Part 1: {result_1}");

    let result_2 = solve_2(&data, 100);
    println!("Part 2: {result_2}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = include_str!("../../resources/20-test-00.txt");

    fn test_data() -> Input {
        parse(TEST_DATA).unwrap()
    }

    fn real_data() -> Input {
        parse(DATA).unwrap()
    }

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(&test_data(), 64), 1);
        assert_eq!(solve_1(&test_data(), 40), 2);
        assert_eq!(solve_1(&test_data(), 38), 3);
        assert_eq!(solve_1(&test_data(), 36), 4);
        assert_eq!(solve_1(&test_data(), 20), 5);
        assert_eq!(solve_1(&test_data(), 12), 5 + 3);
        assert_eq!(solve_1(&test_data(), 10), 5 + 3 + 2);
        assert_eq!(solve_1(&test_data(), 8), 5 + 3 + 2 + 4);
        assert_eq!(solve_1(&test_data(), 6), 5 + 3 + 2 + 4 + 2);
        assert_eq!(solve_1(&test_data(), 4), 5 + 3 + 2 + 4 + 2 + 14);
        assert_eq!(solve_1(&test_data(), 2), 5 + 3 + 2 + 4 + 2 + 14 + 14);
    }

    #[test]
    #[ignore]
    fn test_solve_1_real() {
        assert_eq!(solve_1(&real_data(), 100), 1293);
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve_2(&test_data(), 76), 3);
    }

    #[test]
    #[ignore]
    fn test_solve_2_real() {
        assert_eq!(solve_2(&real_data(), 100), 977_747);
    }
}
