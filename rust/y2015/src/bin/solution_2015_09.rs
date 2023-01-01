use advent_of_code_common::parsing::{parse_lines_to_vec, Error};
use pathfinding::prelude::dfs_reach;
use recap::Recap;
use serde::Deserialize;
use std::collections::HashSet;

const DATA: &str = include_str!("../../resources/09.txt");

type Location = String;

#[derive(Debug, Deserialize, Recap, Clone)]
#[recap(regex = r#"^(?P<from>[A-Za-z]+) to (?P<to>[A-Za-z]+) = (?P<distance>\d+)$"#)]
struct Route {
    from: Location,
    to: Location,
    distance: i32,
}

impl Route {
    fn reverse_direction(&self) -> Self {
        Route {
            from: self.to.clone(),
            to: self.from.clone(),
            distance: self.distance,
        }
    }

    fn reverse_distance(&self) -> Self {
        Route {
            from: self.from.clone(),
            to: self.to.clone(),
            distance: -self.distance,
        }
    }
}

fn parse(input: &str) -> Result<Vec<Route>, Error> {
    parse_lines_to_vec(input)
}

fn solve(data: &[Route], prune: bool) -> i32 {
    let locations: HashSet<_> = data
        .iter()
        .flat_map(|d| vec![d.from.clone(), d.to.clone()])
        .collect::<HashSet<_>>();
    let edges: Vec<Route> = data
        .iter()
        .flat_map(|r| vec![r.clone(), r.reverse_direction()])
        .collect();
    let count = locations.len();
    let mut best = i32::MAX;

    let start: (Vec<Location>, i32) = (Vec::new(), 0);

    let successors = |state: &(Vec<Location>, i32)| -> Vec<(Vec<Location>, i32)> {
        let (path, cost) = state;
        if path.len() == count {
            if *cost < best {
                best = *cost;
            }
            vec![]
        } else if prune && *cost >= best {
            vec![]
        } else {
            match path.last() {
                None => locations
                    .iter()
                    .map(|location| (vec![location.to_string()], 0))
                    .collect::<Vec<_>>(),
                Some(last) => {
                    let non_visited_neighbours = edges
                        .iter()
                        .filter(|r| &r.from == last && !path.contains(&r.to));
                    non_visited_neighbours
                        .map(|r| {
                            (
                                vec![path.clone(), vec![r.to.clone()]].concat(),
                                cost + r.distance,
                            )
                        })
                        .collect()
                },
            }
        }
    };

    let _ignore = dfs_reach(start, successors).collect::<Vec<_>>();

    best
}

fn solve_1(data: &[Route]) -> i32 {
    solve(data, true)
}

fn solve_2(data: &[Route]) -> i32 {
    let adjusted: Vec<Route> = data.iter().map(Route::reverse_distance).collect();
    -solve(&adjusted, false)
}

fn part_1(input: &str) -> Result<i32, Error> {
    let data = parse(input)?;
    Ok(solve_1(&data))
}

fn part_2(input: &str) -> Result<i32, Error> {
    let data = parse(input)?;
    Ok(solve_2(&data))
}

fn main() -> Result<(), Error> {
    let result_1 = part_1(DATA)?;
    println!("Part 1: {result_1}");

    let result_2 = part_2(DATA)?;
    println!("Part 2: {result_2}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST: &str = include_str!("../../resources/09-test.txt");

    #[test]
    fn test_solve_1_test() {
        assert_eq!(part_1(TEST), Ok(605));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(251));
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(part_2(TEST), Ok(982));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(898));
    }
}
