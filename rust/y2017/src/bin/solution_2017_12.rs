use std::collections::{BTreeMap, BTreeSet};

use advent_of_code_common::parsing::{
    parse_lines_to_vec, parse_separated_vec, parse_str, split_into_two_strings, Error,
};
use pathfinding::prelude::{bfs_reach, connected_components};

type ProgramId = u16;

struct Graph {
    map: BTreeMap<ProgramId, BTreeSet<ProgramId>>,
}

impl Graph {
    fn new() -> Self {
        Self {
            map: BTreeMap::new(),
        }
    }

    fn successors(&self, x: ProgramId) -> Vec<ProgramId> {
        self.map
            .get(&x)
            .unwrap_or(&BTreeSet::new())
            .iter()
            .copied()
            .collect::<Vec<_>>()
    }

    fn add_correspondence_int(&mut self, a: ProgramId, b: ProgramId) {
        let mut values = self.map.get(&a).unwrap_or(&BTreeSet::new()).clone();
        values.insert(b);
        self.map.insert(a, values);
    }

    fn add_correspondence(&mut self, a: ProgramId, b: ProgramId) {
        self.add_correspondence_int(a, b);
        self.add_correspondence_int(b, a);
    }
}

fn solve_1(graph: &Graph) -> usize {
    let results = bfs_reach(0, |x| graph.successors(*x));
    results.count()
}

fn solve_2(graph: &Graph) -> usize {
    let starts: Vec<ProgramId> = graph.map.keys().copied().collect::<Vec<_>>();
    connected_components(&starts, |x| graph.successors(*x)).len()
}

fn parse(input: &str) -> Result<Graph, Error> {
    let mut result = Graph::new();
    let lines: Vec<String> = parse_lines_to_vec(input)?;
    for line in lines {
        let (a, b) = split_into_two_strings(&line, " <-> ")?;
        let x: ProgramId = parse_str(&a)?;
        let ys: Vec<ProgramId> = parse_separated_vec(&b, ", ")?;
        for y in ys {
            result.add_correspondence(x, y);
        }
    }
    Ok(result)
}

fn part_1(input: &str) -> Result<usize, Error> {
    parse(input).map(|graph| solve_1(&graph))
}

fn part_2(input: &str) -> Result<usize, Error> {
    parse(input).map(|graph| solve_2(&graph))
}

const DATA: &str = include_str!("../../resources/12.txt");

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

    const TEST: &str = include_str!("../../resources/12-test.txt");

    #[test]
    fn test_solve_1_test() {
        assert_eq!(part_1(TEST), Ok(6));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(288));
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(part_2(TEST), Ok(2));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(211));
    }
}
