use std::collections::{BTreeSet, HashMap, HashSet};
use std::fmt::{Debug, Formatter};
use std::str::FromStr;

use advent_of_code_common::parsing::{
    Error, parse_lines_to_vec, parse_lines_to_vec_passing_parser, parse_str, split_into_two_strings,
};
use itertools::Itertools;
use memoize::memoize;
use pathfinding::prelude::{strongly_connected_components, strongly_connected_components_from};

const DATA: &str = include_str!("../../resources/23.txt");

#[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
struct Computer {
    a: char,
    b: char,
}

impl Debug for Computer {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.a, self.b)
    }
}

impl FromStr for Computer {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        assert_eq!(s.len(), 2);
        let a = s.chars().next().unwrap();
        let b = s.chars().nth(1).unwrap();
        Ok(Computer { a, b })
    }
}

type N = u32;
type R = usize;
type Input = Vec<(Computer, Computer)>;

fn parse(input: &str) -> Result<Input, Error> {
    parse_lines_to_vec_passing_parser(input, |line| {
        let (a, b) = split_into_two_strings(line, "-")?;
        let a = parse_str(&a)?;
        let b = parse_str(&b)?;
        Ok((a, b))
    })
}

fn solve_1(data: &Input) -> R {
    data.len()
}

fn f(
    current: &mut HashSet<Computer>,
    remaining: &mut HashSet<Computer>,
    connections: &HashMap<Computer, BTreeSet<Computer>>,
) -> HashSet<Computer> {
    println!("{:?}", current);
    let mut best = current.clone();
    let todos: Vec<_> = remaining.iter().cloned().collect();
    for node in todos {
        if current
            .iter()
            .all(|c| connections.get(c).unwrap().contains(&node))
        {
            current.insert(node);
            remaining.remove(&node);
            let b = f(current, remaining, connections);
            if b.len() > best.len() {
                best = b;
            }
            current.remove(&node);
            remaining.insert(node);
        }
    }
    best
}

fn solve_2(data: &Input) -> String {
    let mut nodes: HashSet<Computer> = HashSet::new();
    let mut connections: HashMap<Computer, BTreeSet<Computer>> = HashMap::new();

    for (a, b) in data {
        nodes.insert(*a);
        nodes.insert(b.clone());

        connections
            .entry(a.clone())
            .or_insert(BTreeSet::new())
            .insert(b.clone());
        connections
            .entry(b.clone())
            .or_insert(BTreeSet::new())
            .insert(a.clone());
    }

    println!("{:?}", connections);

    let best = f(&mut HashSet::new(), &mut nodes, &connections);
    let mut best: Vec<_> = best.into_iter().collect();
    best.sort();
    best.iter().map(|c| format!("{c:?}")).join(",")
}

fn main() -> Result<(), Error> {
    let data = parse(DATA)?;

    let result_1 = solve_1(&data);
    println!("Part 1: {result_1}");

    let result_2 = solve_2(&data);
    println!("Part 2: {result_2}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = include_str!("../../resources/23-test-00.txt");

    fn test_data() -> Input {
        parse(TEST_DATA).unwrap()
    }

    fn real_data() -> Input {
        parse(DATA).unwrap()
    }

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(&test_data()), 0);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(&real_data()), 0);
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve_2(&test_data()), "co,de,ka,ta");
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(&real_data()), "asdf");
    }
}
