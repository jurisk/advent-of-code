use std::collections::{BTreeSet, HashSet};
use std::fmt::{Debug, Formatter};
use std::hash::Hash;
use std::str::FromStr;

use advent_of_code_common::graph::{create_adjacency_hashmap, enumerate_maximum_cliques};
use advent_of_code_common::parsing::{
    Error, parse_lines_to_vec_passing_parser, parse_str, split_into_two_strings,
};
use itertools::Itertools;

const DATA: &str = include_str!("../../resources/23.txt");

#[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
struct Computer {
    a: char,
    b: char,
}

impl Computer {
    #[must_use]
    pub fn starts_with(self, c: char) -> bool {
        self.a == c
    }
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
    let neighbours = create_adjacency_hashmap(data);

    let mut results: HashSet<BTreeSet<Computer>> = HashSet::new();
    let mut f = |clique: &HashSet<Computer>| {
        for triplet in clique.iter().combinations(3) {
            assert_eq!(triplet.len(), 3);

            if triplet.iter().any(|c| c.starts_with('t')) {
                let btree_set: BTreeSet<_> = triplet.into_iter().copied().collect();
                results.insert(btree_set);
            }
        }
    };
    enumerate_maximum_cliques(&neighbours, &mut f);

    results.len()
}

fn solve_2(data: &Input) -> String {
    let neighbours = create_adjacency_hashmap(data);

    let mut best: HashSet<Computer> = HashSet::new();
    let mut f = |clique: &HashSet<Computer>| {
        if clique.len() > best.len() {
            best.clone_from(clique);
        }
    };

    enumerate_maximum_cliques(&neighbours, &mut f);

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
        assert_eq!(solve_1(&test_data()), 7);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(&real_data()), 1230);
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve_2(&test_data()), "co,de,ka,ta");
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(
            solve_2(&real_data()),
            "az,cj,kp,lm,lt,nj,rf,rx,sn,ty,ui,wp,zo"
        );
    }
}
