use std::collections::BTreeSet;

use advent_of_code_common::pairing::Pairing;
use advent_of_code_common::parsing::{
    Error, parse_lines_to_btreeset, parse_str, split_into_two_strings,
};
use pathfinding::prelude::bfs_reach;

type Port = u32;
type Component = Pairing<Port>;

#[derive(Clone, Eq, PartialEq, Hash)]
struct State {
    last:      Port,
    remaining: BTreeSet<Component>,
}

impl State {
    fn new(components: &BTreeSet<Component>) -> Self {
        Self {
            last:      0,
            remaining: components.clone(),
        }
    }

    fn strength(&self, original: &BTreeSet<Component>) -> u32 {
        strength(original) - strength(&self.remaining)
    }

    fn successors(&self) -> Vec<State> {
        self.remaining
            .iter()
            .filter(|x| x.contains(&self.last))
            .map(|x| {
                let mut new_remaining = self.remaining.clone();
                new_remaining.remove(x);
                let new_last = x.find_other_side(&self.last).unwrap();
                Self {
                    last:      *new_last,
                    remaining: new_remaining,
                }
            })
            .collect()
    }
}

fn strength(components: &BTreeSet<Component>) -> u32 {
    components.iter().map(Pairing::sum).sum()
}

fn solve<F, G: Ord>(components: &BTreeSet<Component>, compare_by: F) -> Result<u32, Error>
where
    F: Fn(&State) -> G,
{
    let start = State::new(components);

    bfs_reach(start, State::successors)
        .min_by_key(|x| compare_by(x))
        .ok_or("Failed to find".to_string())
        .map(|x| x.strength(components))
}

fn parse(input: &str) -> Result<BTreeSet<Component>, Error> {
    let lines: BTreeSet<String> = parse_lines_to_btreeset(input)?;
    lines
        .into_iter()
        .map(|s| {
            let (a, b) = split_into_two_strings(&s, "/")?;
            let a: u32 = parse_str(&a)?;
            let b: u32 = parse_str(&b)?;
            Ok(Pairing::new(a, b))
        })
        .collect()
}
fn part_1(input: &str) -> Result<u32, Error> {
    let components = parse(input)?;
    solve(&components, |state| strength(&state.remaining))
}

fn part_2(input: &str) -> Result<u32, Error> {
    let components = parse(input)?;
    solve(&components, |state| {
        (state.remaining.len(), strength(&state.remaining))
    })
}

const DATA: &str = include_str!("../../resources/24.txt");

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

    const TEST: &str = include_str!("../../resources/24-test.txt");

    #[test]
    fn test_solve_1_test() {
        assert_eq!(part_1(TEST), Ok(31));
    }

    #[test]
    #[ignore]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(1906));
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(part_2(TEST), Ok(19));
    }

    #[test]
    #[ignore]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(1824));
    }
}
