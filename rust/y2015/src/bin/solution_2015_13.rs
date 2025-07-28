use std::collections::HashSet;
use std::str::FromStr;

use advent_of_code_common::parsing::{Error, parse_lines_to_vec, parse_str};
use itertools::Itertools;
use recap::Recap;
use serde::Deserialize;

#[allow(non_local_definitions)]
#[derive(Deserialize, Recap)]
#[recap(
    regex = r#"^(?P<who>\w+) would (?P<do_what>\w+) (?P<how_many>\d+) happiness units by sitting next to (?P<next_to_who>\w+).$"#
)]
struct RawProximity {
    who:         String,
    do_what:     String,
    how_many:    i32,
    next_to_who: String,
}

#[derive(Debug, Clone)]
struct Proximity {
    who:             String,
    happiness_delta: i32,
    next_to_who:     String,
}

impl FromStr for Proximity {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let raw: RawProximity = parse_str(s)?;
        let coef = match raw.do_what.as_str() {
            "gain" => Ok(1),
            "lose" => Ok(-1),
            s => Err(format!("Unexpected {s}")),
        };

        coef.map(|coef| {
            Proximity {
                who:             raw.who,
                happiness_delta: coef * raw.how_many,
                next_to_who:     raw.next_to_who,
            }
        })
    }
}

fn parse(input: &str) -> Result<Vec<Proximity>, Error> {
    parse_lines_to_vec(input)
}

fn all_names(data: &[Proximity]) -> HashSet<String> {
    data.iter().map(|p| p.who.clone()).collect()
}

fn solve_1(data: &[Proximity]) -> i32 {
    let calculate_happiness = |names: &[&String]| -> i32 {
        let find_happiness = |who: &str, next_to_who: &str| -> i32 {
            let found = data
                .iter()
                .find(|x| x.who == who && x.next_to_who == next_to_who)
                .unwrap();
            found.happiness_delta
        };

        names
            .iter()
            .circular_tuple_windows()
            .map(|(a, b)| find_happiness(a, b) + find_happiness(b, a))
            .sum()
    };

    let names: HashSet<_> = all_names(data);
    println!("{names:?}");
    names
        .iter()
        .permutations(names.len())
        .map(|x| calculate_happiness(&x))
        .max()
        .unwrap()
}

const MYSELF: &str = "Myself";
fn solve_2(data: &[Proximity]) -> i32 {
    let names = all_names(data);
    let mine: Vec<Proximity> = names
        .iter()
        .flat_map(|n| {
            vec![
                Proximity {
                    who:             MYSELF.to_string(),
                    happiness_delta: 0,
                    next_to_who:     n.clone(),
                },
                Proximity {
                    who:             n.clone(),
                    happiness_delta: 0,
                    next_to_who:     MYSELF.to_string(),
                },
            ]
        })
        .collect();

    let merged = [data, &mine].concat();
    solve_1(&merged)
}

fn part_1(input: &str) -> Result<i32, Error> {
    let data = parse(input)?;
    Ok(solve_1(&data))
}

fn part_2(input: &str) -> Result<i32, Error> {
    let data = parse(input)?;
    Ok(solve_2(&data))
}

const DATA: &str = include_str!("../../resources/13.txt");

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

    const TEST: &str = include_str!("../../resources/13-test.txt");

    #[test]
    fn test_solve_1_test() {
        assert_eq!(part_1(TEST), Ok(330));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(664));
    }

    #[test]
    #[ignore = "Slow"]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(640));
    }
}
