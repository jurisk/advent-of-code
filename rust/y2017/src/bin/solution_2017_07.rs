use std::collections::{HashMap, HashSet};
use std::str::FromStr;

use advent_of_code_common::parsing::{
    Error, parse_lines_to_vec, parse_str, split_into_two_strings,
};
use advent_of_code_common::utils::{least_frequent, most_frequent, single_result};
use itertools::Itertools;

type Name = String;

struct Info {
    name:    Name,
    weight:  i32,
    holding: Vec<Name>,
}

const SEPARATOR: &str = " -> ";
impl FromStr for Info {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (base, holding) = if s.contains(SEPARATOR) {
            let (a, b) = split_into_two_strings(s, SEPARATOR)?;
            (
                a,
                b.split(", ")
                    .map(ToString::to_string)
                    .collect::<Vec<String>>(),
            )
        } else {
            (s.to_string(), vec![])
        };

        let (name, weight_in_brackets) = split_into_two_strings(&base, " ")?;
        let weight_string = weight_in_brackets
            .strip_prefix('(')
            .and_then(|x| x.strip_suffix(')'))
            .ok_or(format!(
                "Weight {weight_in_brackets} did not match expected format"
            ))?;
        let weight: i32 = parse_str(weight_string)?;

        Ok(Info {
            name,
            weight,
            holding,
        })
    }
}

// We could have built a proper tree, but this was easier and works.
type Data = HashMap<Name, Info>;

fn parse(input: &str) -> Result<Data, Error> {
    let vec: Vec<Info> = parse_lines_to_vec(input)?;
    let map: HashMap<_, _> = vec.into_iter().map(|x| (x.name.clone(), x)).collect();
    Ok(map)
}

fn solve_1(data: &Data) -> Name {
    let children: HashSet<_> = data
        .iter()
        .flat_map(|(_name, info)| info.holding.clone())
        .collect();
    let parents: Vec<_> = data
        .values()
        .filter(|x| !children.contains(&x.name))
        .collect();
    single_result(&parents).unwrap().name.clone()
}

// We could have memoized this
fn program_weight(data: &Data, name: &str) -> i32 {
    let info = data.get(name).unwrap();
    let children_weight: i32 = info.holding.iter().map(|x| program_weight(data, x)).sum();
    info.weight + children_weight
}

fn find_correct_weight(data: &Data, name: &str, diff: i32) -> i32 {
    let info = data.get(name).unwrap();
    let children: HashMap<Name, i32> = info
        .holding
        .iter()
        .map(|x| (x.clone(), program_weight(data, x)))
        .collect();
    let children_weights: Vec<i32> = children.values().copied().collect();
    if children_weights.iter().all_equal() {
        // children are balanced so it is us who are at fault
        info.weight + diff
    } else {
        let mf = *most_frequent(&children_weights).unwrap();
        let lf = *least_frequent(&children_weights).unwrap();
        assert!(children_weights.iter().all(|&x| x == mf || x == lf));
        let (name_of_the_wrong_child, _) =
            children.iter().find(|(_, &weight)| weight == lf).unwrap();
        find_correct_weight(data, name_of_the_wrong_child, mf - lf)
    }
}

fn solve_2(data: &Data) -> i32 {
    let root = solve_1(data);
    find_correct_weight(data, &root, 0)
}

fn part_1(input: &str) -> Result<Name, Error> {
    parse(input).map(|input| solve_1(&input))
}

fn part_2(input: &str) -> Result<i32, Error> {
    parse(input).map(|input| solve_2(&input))
}

const DATA: &str = include_str!("../../resources/07.txt");

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

    const TEST: &str = include_str!("../../resources/07-test.txt");

    #[test]
    fn test_solve_1_test() {
        assert_eq!(part_1(TEST), Ok("tknk".to_string()));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok("dgoocsw".to_string()));
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(part_2(TEST), Ok(60));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(1275));
    }
}
