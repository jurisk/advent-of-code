use std::cmp::max;
use std::collections::HashSet;
use std::string::ToString;

use advent_of_code_common::parsing::{
    normalize_newlines, parse_lines_to_vec_passing_parser, parse_str, split_into_two_strings, Error,
};
use pathfinding::prelude::astar;
use strum::{Display, EnumIter, EnumString, IntoEnumIterator};

#[derive(Eq, PartialEq, Hash, Copy, Clone, EnumIter, EnumString, Display, Debug)]
#[allow(non_camel_case_types)]
enum Element {
    Al,
    B,
    Ca,
    F,
    H,
    Mg,
    N,
    O,
    P,
    Si,
    Th,
    Ti,
    e,
    Rn,
    Ar,
    Y,
    C,
}

type Molecule = Vec<Element>;
type Replacements = Vec<(Element, Molecule)>;
type Data = (Replacements, Molecule);

fn parse_molecule(input: &str) -> Result<Molecule, Error> {
    if input.is_empty() {
        Ok(Molecule::new())
    } else {
        for element in Element::iter() {
            if let Some(remaining) = input.strip_prefix(&element.to_string()) {
                let tail = parse_molecule(remaining)?;
                return Ok(vec![vec![element], tail].concat());
            }
        }

        Err(format!("Nothing matched for {input}"))
    }
}

fn parse(input: &str) -> Result<Data, Error> {
    let (replacements_str, medicine_str) =
        split_into_two_strings(&normalize_newlines(input), "\n\n")?;
    let replacements = parse_lines_to_vec_passing_parser(&replacements_str, |x| {
        let (a, b) = split_into_two_strings(x, " => ")?;
        let element: Element = parse_str(&a)?;
        let molecule = parse_molecule(&b)?;
        Ok((element, molecule))
    })?;
    let medicine = parse_molecule(&medicine_str)?;
    Ok((replacements, medicine))
}

fn successors(replacements: &Replacements, state: &Molecule) -> Vec<Molecule> {
    let set: HashSet<Molecule> = replacements
        .iter()
        .flat_map(|(k, v)| {
            let mut options: HashSet<Molecule> = HashSet::new();
            for (idx, element) in state.iter().enumerate() {
                if k == element {
                    let option = vec![&state[0 .. idx], v.as_slice(), &state[idx + 1 ..]].concat();
                    options.insert(option);
                }
            }
            options
        })
        .collect();

    set.into_iter().collect()
}

fn solve_1(data: &Data) -> usize {
    let (replacements, calibration) = data;
    let result = successors(replacements, calibration);
    result.len()
}

fn solve_2(data: &Data) -> usize {
    let (replacements, target) = data;

    let non_terminal: HashSet<Element> = replacements.iter().map(|(a, _)| a).copied().collect();
    let terminal: HashSet<Element> = Element::iter()
        .filter(|x| !non_terminal.contains(x))
        .collect();

    let count_terminal =
        |w: &Molecule| -> usize { w.iter().filter(|x| terminal.contains(x)).count() };

    let expected_terminal = count_terminal(target);

    println!("Target length: {}", target.len());
    println!("Terminal elements: {}: {terminal:?}", terminal.len());

    let mut best_seen: usize = 0;

    let (_path, cost) = astar(
        &vec![Element::e],
        |state| {
            if state.len() > target.len() || count_terminal(state) > expected_terminal {
                vec![]
            } else {
                successors(replacements, state)
                    .into_iter()
                    .map(|x| (x, 1))
                    .collect()
            }
        },
        |state| {
            let correct = state
                .iter()
                .zip(target.iter())
                .take_while(|(a, b)| a == b)
                .count();

            if correct > best_seen {
                println!("{correct} {state:?}");
                best_seen = correct;
            }

            max(target.len(), state.len()) - correct
        },
        |state| state == target,
    )
    .unwrap();

    cost
}

fn part_1(input: &str) -> Result<usize, Error> {
    let data = parse(input)?;
    Ok(solve_1(&data))
}

fn part_2(input: &str) -> Result<usize, Error> {
    let data = parse(input)?;
    Ok(solve_2(&data))
}

const DATA: &str = include_str!("../../resources/19.txt");

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

    const TEST_HOHOHO: &str = "e => H
e => O
H => HO
H => OH
O => HH

HOHOHO";

    #[test]
    fn test_solve_1_hohoho() {
        assert_eq!(part_1(TEST_HOHOHO), Ok(7));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(535));
    }

    #[test]
    fn test_solve_2_hohoho() {
        assert_eq!(part_2(TEST_HOHOHO), Ok(6));
    }

    #[test]
    #[ignore] // Works quickly in enough in production builds
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(212));
    }
}
