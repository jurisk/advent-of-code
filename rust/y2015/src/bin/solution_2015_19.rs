use advent_of_code_common::parsing::{
    normalize_newlines, parse_lines_to_vec_passing_parser, split_into_two_strings, Error,
};
use pathfinding::prelude::{bfs, dfs};
use std::collections::HashSet;

type Data = (Vec<(String, String)>, String);

fn parse(input: &str) -> Result<Data, Error> {
    let (a, medicine) = split_into_two_strings(&normalize_newlines(input), "\n\n")?;
    let replacements =
        parse_lines_to_vec_passing_parser(&a, |x| split_into_two_strings(x, " => "))?;
    Ok((replacements, medicine))
}

fn successors(replacements: &[(String, String)], state: &str) -> Vec<String> {
    let set: HashSet<String> = replacements
        .iter()
        .flat_map(|(k, v)| {
            state
                .match_indices(k)
                .map(|(idx, s)| {
                    let mut result: String = String::new();
                    result.push_str(&state[0..idx]);
                    result.push_str(v);
                    result.push_str(&state[idx + s.len()..]);
                    result
                })
                .collect::<HashSet<_>>()
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

    // Constraint to ensure our short-circuit is reasonable
    assert!(replacements.iter().all(|(a, b)| a.len() <= b.len()));

    // let result_dfs = dfs(
    //     "e".to_string(),
    //     |state| {
    //         if state.len() > target.len() {
    //             vec![]
    //         } else {
    //             successors(replacements, &state)
    //         }
    //     },
    //     |state| state == target,
    // )
    // .unwrap_or_else(|| panic!("Not found!"));

    let result = bfs(
        &"e".to_string(),
        |state| {
            if state.len() > target.len() {
                vec![]
            } else {
                successors(replacements, state)
            }
        },
        |state| state == target,
    )
    .unwrap_or_else(|| panic!("Not found!"));

    result.len() - 1
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
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(123456));
    }
}
