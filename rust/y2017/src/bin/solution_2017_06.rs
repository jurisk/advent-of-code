use advent_of_code_common::simulate::{until_matches_predicate, until_state_repeats};
use itertools::Itertools;

fn reallocate(banks: &Vec<usize>) -> Vec<usize> {
    let mut result = banks.to_owned();
    let max = banks.iter().max().unwrap();
    let (chosen_index, _) = banks.iter().find_position(|&x| x == max).unwrap();
    let amount = banks[chosen_index];
    result[chosen_index] = 0;
    for i in chosen_index + 1 ..= chosen_index + amount {
        result[i % banks.len()] += 1;
    }
    result
}

fn helper_1(data: &[usize]) -> (usize, Vec<usize>) {
    until_state_repeats(&data.to_vec(), reallocate)
}

fn solve_1(data: &[usize]) -> usize {
    let (repeats_at, _) = helper_1(data);
    repeats_at
}

fn solve_2(data: &[usize]) -> usize {
    let (repeats_at, repeating_block) = helper_1(data);
    let (first_seen, _) =
        until_matches_predicate(&data.to_vec(), reallocate, |x| x == &repeating_block);
    repeats_at - first_seen
}

const DATA: [usize; 16] = [0, 5, 10, 0, 11, 14, 13, 4, 11, 8, 8, 7, 1, 4, 12, 11];

fn main() {
    solve_1(&[0, 2, 7, 0]);

    let result_1 = solve_1(&DATA);
    println!("Part 1: {result_1}");

    let result_2 = solve_2(&DATA);
    println!("Part 2: {result_2}");
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST: [usize; 4] = [0, 2, 7, 0];

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(&TEST), 5);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(&DATA), 7864);
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve_2(&TEST), 4);
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(&DATA), 1695);
    }
}
