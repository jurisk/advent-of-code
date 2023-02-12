use advent_of_code_common::parsing::{parse_lines_to_vec, Error};
use advent_of_code_common::utils::head_tail;
use memoize::memoize;

fn parse(input: &str) -> Result<Vec<u16>, Error> {
    parse_lines_to_vec(input)
}

#[allow(dead_code)]
fn select(data: &[u16], selection: u32) -> u16 {
    let mut current: u16 = 0;

    for (bit_index, item) in data.iter().enumerate() {
        if (1 << bit_index) & selection != 0 {
            current += item;
        }
    }

    current
}

#[allow(dead_code)]
fn valid_selections(data: &[u16], target: u16) -> Vec<u32> {
    let mut result = vec![];

    for i in 0 .. (2_u32).pow(u32::try_from(data.len()).unwrap()) {
        if select(data, i) == target {
            result.push(i);
        }
    }

    result
}

#[allow(dead_code)]
fn solve_1_old(data: &[u16], target: u16) -> usize {
    let selections = valid_selections(data, target);
    selections.len()
}

#[allow(dead_code)]
fn solve_2_old(data: &[u16], target: u16) -> usize {
    let selections = valid_selections(data, target);
    let minimum_number_of_containers = selections.iter().map(|x| x.count_ones()).min().unwrap();
    selections
        .iter()
        .filter(|x| x.count_ones() == minimum_number_of_containers)
        .count()
}

#[memoize]
#[allow(clippy::needless_pass_by_value)]
fn solve_1(data: Vec<u16>, target: u16) -> usize {
    if target == 0 {
        1
    } else {
        let (h, t) = head_tail(&data);
        match h {
            None => 0,
            Some(&h) => {
                let using_h = if target < h {
                    0
                } else {
                    solve_1(t.to_vec(), target - h)
                };
                let not_using_h = solve_1(t.to_vec(), target);
                using_h + not_using_h
            },
        }
    }
}

#[memoize]
#[allow(clippy::needless_pass_by_value)]
fn solve_2_int(data: Vec<u16>, target: u16, bottles_remaining: u16) -> usize {
    if bottles_remaining == 0 {
        usize::from(target == 0)
    } else {
        let (h, t) = head_tail(&data);
        match h {
            None => 0,
            Some(&h) => {
                let using_h = if target < h {
                    0
                } else {
                    solve_2_int(t.to_vec(), target - h, bottles_remaining - 1)
                };
                let not_using_h = solve_2_int(t.to_vec(), target, bottles_remaining);
                using_h + not_using_h
            },
        }
    }
}

fn solve_2(data: &[u16], target: u16) -> usize {
    for i in 1 ..= data.len() {
        let result = solve_2_int(data.to_vec(), target, i.try_into().unwrap());
        if result > 0 {
            return result;
        }
    }

    panic!("Not found")
}

fn part_1(input: &str, target: u16) -> Result<usize, Error> {
    parse(input).map(|input| solve_1(input, target))
}

fn part_2(input: &str, target: u16) -> Result<usize, Error> {
    parse(input).map(|input| solve_2(&input, target))
}

const DATA: &str = include_str!("../../resources/17.txt");

fn main() -> Result<(), Error> {
    let result_1 = part_1(DATA, 150)?;
    println!("Part 1: {result_1}");

    let result_2 = part_2(DATA, 150)?;
    println!("Part 2: {result_2}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = include_str!("../../resources/17-test.txt");

    #[test]
    fn test_solve_1_test() {
        assert_eq!(part_1(TEST_DATA, 25), Ok(4));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA, 150), Ok(1638));
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(part_2(TEST_DATA, 25), Ok(3));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA, 150), Ok(17));
    }
}
