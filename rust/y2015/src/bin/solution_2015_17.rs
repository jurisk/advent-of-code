use advent_of_code_common::parsing::{parse_lines_to_vec, Error};

fn parse(input: &str) -> Result<Vec<u16>, Error> {
    parse_lines_to_vec(input)
}

fn select(data: &[u16], selection: u32) -> u16 {
    let mut current: u16 = 0;

    for (bit_index, item) in data.iter().enumerate() {
        if (1 << bit_index) & selection != 0 {
            current += item;
        }
    }

    current
}

fn valid_selections(data: &[u16], target: u16) -> Vec<u32> {
    let mut result = vec![];

    for i in 0..(2_u32).pow(u32::try_from(data.len()).unwrap()) {
        if select(data, i) == target {
            result.push(i);
        }
    }

    result
}

fn solve_1(data: &[u16], target: u16) -> usize {
    let selections = valid_selections(data, target);
    selections.len()
}

fn solve_2(data: &[u16], target: u16) -> usize {
    let selections = valid_selections(data, target);
    let minimum_number_of_containers = selections.iter().map(|x| x.count_ones()).min().unwrap();
    selections
        .iter()
        .filter(|x| x.count_ones() == minimum_number_of_containers)
        .count()
}

fn part_1(input: &str, target: u16) -> Result<usize, Error> {
    parse(input).map(|input| solve_1(&input, target))
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
