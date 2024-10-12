use advent_of_code_common::parsing::{Error, parse_lines_to_vec};
use advent_of_code_common::utils::{least_frequent, most_frequent, transpose};

type Data = Vec<String>;

fn parse(input: &str) -> Result<Data, Error> {
    parse_lines_to_vec(input)
}

fn solve<F>(data: &Data, f: F) -> String
where
    F: Fn(&[char]) -> Option<&char>,
{
    let chars: Vec<Vec<char>> = data.iter().map(|s| s.chars().collect()).collect();
    let transposed = transpose(&chars);
    transposed
        .iter()
        .map(|chars| {
            f(chars).unwrap_or_else(|| panic!("Most frequent element not found in {chars:?}!"))
        })
        .collect()
}

fn solve_1(data: &Data) -> String {
    solve(data, most_frequent)
}

fn solve_2(data: &Data) -> String {
    solve(data, least_frequent)
}

fn part_1(input: &str) -> Result<String, Error> {
    parse(input).map(|input| solve_1(&input))
}

fn part_2(input: &str) -> Result<String, Error> {
    parse(input).map(|input| solve_2(&input))
}

const DATA: &str = include_str!("../../resources/06.txt");

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

    const TEST: &str = include_str!("../../resources/06-test.txt");

    #[test]
    fn test_solve_1_test() {
        assert_eq!(part_1(TEST), Ok("easter".to_string()));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok("mshjnduc".to_string()));
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(part_2(TEST), Ok("advent".to_string()));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok("apfeeebz".to_string()));
    }
}
