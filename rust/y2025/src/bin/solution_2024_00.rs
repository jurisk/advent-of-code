use advent_of_code_common::parsing::{Error, parse_lines_to_vec};

const DATA: &str = include_str!("../../resources/00.txt");

type N = u32;
type R = usize;
type Input = Vec<N>;

fn parse(input: &str) -> Result<Input, Error> {
    parse_lines_to_vec(input)
}

fn solve_1(data: &Input) -> R {
    data.len()
}

fn solve_2(data: &Input) -> R {
    data.len()
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

    const TEST_DATA: &str = include_str!("../../resources/00-test-00.txt");

    fn test_data() -> Input {
        parse(TEST_DATA).unwrap()
    }

    fn real_data() -> Input {
        parse(DATA).unwrap()
    }

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(&test_data()), 0);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(&real_data()), 0);
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve_2(&test_data()), 0);
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(&real_data()), 0);
    }
}
