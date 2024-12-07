use advent_of_code_common::parsing::{Error, parse_lines_to_vec};

const DATA: &str = include_str!("../../resources/07.txt");

type N = u64;
type R = u64;

struct Equation {
    result: N,
    numbers: Vec<N>,
}

type Data = Vec<Equation>;

fn parse(input: &str) -> Result<Data, Error> {
    parse_lines_to_vec(input)
}

fn solve_1(data: &Data) -> R {
    data.len()
}

fn solve_2(data: &Data) -> R {
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

    const TEST_DATA: &str = include_str!("../../resources/07-test-00.txt");

    fn test_data() -> Data {
        parse(TEST_DATA).unwrap()
    }

    fn real_data() -> Data {
        parse(DATA).unwrap()
    }

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(&test_data()), 3749);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(&real_data()), 1038838357795);
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve_2(&test_data()), 11387);
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(&real_data()), 254136560217241);
    }
}
