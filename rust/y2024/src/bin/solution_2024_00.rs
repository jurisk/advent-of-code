use advent_of_code_common::parsing::{Error, parse_lines_to_vec};

const DATA: &str = include_str!("../../resources/00.txt");

type N = u32;
type Data = Vec<N>;

fn parse(input: &str) -> Result<Data, Error> {
    parse_lines_to_vec(input)
}

fn solve_1(data: Data) -> N {
    data.into_iter().sum()
}

fn solve_2(data: Data) -> N {
    data.into_iter().sum()
}

fn part_1(input: &str) -> Result<N, Error> {
    let data = parse(input)?;
    Ok(solve_1(data))
}

fn part_2(input: &str) -> Result<N, Error> {
    let data = parse(input)?;
    Ok(solve_2(data))
}

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

    const TEST_DATA: &str = include_str!("../../resources/00-test-00.txt");

    #[test]
    fn test_solve_1_test() {
        assert_eq!(part_1(TEST_DATA), Ok(0));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(0));
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(part_2(TEST_DATA), Ok(0));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(0));
    }
}
