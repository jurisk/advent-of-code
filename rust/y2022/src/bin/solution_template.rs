use advent_of_code_common::parsing::Error;

const DATA: &str = include_str!("../../resources/07.txt");

#[derive(Debug)]
struct Data {
    placeholder: String,
}

fn parse(input: &str) -> Result<Data, Error> {
    Ok(Data {
        placeholder: input.to_string(),
    })
}

type Output1 = usize;
type Output2 = usize;

fn solve_1(data: &Data) -> Output1 {
    data.placeholder.len()
}

fn solve_2(data: &Data) -> Output2 {
    data.placeholder.len()
}

fn part_1(input: &str) -> Result<Output1, Error> {
    let data = parse(input)?;
    Ok(solve_1(&data))
}

fn part_2(input: &str) -> Result<Output2, Error> {
    let data = parse(input)?;
    Ok(solve_2(&data))
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

    const TEST_DATA: &str = include_str!("../../resources/07-test.txt");

    #[test]
    fn test_solve_1_test() {
        assert_eq!(part_1(TEST_DATA), Ok(Output1::MAX));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(Output1::MAX));
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(part_2(TEST_DATA), Ok(Output2::MAX));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(Output2::MAX));
    }
}
