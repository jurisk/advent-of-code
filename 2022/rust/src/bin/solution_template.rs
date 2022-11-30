use advent_of_code_2022::parsing::ErrorMessage;

const DATA: &str = include_str!("../../resources/01.txt");

#[derive(Debug)]
struct Data {
    _placeholder: String,
}

fn parse(input: &str) -> Result<Data, ErrorMessage> {
    todo!("Parsing not implemented for '{}'", input)
}

type Output = usize;

fn solve_1(data: &Data) -> Output {
    todo!("Solving is not implemented for {:?}", data)
}

fn solve_2(data: &Data) -> Output {
    todo!("Solving is not implemented for {:?}", data)
}

fn part_1(input: &str) -> Result<Output, ErrorMessage> {
    let data = parse(input)?;
    Ok(solve_1(&data))
}

fn part_2(input: &str) -> Result<Output, ErrorMessage> {
    let data = parse(input)?;
    Ok(solve_2(&data))
}

fn main() -> Result<(), ErrorMessage> {
    let result_1 = part_1(DATA)?;
    println!("Part 1: {result_1}");

    let result_2 = part_2(DATA)?;
    println!("Part 2: {result_2}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = include_str!("../../resources/01-test.txt");

    #[test]
    fn test_solve_1_test() {
        assert_eq!(part_1(TEST_DATA), Ok(Output::MAX));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(Output::MAX));
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(part_2(TEST_DATA), Ok(Output::MAX));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(Output::MAX));
    }
}
