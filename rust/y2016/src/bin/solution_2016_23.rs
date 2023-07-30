use advent_of_code_2016::assembunny::{parse_instructions, Instruction, State, N};
use advent_of_code_common::parsing::Error;

fn solve(instructions: &[Instruction], eggs: N) -> N {
    State::new(instructions, &[('a', eggs)])
        .run_to_termination()
        .registers[&'a']
}

fn parse_and_solve(input: &str, eggs: N) -> Result<N, Error> {
    parse_instructions(input).map(|instructions| solve(&instructions, eggs))
}

const DATA: &str = include_str!("../../resources/23.txt");

fn main() -> Result<(), Error> {
    let result_1 = parse_and_solve(DATA, 7)?;
    println!("Part 1: {result_1:?}");

    let result_2 = parse_and_solve(DATA, 12)?;
    println!("Part 2: {result_2:?}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST: &str = include_str!("../../resources/23-test.txt");

    #[test]
    fn test_solve_1_test() {
        assert_eq!(parse_and_solve(TEST, 7), Ok(3));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(parse_and_solve(DATA, 7), Ok(11_026));
    }

    #[test]
    #[ignore]
    fn test_solve_2_real() {
        assert_eq!(parse_and_solve(DATA, 12), Ok(479_007_586));
    }
}
