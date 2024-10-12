use advent_of_code_2016::assembunny::{Instruction, N, State, parse_instructions};
use advent_of_code_common::parsing::Error;

fn solve_1(instructions: &[Instruction]) -> N {
    State::new(instructions, &[]).run_to_termination().registers[&'a']
}

fn solve_2(instructions: &[Instruction]) -> N {
    State::new(instructions, &[('c', 1)])
        .run_to_termination()
        .registers[&'a']
}

fn part_1(input: &str) -> Result<N, Error> {
    parse_instructions(input).map(|instructions| solve_1(&instructions))
}

fn part_2(input: &str) -> Result<N, Error> {
    parse_instructions(input).map(|instructions| solve_2(&instructions))
}

const DATA: &str = include_str!("../../resources/12.txt");

fn main() -> Result<(), Error> {
    let result_1 = part_1(DATA)?;
    println!("Part 1: {result_1:?}");

    let result_2 = part_2(DATA);
    println!("Part 2: {result_2:?}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST: &str = include_str!("../../resources/12-test.txt");

    #[test]
    fn test_solve_1_test() {
        assert_eq!(part_1(TEST), Ok(42));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(317993));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(9227647));
    }
}
