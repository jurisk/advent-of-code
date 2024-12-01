use advent_of_code_2016::assembunny::{Instruction, N, State, parse_instructions};
use advent_of_code_common::parsing::Error;
use itertools::Either;

#[allow(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
fn valid_signal(instructions: &[Instruction], input: N) -> bool {
    // We could instead test for loops, but this is easier, and works
    const OUTPUT_THRESHOLD: usize = 256;

    let result = State::new(instructions, &[('a', input)]).run_until_matches_predicate(|state| {
        let valid_output = state
            .output
            .iter()
            .enumerate()
            .all(|(idx, x)| *x == (idx as N) % 2);

        !valid_output || state.output.len() > OUTPUT_THRESHOLD
    });
    match result {
        Either::Left(state) => state.output.len() > OUTPUT_THRESHOLD,
        Either::Right(_) => true,
    }
}

fn solve(instructions: &[Instruction]) -> N {
    let mut current = 0;
    loop {
        println!("Trying {current}...");
        if valid_signal(instructions, current) {
            return current;
        }

        current += 1;
    }
}

fn parse_and_solve(input: &str) -> Result<N, Error> {
    parse_instructions(input).map(|instructions| solve(&instructions))
}

const DATA: &str = include_str!("../../resources/25.txt");

fn main() -> Result<(), Error> {
    let result = parse_and_solve(DATA)?;
    println!("Part 1: {result:?}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    #[ignore]
    fn test_solve_1_real() {
        assert_eq!(parse_and_solve(DATA), Ok(196));
    }
}
