use advent_of_code_common::parsing::Error;
use advent_of_code_common::utils::head_tail;
use num_enum::TryFromPrimitive;

const DATA: &str = include_str!("../../resources/01.txt");

#[derive(Copy, Clone, TryFromPrimitive)]
#[repr(u8)]
enum Command {
    Up = b'(',
    Down = b')',
}

impl Command {
    fn diff(self) -> i32 {
        match self {
            Command::Up => 1,
            Command::Down => -1,
        }
    }
}

type Data = Vec<Command>;

fn parse(input: &str) -> Result<Data, Error> {
    input
        .chars()
        .map(|ch| Command::try_from(ch as u8).map_err(|e| format!("{e}")))
        .collect()
}

type Output1 = i32;
type Output2 = usize;

fn solve_1(data: &Data) -> Output1 {
    data.iter().map(|c| c.diff()).sum()
}

fn scan_finding_index_which_satisfies_predicate<S, P, State, Elem>(
    remaining: &[Elem],
    state: State,
    index: usize,
    next_state: S,
    predicate: P,
) -> Option<usize>
where
    S: Fn(State, &Elem) -> State,
    P: Fn(&State) -> bool,
{
    if predicate(&state) {
        Some(index)
    } else {
        let (h, t) = head_tail(remaining);
        match h {
            None => None,
            Some(h) => scan_finding_index_which_satisfies_predicate(
                t,
                next_state(state, h),
                index + 1,
                next_state,
                predicate,
            ),
        }
    }
}

fn solve_2(data: &Data) -> Output2 {
    scan_finding_index_which_satisfies_predicate(
        data,
        0,
        0,
        |floor, c| floor + c.diff(),
        |x| *x < 0,
    )
    .unwrap_or_else(|| panic!("Not found"))
}

fn part_1(input: &str) -> Result<Output1, Error> {
    parse(input).map(|input| solve_1(&input))
}

fn part_2(input: &str) -> Result<Output2, Error> {
    parse(input).map(|input| solve_2(&input))
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

    const TEST_DATA: &str = include_str!("../../resources/01-test.txt");

    #[test]
    fn test_solve_1_test() {
        assert_eq!(part_1(TEST_DATA), Ok(-1));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(74));
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(part_2(TEST_DATA), Ok(5));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(1795));
    }
}
