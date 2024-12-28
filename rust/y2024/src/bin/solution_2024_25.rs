use advent_of_code_common::grid2d::{Grid2D, MatrixGrid2D};
use advent_of_code_common::parsing::{
    Error, parse_double_newline_separated_lines_to_vec_passing_parser,
};
use itertools::{Either, Itertools};

const DATA: &str = include_str!("../../resources/25.txt");

type N = usize;
type R = usize;
type Input = Vec<MatrixGrid2D<bool>>;

fn parse(input: &str) -> Result<Input, Error> {
    parse_double_newline_separated_lines_to_vec_passing_parser(input, |s| {
        Ok(MatrixGrid2D::parse_bool_field(s))
    })
}

const HEIGHT: N = 7;

struct Key {
    heights: Vec<N>,
}
struct Lock {
    heights: Vec<N>,
}

fn fits(key: &Key, lock: &Lock) -> bool {
    lock.heights
        .iter()
        .zip(key.heights.iter())
        .all(|(l, k)| l + k <= HEIGHT - 2)
}

#[expect(clippy::needless_pass_by_value)]
fn convert(field: MatrixGrid2D<bool>) -> Either<Lock, Key> {
    let heights: Vec<usize> = field
        .all_column_values()
        .into_iter()
        .map(|c: Vec<bool>| c.iter().filter(|b| **b).count() - 1)
        .collect::<Vec<_>>();
    let first_row = field.first_row_values();
    let last_row = field.last_row_values();
    match (
        first_row.iter().all(|&v| v) && last_row.iter().all(|&v| !v),
        first_row.iter().all(|&v| !v) && last_row.iter().all(|&v| v),
    ) {
        (true, false) => Either::Left(Lock { heights }),
        (false, true) => Either::Right(Key { heights }),
        _ => panic!("Invalid field"),
    }
}

fn solve_1(data: Input) -> R {
    let (locks, keys): (Vec<_>, Vec<_>) = data.into_iter().partition_map(convert);
    let mut result = 0;
    for lock in &locks {
        for key in &keys {
            if fits(key, lock) {
                result += 1;
            }
        }
    }
    result
}

fn main() -> Result<(), Error> {
    let data = parse(DATA)?;

    let result_1 = solve_1(data);
    println!("Part 1: {result_1}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = include_str!("../../resources/25-test-00.txt");

    fn test_data() -> Input {
        parse(TEST_DATA).unwrap()
    }

    fn real_data() -> Input {
        parse(DATA).unwrap()
    }

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(test_data()), 3);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(real_data()), 3301);
    }
}
