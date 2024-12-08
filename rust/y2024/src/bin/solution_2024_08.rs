use std::collections::HashSet;

use advent_of_code_common::grid2d::{Coords, Grid2D, MatrixGrid2D};

const DATA: &str = include_str!("../../resources/08.txt");

type R = usize;
type Frequency = char;
type Data = MatrixGrid2D<Option<Frequency>>;

fn parse_option_frequency(input: char) -> Option<Frequency> {
    if input == '.' { None } else { Some(input) }
}

fn parse(input: &str) -> Data {
    MatrixGrid2D::parse(input, parse_option_frequency)
}

fn helper(
    antinodes: &mut MatrixGrid2D<bool>,
    current: Coords,
    diff: Coords,
    limit: Option<usize>,
    count: usize,
) {
    if antinodes.valid_coords(current) && limit.is_none_or(|l| count <= l) {
        if limit.is_none() || count != 0 {
            antinodes.set(current, true);
        }

        helper(antinodes, current + diff, diff, limit, count + 1);
    }
}

fn process_frequency(
    data: &Data,
    antinodes: &mut MatrixGrid2D<bool>,
    frequency: Frequency,
    limit: Option<usize>,
) {
    let locations = data.filter_coords_by_value(&Some(frequency));
    for a in &locations {
        for b in &locations {
            if a != b {
                helper(antinodes, *a, *a - *b, limit, 0);
            }
        }
    }
}

fn solve(data: &Data, limit: Option<usize>) -> R {
    let mut antinodes = data.map_by_coords(|_| false);
    let frequencies = data
        .iter()
        .flat_map(|(_, v)| v)
        .copied()
        .collect::<HashSet<_>>();
    for frequency in frequencies {
        process_frequency(data, &mut antinodes, frequency, limit);
    }
    antinodes.count(|_, v| *v)
}

fn solve_1(data: &Data) -> R {
    solve(data, Some(1))
}

fn solve_2(data: &Data) -> R {
    solve(data, None)
}

fn main() {
    let data = parse(DATA);

    let result_1 = solve_1(&data);
    println!("Part 1: {result_1}");

    let result_2 = solve_2(&data);
    println!("Part 2: {result_2}");
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = include_str!("../../resources/08-test-00.txt");

    fn test_data() -> Data {
        parse(TEST_DATA)
    }

    fn real_data() -> Data {
        parse(DATA)
    }

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(&test_data()), 14);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(&real_data()), 256);
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve_2(&test_data()), 34);
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(&real_data()), 1005);
    }
}
