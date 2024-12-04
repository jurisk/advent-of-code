use std::collections::BTreeSet;

use advent_of_code_common::direction_with_diagonals::DirectionWithDiagonals;
use advent_of_code_common::direction_with_diagonals::DirectionWithDiagonals::{NE, NW, SE, SW};
use advent_of_code_common::grid2d::{Coords, Grid2D, MatrixGrid2D};
use advent_of_code_common::parsing::Error;
use advent_of_code_common::utils::head_tail;

const DATA: &str = include_str!("../../resources/04.txt");

type C = char;
type R = usize;

const X: C = 'X';
const M: C = 'M';
const A: C = 'A';
const S: C = 'S';

const XMAS: [C; 4] = [X, M, A, S];

fn parse(input: &str) -> Result<MatrixGrid2D<char>, Error> {
    input.parse()
}

fn is_valid_1<D: Grid2D<char>>(
    data: &D,
    coords: Coords,
    direction: DirectionWithDiagonals,
    options: &[char],
) -> bool {
    match head_tail(options) {
        (Some(&head), tail) => {
            if data.get(coords) == Some(&head) {
                is_valid_1(data, coords + direction, direction, tail)
            } else {
                false
            }
        },
        _ => true,
    }
}

fn solve_1<D: Grid2D<char>>(data: &D) -> R {
    data.map_by_coords(|c| {
        DirectionWithDiagonals::all()
            .into_iter()
            .filter(|direction| is_valid_1(data, c, *direction, &XMAS))
            .count()
    })
    .sum()
}

fn is_valid_2<D: Grid2D<char>>(data: &D, coords: Coords) -> bool {
    let corners: BTreeSet<BTreeSet<char>> =
        BTreeSet::from([BTreeSet::from([NE, SW]), BTreeSet::from([NW, SE])])
            .into_iter()
            .map(|directions| {
                directions
                    .into_iter()
                    .flat_map(|direction| data.get(coords + direction).copied())
                    .collect()
            })
            .collect();

    data.get(coords) == Some(&A) && corners == BTreeSet::from([BTreeSet::from([M, S])])
}

fn solve_2<D: Grid2D<char>>(data: &D) -> R {
    data.map_by_coords(|c| is_valid_2(data, c))
        .count(|_, valid| *valid)
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

    const TEST_DATA: &str = include_str!("../../resources/04-test-00.txt");

    fn test_data() -> MatrixGrid2D<char> {
        parse(TEST_DATA).unwrap()
    }

    fn real_data() -> MatrixGrid2D<char> {
        parse(DATA).unwrap()
    }

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(&test_data()), 18);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(&real_data()), 2468);
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve_2(&test_data()), 9);
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(&real_data()), 1864);
    }
}
