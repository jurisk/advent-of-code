use std::collections::HashSet;

use advent_of_code_common::direction::Direction;
use advent_of_code_common::grid2d::{Coords, Grid2D, MatrixGrid2D};
use advent_of_code_common::itertools::group_by::GroupBy;
use itertools::Itertools;
use pathfinding::prelude::connected_components;

const DATA: &str = include_str!("../../resources/12.txt");

type R = usize;
type Data = MatrixGrid2D<char>;

fn parse(input: &str) -> Data {
    MatrixGrid2D::parse_char_field(input)
}

fn perimeter(coords: &HashSet<Coords>) -> R {
    coords
        .iter()
        .map(|c| {
            c.adjacent4()
                .into_iter()
                .filter(|n| !coords.contains(n))
                .count()
        })
        .sum()
}

fn ascending_sequences(data: Vec<i32>) -> usize {
    let mut data = data;
    data.sort_unstable();
    if data.is_empty() {
        0
    } else if data.len() == 1 {
        1
    } else {
        data.iter()
            .tuple_windows()
            .filter(|&(a, b)| a + 1 != *b)
            .count()
            + 1
    }
}

fn number_of_sides(garden_coords: &HashSet<Coords>) -> R {
    Direction::all()
        .into_iter()
        .map(|d| {
            garden_coords
                .iter()
                .filter(|&c| !garden_coords.contains(&(*c + d)))
                .group_by_key(|c| {
                    match d {
                        Direction::North | Direction::South => c.y,
                        Direction::East | Direction::West => c.x,
                    }
                })
                .values()
                .map(|coords| {
                    let values = coords
                        .iter()
                        .map(|c| {
                            match d {
                                Direction::North | Direction::South => c.x,
                                Direction::East | Direction::West => c.y,
                            }
                        })
                        .collect();
                    ascending_sequences(values)
                })
                .sum::<usize>()
        })
        .sum()
}

fn solve<F>(data: &Data, f: F) -> R
where
    F: Fn(&HashSet<Coords>) -> R,
{
    let coords: Vec<_> = data.coords().collect();
    let regions = connected_components(&coords, |&c| {
        data.neighbours_for(c, false)
            .into_iter()
            .filter(move |&n| data.get(n) == data.get(c))
    });

    regions
        .into_iter()
        .map(|coords| {
            let area = coords.len();
            area * f(&coords)
        })
        .sum()
}

fn solve_1(data: &Data) -> R {
    solve(data, perimeter)
}

fn solve_2(data: &Data) -> R {
    solve(data, number_of_sides)
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

    fn real_data() -> Data {
        parse(DATA)
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(&real_data()), 1_371_306);
    }

    #[test]
    fn test_solve_2_test_0() {
        assert_eq!(
            solve_2(&parse(include_str!("../../resources/12-test-00.txt"))),
            80
        );
    }

    #[test]
    fn test_solve_2_test_1() {
        assert_eq!(
            solve_2(&parse(include_str!("../../resources/12-test-01.txt"))),
            436
        );
    }

    #[test]
    fn test_solve_2_test_2() {
        assert_eq!(
            solve_2(&parse(include_str!("../../resources/12-test-02.txt"))),
            1206
        );
    }

    #[test]
    fn test_solve_2_test_3() {
        assert_eq!(
            solve_2(&parse(include_str!("../../resources/12-test-03.txt"))),
            236
        );
    }

    #[test]
    fn test_solve_2_test_4() {
        assert_eq!(
            solve_2(&parse(include_str!("../../resources/12-test-04.txt"))),
            368
        );
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(&real_data()), 805_880);
    }
}
