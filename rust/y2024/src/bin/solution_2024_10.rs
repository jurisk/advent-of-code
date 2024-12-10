use std::collections::HashSet;

use advent_of_code_common::grid2d::{Coords, Grid2D, MatrixGrid2D};

const DATA: &str = include_str!("../../resources/10.txt");

type N = u8;
type R = usize;
type Data = MatrixGrid2D<N>;

const START: N = 0;
const FINISH: N = 9;

#[expect(clippy::cast_possible_truncation)]
fn parse(input: &str) -> Data {
    MatrixGrid2D::parse(input, |c| {
        c.to_digit(10).map(|n| n as N).expect("Invalid digit")
    })
}

fn count_reachable_ends_helper(acc: &mut HashSet<Coords>, data: &Data, c: Coords, current: N) {
    if current == FINISH {
        acc.insert(c);
    } else {
        let next = current + 1;
        data.neighbours_for(c, false)
            .into_iter()
            .filter(|n| data.get(*n).is_some_and(|v| *v == next))
            .for_each(|n| count_reachable_ends_helper(acc, data, n, next));
    }
}

fn count_reachable_ends(data: &Data, c: Coords) -> R {
    let mut acc = HashSet::new();
    count_reachable_ends_helper(&mut acc, data, c, 0);
    acc.len()
}

fn count_distinct_paths_helper(data: &Data, c: Coords, current: N) -> R {
    if current == FINISH {
        1
    } else {
        let next = current + 1;
        data.neighbours_for(c, false)
            .into_iter()
            .filter(|n| data.get(*n).is_some_and(|v| *v == next))
            .map(|n| count_distinct_paths_helper(data, n, next))
            .sum()
    }
}

fn count_distinct_paths(data: &Data, c: Coords) -> R {
    count_distinct_paths_helper(data, c, 0)
}

fn solve<F>(data: &Data, f: F) -> R
where
    F: Fn(&Data, Coords) -> R,
{
    let candidate_trail_heads = data.filter_coords_by_value(&START);
    candidate_trail_heads.iter().map(|c| f(data, *c)).sum()
}

fn solve_1(data: &Data) -> R {
    solve(data, count_reachable_ends)
}

fn solve_2(data: &Data) -> R {
    solve(data, count_distinct_paths)
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

    const TEST_DATA: &str = include_str!("../../resources/10-test-00.txt");

    fn test_data() -> Data {
        parse(TEST_DATA)
    }

    fn real_data() -> Data {
        parse(DATA)
    }

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(&test_data()), 36);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(&real_data()), 811);
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve_2(&test_data()), 81);
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(&real_data()), 1794);
    }
}
