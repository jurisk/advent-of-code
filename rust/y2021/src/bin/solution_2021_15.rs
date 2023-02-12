use advent_of_code_common::parsing::{parse_u8_matrix, Error};
use pathfinding::matrix::Matrix;
use pathfinding::prelude::dijkstra;

const DATA: &str = include_str!("../../resources/15.txt");

type Cavern = Matrix<u8>;
type Coords = (usize, usize); // (row, column)

fn successors(cavern: &Cavern, from: &Coords) -> Vec<(Coords, usize)> {
    cavern
        .neighbours(*from, false)
        .map(|c| (c, cavern[c] as usize))
        .collect()
}

fn cost(cavern: &Cavern) -> Result<usize, Error> {
    let start: Coords = (0, 0);
    let end: Coords = (cavern.rows - 1, cavern.columns - 1);
    let (_, cost) =
        dijkstra(&start, |n| successors(cavern, n), |(x, y)| (*x, *y) == end).ok_or("Not found")?;

    Ok(cost)
}

fn wrap_risk(risk: usize) -> u8 {
    u8::try_from(if risk > 9 { risk - 9 } else { risk }).unwrap()
}

fn extend_cavern(cavern: &Cavern) -> Cavern {
    const COEF: usize = 5;
    let mut result: Cavern = Matrix::new(cavern.rows * COEF, cavern.columns * COEF, 0);
    for mr in 0 .. COEF {
        for mc in 0 .. COEF {
            for r in 0 .. cavern.rows {
                for c in 0 .. cavern.columns {
                    result[(r + mr * cavern.rows, c + mc * cavern.columns)] =
                        wrap_risk(cavern[(r, c)] as usize + mr + mc);
                }
            }
        }
    }
    result
}

fn solve_1(input: &str) -> Result<usize, Error> {
    let cavern: Cavern = parse_u8_matrix(input)?;
    cost(&cavern)
}

fn solve_2(input: &str) -> Result<usize, Error> {
    let small_cavern: Cavern = parse_u8_matrix(input)?;
    let larger_cavern = extend_cavern(&small_cavern);
    cost(&larger_cavern)
}

fn main() {
    let result_1 = solve_1(DATA);
    println!("Part 1: {result_1:?}");

    let result_2 = solve_2(DATA);
    println!("Part 2: {result_2:?}");
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = include_str!("../../resources/15-test.txt");

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(TEST_DATA), Ok(40));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(DATA), Ok(386));
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve_2(TEST_DATA), Ok(315));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(DATA), Ok(2806));
    }
}
