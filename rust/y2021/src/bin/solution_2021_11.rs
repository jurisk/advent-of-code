use advent_of_code_common::parsing::{parse_u8_matrix, Error};
use pathfinding::matrix::Matrix;

const DATA: &str = include_str!("../../resources/11.txt");

type Octopuses = Matrix<u8>;

#[allow(clippy::redundant_else)]
fn step(octopuses: &Octopuses) -> (usize, Octopuses) {
    const FLASH_THRESHOLD: u8 = 9;
    const FLASHED: u8 = u8::MAX;
    let mut result = octopuses.clone();
    let mut flashes: usize = 0;

    // First, the energy level of each octopus increases by 1.
    for (r, c) in result.keys() {
        result[(r, c)] += 1;
    }

    loop {
        let mut current_batch_flashes: usize = 0;

        for (r, c) in result.keys() {
            if result[(r, c)] != FLASHED && result[(r, c)] > FLASH_THRESHOLD {
                // Then, any octopus with an energy level greater than 9 flashes. This increases
                // the energy level of all adjacent octopuses by 1, including
                // octopuses that are diagonally adjacent.

                current_batch_flashes += 1;
                result[(r, c)] = FLASHED;

                for neighbour in result.neighbours((r, c), true) {
                    result[neighbour] = result[neighbour].saturating_add(1);
                }
            }
        }

        if current_batch_flashes == 0 {
            // This process continues as long as new octopuses keep having their energy
            // level increased beyond 9. (An octopus can only flash at most once
            // per step.)
            break;
        } else {
            flashes += current_batch_flashes;
        }
    }

    // Finally, any octopus that flashed during this step has its energy level set
    // to 0, as it used all of its energy to flash.
    for (r, c) in result.keys() {
        if result[(r, c)] == FLASHED {
            result[(r, c)] = 0;
        }
    }

    (flashes, result.clone())
}

fn solve_1(input: &str, steps: usize) -> Result<usize, Error> {
    let mut octopuses: Octopuses = parse_u8_matrix(input)?;
    let mut total_flashes: usize = 0;
    for _ in 0 .. steps {
        let (new_flashes, new_octopuses) = step(&octopuses);
        total_flashes += new_flashes;
        octopuses = new_octopuses;
    }
    Ok(total_flashes)
}

fn solve_2(input: &str) -> Result<usize, Error> {
    let mut octopuses = parse_u8_matrix(input)?;
    for idx in 1 .. usize::MAX {
        let (new_flashes, new_octopuses) = step(&octopuses);
        if new_flashes == octopuses.columns * octopuses.rows {
            return Ok(idx);
        }
        octopuses = new_octopuses;
    }

    Err("Failed to find".to_string())
}

fn main() {
    let result_1 = solve_1(DATA, 100);
    println!("Part 1: {result_1:?}");

    let result_2 = solve_2(DATA);
    println!("Part 2: {result_2:?}");
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = include_str!("../../resources/11-test.txt");

    #[test]
    fn test_solve_1_test_1() {
        assert_eq!(solve_1(TEST_DATA, 10), Ok(204));
    }

    #[test]
    fn test_solve_1_test_2() {
        assert_eq!(solve_1(TEST_DATA, 100), Ok(1656));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(DATA, 100), Ok(1686));
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve_2(TEST_DATA), Ok(195));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(DATA), Ok(360));
    }
}
