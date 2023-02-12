use advent_of_code_common::matrices::{IsCorner, MapByCoords};
use advent_of_code_common::parsing::{parse_matrix, Error};
use advent_of_code_common::simulate::n_steps;
use pathfinding::matrix::Matrix;

type Data = Matrix<bool>;

fn parse(input: &str) -> Result<Data, Error> {
    parse_matrix(input, |ch| {
        match ch {
            '.' => Ok(false),
            '#' => Ok(true),
            _ => Err(format!("Unrecognized `{ch}`")),
        }
    })
}

fn next_value_1(data: &Data, coords: (usize, usize)) -> bool {
    let neighbours_on: u8 = data
        .neighbours(coords, true)
        .map(|neighbour_coords| u8::from(data[neighbour_coords]))
        .sum();
    if data[coords] {
        neighbours_on == 2 || neighbours_on == 3
    } else {
        neighbours_on == 3
    }
}

fn next_value_2(data: &Data, coords: (usize, usize)) -> bool {
    if data.is_corner(coords) {
        true
    } else {
        let neighbours_on: u8 = data
            .neighbours(coords, true)
            .map(|neighbour_coords| {
                let neighbour = if data.is_corner(neighbour_coords) {
                    true
                } else {
                    data[neighbour_coords]
                };
                u8::from(neighbour)
            })
            .sum();
        if data[coords] {
            neighbours_on == 2 || neighbours_on == 3
        } else {
            neighbours_on == 3
        }
    }
}

fn solution<F>(input: &str, steps: usize, next_value: F) -> Result<usize, Error>
where
    F: Fn(&Data, (usize, usize)) -> bool,
{
    let data = parse(input)?;
    let resulting = n_steps(&data, steps, |current| {
        current.map_by_coords(|coords| next_value(current, coords))
    });

    let result = resulting.values().filter(|&&b| b).count();
    Ok(result)
}

fn part_1(input: &str, steps: usize) -> Result<usize, Error> {
    solution(input, steps, next_value_1)
}

fn part_2(input: &str, steps: usize) -> Result<usize, Error> {
    solution(input, steps, next_value_2)
}

const DATA: &str = include_str!("../../resources/18.txt");

fn main() -> Result<(), Error> {
    let result_1 = part_1(DATA, 100)?;
    println!("Part 1: {result_1}");

    let result_2 = part_2(DATA, 100)?;
    println!("Part 2: {result_2}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = include_str!("../../resources/18-test.txt");

    #[test]
    fn test_solve_1_test() {
        assert_eq!(part_1(TEST_DATA, 4), Ok(4));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA, 100), Ok(768));
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(part_2(TEST_DATA, 5), Ok(17));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA, 100), Ok(781));
    }
}
