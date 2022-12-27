use advent_of_code_common::coords2d::Coords2D;
use advent_of_code_common::direction::Direction;
use advent_of_code_common::parsing::Error;
use itertools::Itertools;
use std::collections::HashSet;

const DATA: &str = include_str!("../../resources/03.txt");

type Coords = Coords2D<i32>;
type Data = Vec<Direction>;

fn parse(input: &str) -> Result<Data, Error> {
    input.chars().map(Direction::try_from_caret).collect()
}

type Output1 = usize;
type Output2 = usize;

fn visited_coords(directions: &Data) -> HashSet<Coords> {
    let mut current = Coords::origin();
    let mut visited: HashSet<Coords> = HashSet::new();
    visited.insert(current);

    for direction in directions {
        current = current.move_in_direction(*direction);
        visited.insert(current);
    }

    visited
}

fn solve_1(directions: &Data) -> Output1 {
    visited_coords(directions).len()
}

fn solve_2(data: &Data) -> Output2 {
    assert_eq!(
        data.len() % 2,
        0,
        "Assuming Santa and Robo-Santa have same amount of instructions."
    );
    let pairs: Vec<(&Direction, &Direction)> = data.iter().tuples().collect();
    let commands_a = pairs.iter().map(|(a, _)| **a).collect();
    let commands_b = pairs.iter().map(|(_, b)| **b).collect();
    let visited_a = visited_coords(&commands_a);
    let visited_b = visited_coords(&commands_b);
    let visited: HashSet<&Coords> = visited_a.union(&visited_b).collect();
    visited.len()
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

    const TEST_DATA_1: &str = ">";
    const TEST_DATA_2: &str = "^>v<";
    const TEST_DATA_3: &str = "^v^v^v^v^v";

    #[test]
    fn test_solve_1_test_1() {
        assert_eq!(part_1(TEST_DATA_1), Ok(2));
    }

    #[test]
    fn test_solve_1_test_2() {
        assert_eq!(part_1(TEST_DATA_2), Ok(4));
    }

    #[test]
    fn test_solve_1_test_3() {
        assert_eq!(part_1(TEST_DATA_3), Ok(2));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(2572));
    }

    #[test]
    fn test_solve_2_test_2() {
        assert_eq!(part_2(TEST_DATA_2), Ok(3));
    }

    #[test]
    fn test_solve_2_test_3() {
        assert_eq!(part_2(TEST_DATA_3), Ok(11));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(2631));
    }
}
