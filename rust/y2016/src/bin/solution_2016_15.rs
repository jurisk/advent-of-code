use advent_of_code_common::parsing::{parse_lines_to_vec, Error};
use recap::Recap;
use serde::Deserialize;

#[derive(Debug, Deserialize, Recap)]
#[recap(
    regex = r#"^Disc #(?P<id>\d+) has (?P<positions>\d+) positions; at time=0, it is at position (?P<at_time_0>\d+).$"#
)]
struct Disc {
    id:        usize,
    positions: usize,
    at_time_0: usize,
}

impl Disc {
    fn valid_at(&self, time: usize) -> bool {
        let effective_time = self.id + time;
        (self.at_time_0 + effective_time) % self.positions == 0
    }
}

fn parse(input: &str) -> Result<Vec<Disc>, Error> {
    parse_lines_to_vec(input)
}

fn is_valid(time: usize, data: &[Disc]) -> bool {
    data.iter().all(|d| d.valid_at(time))
}

fn solve(data: &[Disc]) -> usize {
    for i in 0 .. usize::MAX {
        if is_valid(i, data) {
            return i;
        }
    }

    panic!("Failed to find")
}

fn part_1(input: &str) -> Result<usize, Error> {
    parse(input).map(|x| solve(&x))
}

fn part_2(input: &str) -> Result<usize, Error> {
    let mut discs = parse(input)?;
    let extra_disc = Disc {
        id:        discs.len() + 1,
        positions: 11,
        at_time_0: 0,
    };
    discs.push(extra_disc);
    let result = solve(&discs);
    Ok(result)
}

const DATA: &str = include_str!("../../resources/15.txt");

fn main() -> Result<(), Error> {
    let result_1 = part_1(DATA)?;
    println!("Part 1: {result_1:?}");

    let result_2 = part_2(DATA)?;
    println!("Part 2: {result_2:?}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST: &str = include_str!("../../resources/15-test.txt");

    #[test]
    fn test_solve_1_test() {
        assert_eq!(part_1(TEST), Ok(5));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(203660));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(2408135));
    }
}
