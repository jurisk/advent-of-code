use advent_of_code_common::parsing::{parse_lines_to_vec, Error};
use recap::Recap;
use serde::Deserialize;
use std::cmp::min;

#[derive(Debug, Deserialize, Recap)]
#[recap(
    regex = r#"^\w+ can fly (?P<speed>\d+) km/s for (?P<fly_for_how_long>\d+) seconds, but then must rest for (?P<rest_for_how_long>\d+) seconds.$"#
)]
struct Reindeer {
    speed: u32,
    fly_for_how_long: u32,
    rest_for_how_long: u32,
}

impl Reindeer {
    #[inline]
    fn cycle_time(&self) -> u32 {
        self.fly_for_how_long + self.rest_for_how_long
    }

    #[inline]
    fn distance_covered(&self, seconds: u32) -> u32 {
        let full = seconds / self.cycle_time();
        let incomplete = min(seconds % self.cycle_time(), self.fly_for_how_long);
        (full * self.fly_for_how_long + incomplete) * self.speed
    }
}

fn parse(input: &str) -> Result<Vec<Reindeer>, Error> {
    parse_lines_to_vec(input)
}

const SECONDS: u32 = 2503;
fn solve_1(data: &[Reindeer]) -> u32 {
    data.iter()
        .map(|r| r.distance_covered(SECONDS))
        .max()
        .unwrap()
}

fn solve_2(data: &[Reindeer]) -> u32 {
    let mut points: Vec<u32> = vec![0; data.len()];
    for time in 1..=SECONDS {
        let distances: Vec<u32> = data.iter().map(|r| r.distance_covered(time)).collect();
        let max_distance = distances.iter().max().unwrap();
        for (idx, distance) in distances.iter().enumerate() {
            if distance == max_distance {
                points[idx] += 1;
            }
        }
    }

    *points.iter().max().unwrap()
}

fn part_1(input: &str) -> Result<u32, Error> {
    let data = parse(input)?;
    Ok(solve_1(&data))
}

fn part_2(input: &str) -> Result<u32, Error> {
    let data = parse(input)?;
    Ok(solve_2(&data))
}

const DATA: &str = include_str!("../../resources/14.txt");

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

    #[test]
    fn test_comet() {
        let comet: Reindeer =
            "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds."
                .parse()
                .unwrap();
        assert_eq!(comet.distance_covered(1000), 1120);
    }

    #[test]
    fn test_dancer() {
        let dancer: Reindeer =
            "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."
                .parse()
                .unwrap();
        assert_eq!(dancer.distance_covered(1000), 1056);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(2660));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(1256));
    }
}
