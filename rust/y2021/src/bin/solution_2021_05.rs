use std::str::FromStr;

use advent_of_code_common::coords2d::Coords2D;
use advent_of_code_common::parsing::Error;
use itertools::Itertools;

#[derive(Debug)]
struct Vent {
    from: Coords2D<usize>,
    to:   Coords2D<usize>,
}

impl Vent {
    fn from_to(from: usize, to: usize) -> Vec<usize> {
        if from < to {
            (from ..= to).collect()
        } else {
            (to ..= from).rev().collect()
        }
    }

    fn all_points(&self) -> Vec<Coords2D<usize>> {
        if self.from.x == self.to.x {
            Self::from_to(self.from.y, self.to.y)
                .iter()
                .map(|y| Coords2D::new(self.from.x, *y))
                .collect()
        } else if self.from.y == self.to.y {
            Self::from_to(self.from.x, self.to.x)
                .iter()
                .map(|x| Coords2D::new(*x, self.from.y))
                .collect()
        } else {
            Self::from_to(self.from.x, self.to.x)
                .iter()
                .zip(Self::from_to(self.from.y, self.to.y).iter())
                .map(|(x, y)| Coords2D::new(*x, *y))
                .collect()
        }
    }

    fn horizontal_or_vertical(&self) -> bool {
        self.from.x == self.to.x || self.from.y == self.to.y
    }

    fn diagonal(&self) -> bool {
        self.to.x.abs_diff(self.from.x) == self.to.y.abs_diff(self.from.y)
    }

    fn horizontal_or_vertical_or_diagonal(&self) -> bool {
        self.horizontal_or_vertical() || self.diagonal()
    }
}

impl FromStr for Vent {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let v: Vec<_> = s.split(" -> ").collect();
        if v.len() == 2 {
            let from = Coords2D::from_str(v[0])?;
            let to = Coords2D::from_str(v[1])?;
            Ok(Vent { from, to })
        } else {
            Err(format!("Invalid vent {s}").to_string())
        }
    }
}

fn parse(input: &str) -> Result<Vec<Vent>, Error> {
    input.lines().map(Vent::from_str).collect()
}

fn solve<F>(vents: Vec<Vent>, valid: F) -> usize
where
    F: Fn(&Vent) -> bool,
{
    vents
        .into_iter()
        .filter(valid)
        .flat_map(|x| x.all_points())
        .counts()
        .into_iter()
        .filter(|(_coord, times_seen)| *times_seen > 1)
        .count()
}

fn part_1(input: &str) -> Result<usize, Error> {
    let vents = parse(input)?;
    Ok(solve(vents, Vent::horizontal_or_vertical))
}

fn part_2(input: &str) -> Result<usize, Error> {
    let vents = parse(input)?;
    Ok(solve(vents, Vent::horizontal_or_vertical_or_diagonal))
}

const DATA: &str = include_str!("../../resources/05.txt");

fn main() {
    let result_1 = part_1(DATA);
    println!("Part 1: {result_1:?}");

    let result_2 = part_2(DATA);
    println!("Part 2: {result_2:?}");
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST: &str = include_str!("../../resources/05-test.txt");

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(TEST), Ok(5));
    }

    #[test]
    fn real_part_1() {
        assert_eq!(part_1(DATA), Ok(5147));
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(TEST), Ok(12));
    }

    #[test]
    fn real_part_2() {
        assert_eq!(part_2(DATA), Ok(16925));
    }
}
