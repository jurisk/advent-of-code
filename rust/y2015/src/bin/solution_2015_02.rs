use advent_of_code_common::parsing::{Error, parse_lines_to_vec};
use nonempty::{NonEmpty, nonempty};
use recap::Recap;
use serde::Deserialize;

#[derive(Debug, Deserialize, Recap)]
#[recap(regex = r#"^(?P<l>\d+)x(?P<w>\d+)x(?P<h>\d+)$"#)]
struct Box {
    l: u32,
    w: u32,
    h: u32,
}

impl Box {
    fn wrapping_paper(&self) -> u32 {
        let a = self.l * self.w;
        let b = self.w * self.h;
        let c = self.h * self.l;
        let all_sides_area: NonEmpty<u32> = nonempty![a, b, c];
        2 * all_sides_area.iter().sum::<u32>() + all_sides_area.minimum()
    }

    fn ribbon(&self) -> u32 {
        let a = self.l + self.w;
        let b = self.w + self.h;
        let c = self.h + self.l;
        let all_sides_half_perimeter: NonEmpty<u32> = nonempty![a, b, c];
        let bow = self.l * self.w * self.h;
        2 * all_sides_half_perimeter.minimum() + bow
    }
}

fn parse(input: &str) -> Result<Vec<Box>, Error> {
    parse_lines_to_vec(input)
}

fn solve_1(data: &[Box]) -> u32 {
    data.iter().map(Box::wrapping_paper).sum()
}

fn solve_2(data: &[Box]) -> u32 {
    data.iter().map(Box::ribbon).sum()
}

fn part_1(input: &str) -> Result<u32, Error> {
    let data = parse(input)?;
    Ok(solve_1(&data))
}

fn part_2(input: &str) -> Result<u32, Error> {
    let data = parse(input)?;
    Ok(solve_2(&data))
}

const DATA: &str = include_str!("../../resources/02.txt");

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
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(1_588_178));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(3_783_758));
    }
}
