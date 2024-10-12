use std::slice::Iter;

use advent_of_code_common::parsing::{
    Error, parse_ascii_whitespace_separated_vec, parse_lines_to_vec_passing_parser,
};
use itertools::{Chunk, Itertools};

struct Triangle {
    a: u32,
    b: u32,
    c: u32,
}

impl Triangle {
    fn is_valid(&self) -> bool {
        self.a + self.b > self.c
    }

    fn from_vec(vec: &[u32]) -> Self {
        let mut v: Vec<u32> = vec.to_vec();
        assert_eq!(v.len(), 3);
        v.sort_unstable();
        Triangle {
            a: v[0],
            b: v[1],
            c: v[2],
        }
    }
}

type Data = Vec<Vec<u32>>;

fn parse(input: &str) -> Result<Data, Error> {
    parse_lines_to_vec_passing_parser(input, parse_ascii_whitespace_separated_vec)
}

fn solve_1(data: &Data) -> usize {
    data.iter()
        .map(|x| Triangle::from_vec(x))
        .filter(Triangle::is_valid)
        .count()
}

fn solve_2(data: &Data) -> usize {
    fn f(a: u32, b: u32, c: u32) -> usize {
        usize::from(Triangle::from_vec(&[a, b, c]).is_valid())
    }

    fn helper(ch: Chunk<Iter<Vec<u32>>>) -> usize {
        let v: Vec<&Vec<u32>> = ch.into_iter().collect();
        assert_eq!(v.len(), 3);
        f(v[0][0], v[1][0], v[2][0]) + f(v[0][1], v[1][1], v[2][1]) + f(v[0][2], v[1][2], v[2][2])
    }

    assert_eq!(data.len() % 3, 0);
    data.iter().chunks(3).into_iter().map(helper).sum()
}

fn part_1(input: &str) -> Result<usize, Error> {
    parse(input).map(|input| solve_1(&input))
}

fn part_2(input: &str) -> Result<usize, Error> {
    parse(input).map(|input| solve_2(&input))
}

const DATA: &str = include_str!("../../resources/03.txt");

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
        assert_eq!(part_1(DATA), Ok(1032));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(1838));
    }
}
