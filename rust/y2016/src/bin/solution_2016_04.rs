use std::collections::HashMap;
use std::str::FromStr;

use advent_of_code_common::parsing::{Error, parse_lines_to_vec, parse_str};
use advent_of_code_common::utils::additive_hashmap_from_vec;
use itertools::Itertools;
use memoize::lazy_static::lazy_static;
use regex::Regex;

#[derive(Debug)]
struct Room {
    encrypted_name: Vec<String>,
    id:             u32,
    checksum:       String,
}

impl FromStr for Room {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        lazy_static! {
            static ref RE: Regex = Regex::new(r"([\w\-]+)-(\d+)\[(\w+)\]").unwrap();
        }

        let captures = RE.captures(s).unwrap();
        let groups = (captures.get(1), captures.get(2), captures.get(3));

        match groups {
            (Some(a), Some(b), Some(c)) => {
                Ok(Room {
                    encrypted_name: a.as_str().split('-').map(ToString::to_string).collect(),
                    id:             parse_str(b.as_str())?,
                    checksum:       parse_str(c.as_str())?,
                })
            },
            _ => Err(format!("Failed to match {s}")),
        }
    }
}

impl Room {
    fn decrypt_char(&self, ch: char) -> char {
        let a = 'a' as u32;
        let z = 'z' as u32;
        let count = z - a + 1;
        u8::try_from((ch as u32 - a + self.id) % count + a).unwrap() as char
    }

    fn decrypt(&self, s: &str) -> String {
        s.chars().map(|ch| self.decrypt_char(ch)).collect()
    }

    fn decrypted_name(&self) -> String {
        self.encrypted_name
            .iter()
            .map(|s| self.decrypt(s))
            .join(" ")
    }

    fn letter_counts(&self) -> HashMap<char, usize> {
        let vectors = self
            .encrypted_name
            .iter()
            .map(|x| x.chars().counts().into_iter().collect::<Vec<_>>())
            .concat();

        additive_hashmap_from_vec(vectors)
    }

    fn is_valid(&self) -> bool {
        let counts = self.letter_counts();
        let expected: String = counts
            .into_iter()
            .sorted_by_key(|(ch, count)| (-isize::try_from(*count).unwrap(), *ch as u8))
            .take(5)
            .map(|(ch, _)| ch)
            .collect();

        expected == self.checksum
    }
}

type Data = Vec<Room>;

fn parse(input: &str) -> Result<Data, Error> {
    parse_lines_to_vec(input)
}

fn solve_1(data: &Data) -> u32 {
    data.iter()
        .filter(|&x| Room::is_valid(x))
        .map(|x| x.id)
        .sum()
}

fn solve_2(data: &Data) -> u32 {
    data.iter()
        .filter(|&x| Room::is_valid(x))
        .find(|r| r.decrypted_name() == "northpole object storage")
        .unwrap()
        .id
}

fn part_1(input: &str) -> Result<u32, Error> {
    parse(input).map(|input| solve_1(&input))
}

fn part_2(input: &str) -> Result<u32, Error> {
    parse(input).map(|input| solve_2(&input))
}

const DATA: &str = include_str!("../../resources/04.txt");

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
    fn test_solve_1_test_1() {
        assert_eq!(part_1("aaaaa-bbb-z-y-x-123[abxyz]"), Ok(123));
    }

    #[test]
    fn test_solve_1_test_2() {
        assert_eq!(part_1("a-b-c-d-e-f-g-h-987[abcde]"), Ok(987));
    }

    #[test]
    fn test_solve_1_test_3() {
        assert_eq!(part_1("not-a-real-room-404[oarel]"), Ok(404));
    }

    #[test]
    fn test_solve_1_test_4() {
        assert_eq!(part_1("totally-real-room-200[decoy] "), Ok(0));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(245_102));
    }

    #[test]
    fn test_decrypt() {
        let test: Room = "qzmt-zixmtkozy-ivhz-343[aaaaa]".parse().unwrap();
        assert_eq!(test.decrypted_name(), "very encrypted name");
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(324));
    }
}
