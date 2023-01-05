extern crate core;

use crate::Value::{And, Assign, LeftShift, Not, Or, RightShift};
use crate::WireOrLiteral::{Literal, Wire};
use advent_of_code_common::parsing::{
    parse_lines_to_vec, parse_str, split_into_two_strings, Error,
};
use std::collections::HashMap;
use std::str::FromStr;

#[derive(Clone, Debug)]
enum WireOrLiteral {
    Wire(String),
    Literal(u16),
}

#[derive(Clone, Debug)]
enum Value {
    Assign(WireOrLiteral),
    And(WireOrLiteral, WireOrLiteral),
    Or(WireOrLiteral, WireOrLiteral),
    LeftShift(WireOrLiteral, u16),
    RightShift(WireOrLiteral, u16),
    Not(WireOrLiteral),
}

impl FromStr for Value {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let wol = |s| parse_str(s).map_or_else(|_| Wire(s.to_string()), Literal);

        let words: Vec<_> = s.split(' ').collect();
        match words.as_slice() {
            [a, "AND", b] => Ok(And(wol(a), wol(b))),
            [a, "OR", b] => Ok(Or(wol(a), wol(b))),
            [a, "LSHIFT", b] => Ok(LeftShift(wol(a), parse_str(b)?)),
            [a, "RSHIFT", b] => Ok(RightShift(wol(a), parse_str(b)?)),
            ["NOT", x] => Ok(Not(wol(x))),
            [x] => Ok(Assign(wol(x))),
            _ => Err(format!("Failed to parse {s}")),
        }
    }
}

struct Entry {
    wire: String,
    value: Value,
}

impl FromStr for Entry {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (a, b) = split_into_two_strings(s, " -> ")?;
        Ok(Entry {
            wire: b,
            value: a.parse()?,
        })
    }
}

type Data = HashMap<String, Value>;

fn parse(input: &str) -> Result<Data, Error> {
    let entries: Vec<Entry> = parse_lines_to_vec(input)?;
    let result: Data = entries
        .into_iter()
        .map(|e| (e.wire.clone(), e.value))
        .collect();
    Ok(result)
}

fn on_wire<'a>(cache: &mut HashMap<&'a str, u16>, data: &'a Data, wire: &'a str) -> u16 {
    if let Some(found) = cache.get(wire) {
        *found
    } else {
        let calculated = match data
            .get(wire)
            .unwrap_or_else(|| panic!("Wire {wire:?} not found!"))
        {
            Assign(a) => f(cache, data, a),
            And(a, b) => f(cache, data, a) & f(cache, data, b),
            Or(a, b) => f(cache, data, a) | f(cache, data, b),
            LeftShift(a, b) => f(cache, data, a) << b,
            RightShift(a, b) => f(cache, data, a) >> b,
            Not(a) => !f(cache, data, a),
        };

        cache.insert(wire, calculated);

        calculated
    }
}

fn f<'a>(cache: &mut HashMap<&'a str, u16>, d: &'a Data, wol: &'a WireOrLiteral) -> u16 {
    match wol {
        Wire(wire) => on_wire(cache, d, wire),
        Literal(value) => *value,
    }
}

fn solve_1(data: &Data, wire: &str) -> u16 {
    println!("{data:?}");

    let mut cache: HashMap<&str, u16> = HashMap::new();

    f(&mut cache, data, &Wire(wire.to_string()))
}

fn solve_2(data: &Data) -> u16 {
    let on_a = solve_1(data, "a");
    let mut new_data = data.clone();
    new_data.insert("b".to_string(), Assign(Literal(on_a)));
    solve_1(&new_data, "a")
}

fn part_1(input: &str, wire: &str) -> Result<u16, Error> {
    parse(input).map(|input| solve_1(&input, wire))
}

fn part_2(input: &str) -> Result<u16, Error> {
    parse(input).map(|input| solve_2(&input))
}

const DATA: &str = include_str!("../../resources/07.txt");

fn main() -> Result<(), Error> {
    let result_1 = part_1(DATA, "a")?;
    println!("Part 1: {result_1}");

    let result_2 = part_2(DATA)?;
    println!("Part 2: {result_2}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = include_str!("../../resources/07-test.txt");

    #[test]
    fn test_solve_1_test() {
        assert_eq!(part_1(TEST_DATA, "d"), Ok(72));
        assert_eq!(part_1(TEST_DATA, "e"), Ok(507));
        assert_eq!(part_1(TEST_DATA, "f"), Ok(492));
        assert_eq!(part_1(TEST_DATA, "g"), Ok(114));
        assert_eq!(part_1(TEST_DATA, "h"), Ok(65412));
        assert_eq!(part_1(TEST_DATA, "i"), Ok(65079));
        assert_eq!(part_1(TEST_DATA, "x"), Ok(123));
        assert_eq!(part_1(TEST_DATA, "y"), Ok(456));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA, "a"), Ok(3176));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(14710));
    }
}
