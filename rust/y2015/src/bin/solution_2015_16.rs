use std::collections::HashMap;
use std::str::FromStr;

use advent_of_code_common::parsing::{Error, parse_lines_to_hashmap, parse_str};

type AuntId = String;

#[derive(Debug)]
struct Info {
    map: HashMap<String, u32>,
}

impl Info {
    fn comparison_1(_name: &str, at_sue: u32, at_ticker: u32) -> bool {
        at_sue == at_ticker
    }

    fn comparison_2(name: &str, at_sue: u32, at_ticker: u32) -> bool {
        match name {
            "cats" | "trees" => at_sue > at_ticker,
            "pomeranians" | "goldfish" => at_sue < at_ticker,
            _ => at_sue == at_ticker,
        }
    }

    fn is_possible<F>(&self, ticker: &Info, comparison: F) -> bool
    where
        F: Fn(&str, u32, u32) -> bool,
    {
        ticker.map.iter().all(|(name, value)| {
            if let Some(contains) = self.map.get(name) {
                comparison(name, *contains, *value)
            } else {
                true
            }
        })
    }
}

impl FromStr for Info {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let map = parse_lines_to_hashmap(s, ": ")?;
        Ok(Info { map })
    }
}

fn parse_sues(input: &str) -> Result<HashMap<AuntId, Info>, Error> {
    let result: Result<HashMap<AuntId, Info>, Error> = input
        .lines()
        .map(|s| {
            let split_position = s
                .chars()
                .position(|x| x == ':')
                .unwrap_or_else(|| panic!("Unexpected format"));
            let (a, b) = s.split_at(split_position);
            let info: Info = parse_str(&b[2 ..].replace(", ", "\n"))?;
            Ok((a.to_string(), info))
        })
        .collect();
    result
}

fn solve<F>(sues: &HashMap<AuntId, Info>, ticker: &Info, f: F) -> AuntId
where
    F: Fn(&str, u32, u32) -> bool,
{
    println!("{sues:?} {ticker:?}");
    let potential: Vec<_> = sues
        .iter()
        .filter(|(_id, sue)| sue.is_possible(ticker, &f))
        .collect();
    println!("{potential:?}");
    assert_eq!(potential.len(), 1);
    potential[0].0.to_string()
}

fn part_1(sues_input: &str, ticker_input: &str) -> Result<AuntId, Error> {
    let sues = parse_sues(sues_input)?;
    let ticker = parse_str(ticker_input)?;
    Ok(solve(&sues, &ticker, Info::comparison_1))
}

fn part_2(sues_input: &str, ticker_input: &str) -> Result<AuntId, Error> {
    let sues = parse_sues(sues_input)?;
    let ticker = parse_str(ticker_input)?;
    Ok(solve(&sues, &ticker, Info::comparison_2))
}

const SUES: &str = include_str!("../../resources/16.txt");
const TICKER: &str = include_str!("../../resources/16-ticker.txt");

fn main() -> Result<(), Error> {
    let result_1 = part_1(SUES, TICKER)?;
    println!("Part 1: {result_1}");

    let result_2 = part_2(SUES, TICKER)?;
    println!("Part 2: {result_2}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(SUES, TICKER), Ok("Sue 213".to_string()));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(SUES, TICKER), Ok("Sue 323".to_string()));
    }
}
