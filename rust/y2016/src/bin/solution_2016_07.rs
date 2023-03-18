use std::str;
use std::str::FromStr;

use advent_of_code_common::parsing;
use advent_of_code_common::parsing::parse_lines_to_vec;
use itertools::Itertools;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag};
use nom::combinator::{complete, map};
use nom::error::ParseError;
use nom::error::{Error, ErrorKind};
use nom::multi::many0;
use nom::sequence::delimited;
use nom::Err;
use nom::{Finish, IResult};

#[derive(Debug)]
enum Element {
    SuperNet { value: String },
    HyperNet { value: String },
}

#[derive(Debug)]
struct Ipv7Addr {
    elements: Vec<Element>,
}

fn super_net(input: &str) -> IResult<&str, Element> {
    if input.is_empty() {
        Err(Err::Error(Error::from_error_kind(input, ErrorKind::Many0)))
    } else {
        map(is_not("["), |chars: &str| {
            Element::SuperNet {
                value: chars.to_string(),
            }
        })(input)
    }
}

fn hyper_net(input: &str) -> IResult<&str, Element> {
    map(delimited(tag("["), is_not("]"), tag("]")), |chars: &str| {
        Element::HyperNet {
            value: chars.to_string(),
        }
    })(input)
}

fn element(input: &str) -> IResult<&str, Element> {
    alt((hyper_net, super_net))(input)
}

impl FromStr for Ipv7Addr {
    type Err = parsing::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parsed = complete(many0(element))(s);
        let (_, result) =
            Finish::finish(parsed).map_err(|err| format!("{err:?} {:?}", err.code))?;
        Ok(Ipv7Addr { elements: result })
    }
}

impl Ipv7Addr {
    fn contains_abba(s: &str) -> bool {
        s.chars()
            .tuple_windows()
            .any(|(a, b, c, d)| a == d && b == c && a != b)
    }

    fn abba_in_super_net(&self) -> bool {
        self.super_net_sequences()
            .iter()
            .any(|x| Ipv7Addr::contains_abba(x))
    }

    fn abba_in_hyper_net(&self) -> bool {
        self.hyper_net_sequences()
            .iter()
            .any(|x| Ipv7Addr::contains_abba(x))
    }

    fn super_net_sequences(&self) -> Vec<String> {
        self.elements
            .iter()
            .filter_map(|x| {
                match x {
                    Element::SuperNet { value } => Some(value.to_string()),
                    Element::HyperNet { .. } => None,
                }
            })
            .collect()
    }

    fn hyper_net_sequences(&self) -> Vec<String> {
        self.elements
            .iter()
            .filter_map(|x| {
                match x {
                    Element::SuperNet { .. } => None,
                    Element::HyperNet { value } => Some(value.to_string()),
                }
            })
            .collect()
    }

    fn supports_tls(&self) -> bool {
        self.abba_in_super_net() && !self.abba_in_hyper_net()
    }

    fn candidate_abas(&self) -> Vec<(char, char)> {
        self.super_net_sequences()
            .iter()
            .flat_map(|s| {
                s.chars()
                    .tuple_windows()
                    .filter(|(a, b, c)| a == c && a != b)
                    .map(|(a, b, _)| (a, b))
                    .collect::<Vec<_>>()
            })
            .collect()
    }

    fn has_bab(s: &str, expected_a: char, expected_b: char) -> bool {
        s.chars()
            .tuple_windows()
            .any(|(a, b, c)| a == expected_b && b == expected_a && c == expected_b)
    }

    fn has_bab_for_aba(&self, aba: (char, char)) -> bool {
        let (a, b) = aba;
        self.hyper_net_sequences()
            .iter()
            .any(|x| Ipv7Addr::has_bab(x, a, b))
    }

    fn supports_ssl(&self) -> bool {
        self.candidate_abas()
            .iter()
            .any(|&aba| self.has_bab_for_aba(aba))
    }
}

type Data = Vec<Ipv7Addr>;

fn parse(input: &str) -> Result<Data, parsing::Error> {
    parse_lines_to_vec(input)
}

fn solve_1(data: &Data) -> usize {
    data.iter().filter(|x| x.supports_tls()).count()
}

fn solve_2(data: &Data) -> usize {
    data.iter().filter(|x| x.supports_ssl()).count()
}

fn part_1(input: &str) -> Result<usize, parsing::Error> {
    parse(input).map(|input| solve_1(&input))
}

fn part_2(input: &str) -> Result<usize, parsing::Error> {
    parse(input).map(|input| solve_2(&input))
}

const DATA: &str = include_str!("../../resources/07.txt");

fn main() -> Result<(), parsing::Error> {
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
    fn test_solve_1_test_0() {
        assert_eq!(part_1(""), Ok(0));
    }

    #[test]
    fn test_solve_1_test_1() {
        assert_eq!(part_1("abba[mnop]qrst"), Ok(1));
    }

    #[test]
    fn test_solve_1_test_2() {
        assert_eq!(part_1("abcd[bddb]xyyx"), Ok(0));
    }

    #[test]
    fn test_solve_1_test_3() {
        assert_eq!(part_1("aaaa[qwer]tyui"), Ok(0));
    }

    #[test]
    fn test_solve_1_test_4() {
        assert_eq!(part_1("ioxxoj[asdfgh]zxcvbn"), Ok(1));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(110));
    }

    #[test]
    fn test_solve_2_test_1() {
        assert_eq!(part_2("aba[bab]xyz"), Ok(1));
    }

    #[test]
    fn test_solve_2_test_2() {
        assert_eq!(part_2("xyx[xyx]xyx"), Ok(0));
    }

    #[test]
    fn test_solve_2_test_3() {
        assert_eq!(part_2("aaa[kek]eke"), Ok(1));
    }

    #[test]
    fn test_solve_2_test_4() {
        assert_eq!(part_2("zazbz[bzb]cdb"), Ok(1));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(242));
    }
}
