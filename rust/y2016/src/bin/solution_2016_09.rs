use std::str;

use advent_of_code_common::parsing;
use advent_of_code_common::parsing::split_into_two_strings;
use nom::Err;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag};
use nom::combinator::{complete, map};
use nom::error::ParseError;
use nom::error::{Error, ErrorKind};
use nom::multi::many0;
use nom::sequence::delimited;
use nom::{Finish, IResult};

const DATA: &str = include_str!("../../resources/09.txt");

type Data = Vec<Element>;

#[derive(Debug)]
enum Element {
    Chars(String),
    Sequence { chars: String, times: usize },
}

trait WithLength {
    fn ordinary_len(&self) -> usize;
    fn nested_len(&self) -> usize;
}

impl WithLength for Vec<Element> {
    fn ordinary_len(&self) -> usize {
        self.iter().map(WithLength::ordinary_len).sum()
    }

    fn nested_len(&self) -> usize {
        self.iter().map(WithLength::nested_len).sum()
    }
}

impl WithLength for Element {
    fn ordinary_len(&self) -> usize {
        match self {
            Element::Chars(s) => s.len(),
            Element::Sequence { chars, times } => chars.len() * times,
        }
    }

    fn nested_len(&self) -> usize {
        match self {
            Element::Chars(s) => s.len(),
            Element::Sequence { chars, times } => {
                let parsed = parse_1(chars).unwrap();
                parsed.nested_len() * times
            },
        }
    }
}

fn sequence_element(input: &str) -> IResult<&str, Element> {
    let (more, (count, times)) = map(delimited(tag("("), is_not(")"), tag(")")), |chars: &str| {
        let s = chars.to_string();
        let (a, b) = split_into_two_strings(&s, "x").unwrap();
        let count: usize = a.parse().unwrap();
        let times: usize = b.parse().unwrap();
        (count, times)
    })(input)?;
    let (a, b) = more.split_at(count);
    let chars = a.to_string();
    let result = Element::Sequence { chars, times };
    Ok((b, result))
}

fn chars_element(input: &str) -> IResult<&str, Element> {
    if input.is_empty() {
        Err(Err::Error(Error::from_error_kind(input, ErrorKind::Many0)))
    } else {
        map(is_not("("), |chars: &str| Element::Chars(chars.to_string()))(input)
    }
}

fn element(input: &str) -> IResult<&str, Element> {
    alt((sequence_element, chars_element))(input)
}

fn parse_1(input: &str) -> Result<Data, parsing::Error> {
    let parsed = complete(many0(element))(input);
    let (_, result) = Finish::finish(parsed).map_err(|err| format!("{err:?} {:?}", err.code))?;
    Ok(result)
}

fn part_1(input: &str) -> Result<usize, parsing::Error> {
    parse_1(input).map(|x| x.ordinary_len())
}

fn part_2(input: &str) -> Result<usize, parsing::Error> {
    parse_1(input).map(|x| x.nested_len())
}

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
        assert_eq!(part_1("ADVENT"), Ok(6));
    }

    #[test]
    fn test_solve_1_test_1() {
        assert_eq!(part_1("A(1x5)BC"), Ok(7));
    }

    #[test]
    fn test_solve_1_test_2() {
        assert_eq!(part_1("(3x3)XYZ"), Ok(9));
    }

    #[test]
    fn test_solve_1_test_3() {
        assert_eq!(part_1("A(2x2)BCD(2x2)EFG"), Ok(11));
    }

    #[test]
    fn test_solve_1_test_4() {
        assert_eq!(part_1("(6x1)(1x3)A"), Ok(6));
    }

    #[test]
    fn test_solve_1_test_5() {
        assert_eq!(part_1("X(8x2)(3x3)ABCY"), Ok(18));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(183269));
    }

    #[test]
    fn test_solve_2_test_0() {
        assert_eq!(part_2("(3x3)XYZ"), Ok(9));
    }

    #[test]
    fn test_solve_2_test_1() {
        assert_eq!(part_2("X(8x2)(3x3)ABCY"), Ok(20));
    }

    #[test]
    fn test_solve_2_test_2() {
        assert_eq!(part_2("(27x12)(20x12)(13x14)(7x10)(1x12)A"), Ok(241920));
    }

    #[test]
    fn test_solve_2_test_3() {
        assert_eq!(
            part_2("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"),
            Ok(445)
        );
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(11317278863));
    }
}
