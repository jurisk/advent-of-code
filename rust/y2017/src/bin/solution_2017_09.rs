use std::str;

use advent_of_code_common::parsing;
use nom::branch::alt;
use nom::bytes::complete::{is_not, tag};
use nom::character::complete::{anychar, char};
use nom::combinator::{complete, map};
use nom::multi::{many0, separated_list0};
use nom::sequence::delimited;
use nom::{Finish, IResult, Parser};

use crate::Thing::{Garbage, Group};

const DATA: &str = include_str!("../../resources/09.txt");

#[derive(Debug)]
enum Thing {
    Group { things: Vec<Thing> },
    Garbage { contents: String },
}

impl Thing {
    fn total_score(&self, current: usize) -> usize {
        match self {
            Group { things } => {
                let children_score: usize = things
                    .iter()
                    .map(|thing| thing.total_score(current + 1))
                    .sum();
                current + children_score
            },
            Garbage { .. } => 0,
        }
    }

    fn garbage_score(&self) -> usize {
        match self {
            Group { things } => things.iter().map(Thing::garbage_score).sum(),
            Garbage { contents } => contents.len(),
        }
    }
}

fn ignored(input: &str) -> IResult<&str, &str> {
    let (input, _) = char('!')(input)?;
    let (input, _) = anychar(input)?;
    Ok((input, ""))
}

fn garbage_char(input: &str) -> IResult<&str, &str> {
    is_not("!>")(input)
}

fn garbage_component(input: &str) -> IResult<&str, &str> {
    alt((ignored, garbage_char)).parse(input)
}

fn garbage(input: &str) -> IResult<&str, Thing> {
    map(
        delimited(tag("<"), many0(garbage_component), tag(">")),
        |vec| {
            Garbage {
                contents: vec.concat(),
            }
        },
    )
    .parse(input)
}

fn group(input: &str) -> IResult<&str, Thing> {
    map(
        delimited(tag("{"), separated_list0(tag(","), thing), tag("}")),
        |things| Group { things },
    )
    .parse(input)
}

fn thing(input: &str) -> IResult<&str, Thing> {
    alt((group, garbage)).parse(input)
}

fn parse(input: &str) -> Result<Thing, parsing::Error> {
    let parsed = complete(thing).parse(input);
    let (_, result) = Finish::finish(parsed).map_err(|err| format!("{err:?} {:?}", err.code))?;
    Ok(result)
}

fn part_1(input: &str) -> Result<usize, parsing::Error> {
    println!("{:?}", parse(input));
    parse(input).map(|thing| thing.total_score(1))
}

fn part_2(input: &str) -> Result<usize, parsing::Error> {
    parse(input).map(|thing| thing.garbage_score())
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

    // {}, score of 1
    #[test]
    fn test_solve_1_test_0() {
        assert_eq!(part_1("{}"), Ok(1));
    }

    // {{{}}}, score of 1 + 2 + 3 = 6.
    #[test]
    fn test_solve_1_test_1() {
        assert_eq!(part_1("{{{}}}"), Ok(6));
    }

    // {{},{}}, score of 1 + 2 + 2 = 5.
    #[test]
    fn test_solve_1_test_2() {
        assert_eq!(part_1("{{},{}}"), Ok(5));
    }

    // {{{},{},{{}}}}, score of 1 + 2 + 3 + 3 + 3 + 4 = 16.
    #[test]
    fn test_solve_1_test_3() {
        assert_eq!(part_1("{{{},{},{{}}}}"), Ok(16));
    }

    // {<a>,<a>,<a>,<a>}, score of 1.
    #[test]
    fn test_solve_1_test_4() {
        assert_eq!(part_1("{<a>,<a>,<a>,<a>}"), Ok(1));
    }

    // {{<ab>},{<ab>},{<ab>},{<ab>}}, score of 1 + 2 + 2 + 2 + 2 = 9.
    #[test]
    fn test_solve_1_test_5() {
        assert_eq!(part_1("{{<ab>},{<ab>},{<ab>},{<ab>}}"), Ok(9));
    }

    // {{<!!>},{<!!>},{<!!>},{<!!>}}, score of 1 + 2 + 2 + 2 + 2 = 9.
    #[test]
    fn test_solve_1_test_6() {
        assert_eq!(part_1("{{<!!>},{<!!>},{<!!>},{<!!>}}"), Ok(9));
    }

    // {{<a!>},{<a!>},{<a!>},{<ab>}}, score of 1 + 2 = 3.
    #[test]
    fn test_solve_1_test_7() {
        assert_eq!(part_1("{{<a!>},{<a!>},{<a!>},{<ab>}}"), Ok(3));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(20530));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(9978));
    }
}
