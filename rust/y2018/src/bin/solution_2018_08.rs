use std::str::FromStr;

use advent_of_code_common::parsing::{Error, parse_separated_vec};
use advent_of_code_common::utils::head_tail;
use nom::Err::Incomplete;
use nom::combinator::complete;
use nom::multi::count;
use nom::{Finish, IResult, Needed, Parser};

const DATA: &str = include_str!("../../resources/08.txt");

#[derive(Debug)]
struct Node {
    children: Vec<Node>,
    metadata: Vec<usize>,
}

impl Node {
    fn recursive_sum(&self) -> usize {
        self.metadata.iter().sum::<usize>()
            + self.children.iter().map(Node::recursive_sum).sum::<usize>()
    }

    fn root_node(&self) -> usize {
        if self.children.is_empty() {
            self.recursive_sum()
        } else {
            self.metadata
                .iter()
                .map(|idx| self.children.get(idx - 1).map_or(0, Node::root_node))
                .sum()
        }
    }
}

/// Takes head of the list, or fails
fn take_head<T: Clone>(input: &[T]) -> IResult<&[T], T> {
    match head_tail(input) {
        None => Err(Incomplete(Needed::Unknown)),
        Some((h, t)) => Ok((t, h.clone())),
    }
}

fn node(input: &[usize]) -> IResult<&[usize], Node> {
    let (input, children_count) = take_head(input)?;
    let (input, metadata_count) = take_head(input)?;
    let (input, children) = count(node, children_count).parse(input)?;
    let (input, metadata) = count(take_head, metadata_count).parse(input)?;
    Ok((input, Node { children, metadata }))
}

impl FromStr for Node {
    type Err = Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let numbers: Vec<usize> = parse_separated_vec(input, " ")?;
        let parsed = complete(node).parse(&numbers);
        let (_, result) = Finish::finish(parsed).map_err(|err| format!("{:?}", err.code))?;
        Ok(result)
    }
}

type Output = usize;

fn solve_1(data: &Node) -> Output {
    data.recursive_sum()
}

fn solve_2(data: &Node) -> Output {
    data.root_node()
}

fn part_1(input: &str) -> Result<Output, Error> {
    let data = input.parse()?;
    Ok(solve_1(&data))
}

fn part_2(input: &str) -> Result<Output, Error> {
    let data = input.parse()?;
    Ok(solve_2(&data))
}

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

    const TEST_DATA: &str = include_str!("../../resources/08-test.txt");

    #[test]
    fn test_solve_1_test() {
        assert_eq!(part_1(TEST_DATA), Ok(138));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(45868));
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(part_2(TEST_DATA), Ok(66));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(19724));
    }
}
