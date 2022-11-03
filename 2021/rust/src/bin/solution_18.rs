use advent_of_code::parsing::{parse_lines_to_nonempty, Error};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::digit1;
use nom::combinator::map_res;
use nom::{Finish, IResult};
use nonempty::NonEmpty;
use std::fmt::{Debug, Formatter};
use std::str::FromStr;

const DATA: &str = include_str!("../../resources/18.txt");

type Number = u32;

#[derive(Eq, PartialEq, Clone)]
enum SnailfishNumber {
    Literal {
        value: Number,
    },
    Pair {
        left: Box<SnailfishNumber>,
        right: Box<SnailfishNumber>,
    },
}

impl SnailfishNumber {
    fn literal(value: Number) -> SnailfishNumber {
        SnailfishNumber::Literal { value }
    }

    fn pair(left: &SnailfishNumber, right: &SnailfishNumber) -> SnailfishNumber {
        SnailfishNumber::Pair {
            left: Box::new(left.clone()),
            right: Box::new(right.clone()),
        }
    }
}

impl Debug for SnailfishNumber {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SnailfishNumber::Literal { value } => write!(f, "{}", value),
            SnailfishNumber::Pair { left, right } => write!(f, "[{:?},{:?}]", **left, **right),
        }
    }
}

fn literal_snailfish_number(input: &str) -> IResult<&str, SnailfishNumber> {
    map_res(digit1, |s| str::parse(s).map(SnailfishNumber::literal))(input)
}

fn pair_snailfish_number(input: &str) -> IResult<&str, SnailfishNumber> {
    let (input, _) = tag("[")(input)?;
    let (input, left) = snailfish_number(input)?;
    let (input, _) = tag(",")(input)?;
    let (input, right) = snailfish_number(input)?;
    let (input, _) = tag("]")(input)?;
    Ok((input, SnailfishNumber::pair(&left, &right)))
}

fn snailfish_number(input: &str) -> IResult<&str, SnailfishNumber> {
    alt((literal_snailfish_number, pair_snailfish_number))(input)
}

impl FromStr for SnailfishNumber {
    type Err = Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let (_, result) =
            Finish::finish(snailfish_number(input)).map_err(|err| format!("{:?}", err.code))?;
        Ok(result)
    }
}

// apply carry to leftmost leaf
fn try_apply_right_carry(
    tree: &SnailfishNumber,
    carry: Option<Number>,
) -> (SnailfishNumber, Option<Number>) {
    match carry {
        None => (tree.clone(), None),
        Some(carry) => match tree {
            SnailfishNumber::Literal { value } => (SnailfishNumber::literal(value + carry), None),
            SnailfishNumber::Pair { left, right } => {
                let (new_left, new_carry) = try_apply_right_carry(left, Some(carry));
                match new_carry {
                    None => (SnailfishNumber::pair(&new_left, right), None),
                    Some(new_carry) => {
                        let (new_right, new_carry) = try_apply_right_carry(right, Some(new_carry));
                        (SnailfishNumber::pair(&new_left, &new_right), new_carry)
                    },
                }
            },
        },
    }
}

// apply carry to rightmost leaf
fn try_apply_left_carry(
    tree: &SnailfishNumber,
    carry: Option<Number>,
) -> (SnailfishNumber, Option<Number>) {
    match carry {
        None => (tree.clone(), None),
        Some(carry) => match tree {
            SnailfishNumber::Literal { value } => (SnailfishNumber::literal(value + carry), None),
            SnailfishNumber::Pair { left, right } => {
                let (new_right, new_carry) = try_apply_left_carry(right, Some(carry));
                match new_carry {
                    None => (SnailfishNumber::pair(left, &new_right), None),
                    Some(new_carry) => {
                        let (new_left, new_carry) = try_apply_left_carry(left, Some(new_carry));
                        (SnailfishNumber::pair(&new_left, &new_right), new_carry)
                    },
                }
            },
        },
    }
}

impl SnailfishNumber {
    fn add_with_reduction(&self, other: &SnailfishNumber) -> SnailfishNumber {
        let added = self.add_without_reduction(other);
        added.reduce_fully()
    }

    // If any pair is nested inside four pairs, the leftmost such pair explodes.
    //
    // To explode a pair, the pair's left value is added to the first regular number to the left of
    // the exploding pair (if any), and the pair's right value is added to the first regular number
    // to the right of the exploding pair (if any). Exploding pairs will always consist of two
    // regular numbers. Then, the entire exploding pair is replaced with the regular number 0.
    //
    // Returns Some if "explode" succeeded, None if failed
    #[allow(clippy::collapsible_else_if)]
    fn attempt_explode_internal(
        &self,
        depth_at: usize,
    ) -> Option<(Option<Number>, SnailfishNumber, Option<Number>)> {
        match self {
            SnailfishNumber::Literal { .. } => None,
            SnailfishNumber::Pair { left, right } => {
                if depth_at > 4 {
                    panic!("Did not expect to go so deep!");
                } else if depth_at == 4 {
                    let left = match **left {
                        SnailfishNumber::Literal { value } => value,
                        SnailfishNumber::Pair { .. } => panic!("Expected literal"),
                    };
                    let right = match **right {
                        SnailfishNumber::Literal { value } => value,
                        SnailfishNumber::Pair { .. } => panic!("Expected literal"),
                    };

                    Some((Some(left), SnailfishNumber::literal(0), Some(right)))
                } else {
                    if let Some((left_carry, new_left, right_carry)) =
                        left.attempt_explode_internal(depth_at + 1)
                    {
                        let (new_right, new_right_carry) =
                            try_apply_right_carry(right, right_carry);
                        Some((
                            left_carry,
                            SnailfishNumber::pair(&new_left, &new_right),
                            new_right_carry,
                        ))
                    } else if let Some((left_carry, new_right, right_carry)) =
                        right.attempt_explode_internal(depth_at + 1)
                    {
                        let (new_left, new_left_carry) = try_apply_left_carry(left, left_carry);
                        Some((
                            new_left_carry,
                            SnailfishNumber::pair(&new_left, &new_right),
                            right_carry,
                        ))
                    } else {
                        None
                    }
                }
            },
        }
    }

    fn attempt_explode_step(&self) -> Option<SnailfishNumber> {
        self.attempt_explode_internal(0)
            .map(|(_, result, _)| result)
    }

    // If any regular number is 10 or greater, the leftmost such regular number splits.
    //
    // To split a regular number, replace it with a pair; the left element of the pair should be the
    // regular number divided by two and rounded down, while the right element of the pair should be
    // the regular number divided by two and rounded up.
    //
    // Returns Some if "split" succeeded, None if failed
    #[allow(clippy::manual_map)]
    #[allow(clippy::cast_sign_loss)]
    fn attempt_split_step(&self) -> Option<SnailfishNumber> {
        match self {
            SnailfishNumber::Literal { value } => {
                if *value >= 10 {
                    Some(SnailfishNumber::pair(
                        &SnailfishNumber::literal((*value as f32 / 2.0).floor() as Number),
                        &SnailfishNumber::literal((*value as f32 / 2.0).ceil() as Number),
                    ))
                } else {
                    None
                }
            },
            SnailfishNumber::Pair { left, right } => {
                if let Some(new_left) = left.attempt_split_step() {
                    Some(SnailfishNumber::pair(&new_left, right))
                } else if let Some(new_right) = right.attempt_split_step() {
                    Some(SnailfishNumber::pair(left, &new_right))
                } else {
                    None
                }
            },
        }
    }

    // Returns Some if succeeded, None if failed
    fn attempt_reduce_step(&self) -> Option<SnailfishNumber> {
        self.attempt_explode_step()
            .or_else(|| self.attempt_split_step())
    }

    fn reduce_fully(&self) -> SnailfishNumber {
        if let Some(result) = self.attempt_reduce_step() {
            result.reduce_fully()
        } else {
            self.clone()
        }
    }

    fn add_without_reduction(&self, other: &SnailfishNumber) -> SnailfishNumber {
        SnailfishNumber::pair(self, other)
    }

    fn magnitude(&self) -> Number {
        match self {
            SnailfishNumber::Literal { value } => *value,
            SnailfishNumber::Pair { left, right } => 3 * left.magnitude() + 2 * right.magnitude(),
        }
    }
}

struct SnailfishNumberList {
    numbers: NonEmpty<SnailfishNumber>,
}

impl SnailfishNumberList {
    fn add_numbers(&self) -> SnailfishNumber {
        self.numbers
            .tail
            .iter()
            .fold(self.numbers.head.clone(), |acc, n| {
                acc.add_with_reduction(n)
            })
    }
}

impl FromStr for SnailfishNumberList {
    type Err = Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let numbers = parse_lines_to_nonempty(input)?;
        Ok(SnailfishNumberList { numbers })
    }
}

fn solve_1(input: &str) -> Result<Number, Error> {
    let list: SnailfishNumberList = input.parse()?;
    let sum: SnailfishNumber = list.add_numbers();
    Ok(sum.magnitude())
}

fn solve_2(input: &str) -> Result<Number, Error> {
    let list: SnailfishNumberList = input.parse()?;

    let mut potential: Vec<Number> = vec![];
    for i in 0..list.numbers.len() {
        for j in 0..list.numbers.len() {
            if i != j {
                let a = list.numbers[i].clone();
                let b = list.numbers[j].clone();
                potential.push(a.add_with_reduction(&b).magnitude());
                potential.push(b.add_with_reduction(&a).magnitude());
            }
        }
    }
    potential
        .into_iter()
        .max()
        .ok_or_else(|| "Empty".to_string())
}

fn main() {
    let result_1 = solve_1(DATA);
    println!("Part 1: {:?}", result_1);

    let result_2 = solve_2(DATA);
    println!("Part 2: {:?}", result_2);
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_explode(input: &str, expected: &str) {
        let a: SnailfishNumber = input.parse().unwrap();
        let b: SnailfishNumber = expected.parse().unwrap();
        assert_eq!(a.attempt_explode_step(), Some(b));
    }

    #[test]
    fn test_explode_1() {
        // the 9 has no regular number to its left, so it is not added to any regular number
        test_explode("[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]");
    }

    #[test]
    fn test_explode_2() {
        // the 2 has no regular number to its right, and so it is not added to any regular number
        test_explode("[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]");
    }

    #[test]
    fn test_explode_3() {
        test_explode("[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]");
    }
    #[test]
    fn test_explode_4() {
        // the pair [3,2] is unaffected because the pair [7,3] is further to the left; [3,2] would explode on the next action
        test_explode(
            "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]",
            "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]",
        );
    }

    #[test]
    fn test_explode_5() {
        test_explode(
            "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]",
            "[[3,[2,[8,0]]],[9,[5,[7,0]]]]",
        );
    }

    #[test]
    fn test_add() {
        let a: SnailfishNumber = "[[[[4,3],4],4],[7,[[8,4],9]]]".parse().unwrap();
        let b: SnailfishNumber = "[1,1]".parse().unwrap();
        let expected = "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]".parse().unwrap();
        assert_eq!(a.add_with_reduction(&b), expected);
    }

    fn test_addition(list: &str, expected: &str) {
        let list: SnailfishNumberList = list.parse().unwrap();
        let expected: SnailfishNumber = expected.parse().unwrap();
        assert_eq!(list.add_numbers(), expected);
    }

    #[test]
    fn test_solve_1_addition_test_1() {
        test_addition(
            "[1,1]
[2,2]
[3,3]
[4,4]",
            "[[[[1,1],[2,2]],[3,3]],[4,4]]",
        );
    }

    #[test]
    fn test_solve_1_addition_test_2() {
        test_addition(
            "[1,1]
[2,2]
[3,3]
[4,4]
[5,5]",
            "[[[[3,0],[5,3]],[4,4]],[5,5]]",
        );
    }

    #[test]
    fn test_solve_1_addition_test_3() {
        test_addition(
            "[1,1]
[2,2]
[3,3]
[4,4]
[5,5]
[6,6]",
            "[[[[5,0],[7,4]],[5,5]],[6,6]]",
        );
    }

    #[test]
    fn test_solve_1_addition_test_4() {
        test_addition(
            "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]",
            "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]",
        );
    }

    fn test_magnitude(input: &str, expected: Number) {
        let number: SnailfishNumber = input.parse().unwrap();
        assert_eq!(number.magnitude(), expected);
    }

    #[test]
    fn test_magnitudes() {
        test_magnitude("[[1,2],[[3,4],5]]", 143);
        test_magnitude("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]", 1384);
        test_magnitude("[[[[1,1],[2,2]],[3,3]],[4,4]]", 445);
        test_magnitude("[[[[3,0],[5,3]],[4,4]],[5,5]]", 791);
        test_magnitude("[[[[5,0],[7,4]],[5,5]],[6,6]]", 1137);
        test_magnitude(
            "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]",
            3488,
        );
    }

    const TEST: &str = include_str!("../../resources/18-test.txt");

    #[test]
    fn test_solve_test() {
        let final_sum = "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]";
        test_addition(TEST, final_sum);
        test_magnitude(final_sum, 4140);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(DATA), Ok(4124));
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve_2(TEST), Ok(3993));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(DATA), Ok(4673));
    }
}
