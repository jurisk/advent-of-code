use crate::Character::{ClosingBracket, OpeningBracket};
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

#[repr(u8)]
#[derive(FromPrimitive, Copy, Clone, Eq, PartialEq)]
enum OpeningBracketType {
    Round = b'(',
    Square = b'[',
    Curvy = b'{',
    Angle = b'<',
}

impl OpeningBracketType {
    fn matching_closing_bracket(self) -> ClosingBracketType {
        match self {
            OpeningBracketType::Round => ClosingBracketType::Round,
            OpeningBracketType::Square => ClosingBracketType::Square,
            OpeningBracketType::Curvy => ClosingBracketType::Curvy,
            OpeningBracketType::Angle => ClosingBracketType::Angle,
        }
    }

    fn completion_score(self) -> u64 {
        match self {
            OpeningBracketType::Round => 1,
            OpeningBracketType::Square => 2,
            OpeningBracketType::Curvy => 3,
            OpeningBracketType::Angle => 4,
        }
    }
}

#[repr(u8)]
#[derive(FromPrimitive, Copy, Clone, Eq, PartialEq)]
enum ClosingBracketType {
    Round = b')',
    Square = b']',
    Curvy = b'}',
    Angle = b'>',
}

impl ClosingBracketType {
    fn error_score(self) -> u32 {
        match self {
            ClosingBracketType::Round => 3,
            ClosingBracketType::Square => 57,
            ClosingBracketType::Curvy => 1197,
            ClosingBracketType::Angle => 25137,
        }
    }
}

enum Character {
    ClosingBracket(ClosingBracketType),
    OpeningBracket(OpeningBracketType),
}

impl Character {
    fn parse(ch: char) -> Option<Character> {
        let opening = FromPrimitive::from_u8(ch as u8).map(OpeningBracket);
        let closing = FromPrimitive::from_u8(ch as u8).map(ClosingBracket);
        opening.or(closing)
    }
}

#[derive(Clone, Copy)]
enum ParsingResult {
    Valid,
    Incomplete { incomplete_score: u64 },
    Corrupted { corrupted_score: u32 },
}

impl ParsingResult {
    fn error_score(self) -> u32 {
        match self {
            ParsingResult::Incomplete { .. } | ParsingResult::Valid => 0,
            ParsingResult::Corrupted { corrupted_score } => corrupted_score,
        }
    }

    fn completion_score(self) -> u64 {
        match self {
            ParsingResult::Incomplete { incomplete_score } => incomplete_score,
            ParsingResult::Corrupted { .. } | ParsingResult::Valid => 0,
        }
    }
}

fn parse_line(x: &str) -> ParsingResult {
    let characters: Vec<Character> = x
        .chars()
        .map(|x| Character::parse(x).unwrap_or_else(|| panic!("Unrecognized character {x}")))
        .collect();

    let mut stack: Vec<OpeningBracketType> = Vec::new();
    for x in characters {
        match &x {
            ClosingBracket(closing_bracket) => match stack.pop() {
                None => {
                    return ParsingResult::Corrupted {
                        corrupted_score: closing_bracket.error_score(),
                    }
                },
                Some(opening_bracket) => {
                    if opening_bracket.matching_closing_bracket() != *closing_bracket {
                        return ParsingResult::Corrupted {
                            corrupted_score: closing_bracket.error_score(),
                        };
                    }
                },
            },
            OpeningBracket(opening_bracket) => stack.push(*opening_bracket),
        }
    }

    if stack.is_empty() {
        ParsingResult::Valid
    } else {
        let incomplete_score: u64 = stack
            .iter()
            .rev()
            .fold(0, |acc, bracket| acc * 5 + bracket.completion_score());
        ParsingResult::Incomplete { incomplete_score }
    }
}

fn parse_data(data: &str) -> Vec<ParsingResult> {
    data.lines().map(parse_line).collect()
}

fn part_1(data: &[ParsingResult]) -> u32 {
    data.iter().map(|x| x.error_score()).sum()
}

fn part_2(data: &[ParsingResult]) -> u64 {
    let mut completion_scores: Vec<u64> = data
        .iter()
        .filter(|x| match x {
            ParsingResult::Incomplete { .. } => true,
            ParsingResult::Corrupted { .. } | ParsingResult::Valid => false,
        })
        .map(|x| x.completion_score())
        .collect();

    completion_scores.sort_unstable();

    completion_scores[completion_scores.len() / 2]
}

#[allow(clippy::unreadable_literal)]
fn main() {
    let test_data = parse_data(
        "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]",
    );
    let test_result_1 = part_1(&test_data);
    assert_eq!(test_result_1, 26397);

    let data = parse_data(include_str!("../../resources/10.txt"));
    let result_1 = part_1(&data);
    println!("Part 1: {result_1}");
    assert_eq!(result_1, 388713);

    let test_result_2 = part_2(&test_data);
    assert_eq!(test_result_2, 288957);

    let result_2 = part_2(&data);
    println!("Part 2: {result_2}");
    assert_eq!(result_2, 3539961434);
}
