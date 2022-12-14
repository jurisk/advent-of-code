use crate::FoldInstruction::{AlongX, AlongY};
use advent_of_code_common::parsing::{
    parse_lines_to_hashset, parse_lines_to_vec,
    split_into_two_segments_separated_by_double_newline, split_into_two_strings, Error,
};
use advent_of_code_common::utils::head_tail;
use itertools::Itertools;
use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use std::str::FromStr;

const DATA: &str = include_str!("../../resources/13.txt");

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
struct CoordsXY {
    x: usize,
    y: usize,
}

impl CoordsXY {
    fn apply(&self, instruction: &FoldInstruction) -> CoordsXY {
        match instruction {
            AlongX(x_fold) if self.x < *x_fold => self.clone(),
            AlongX(x_fold) if self.x > *x_fold => CoordsXY {
                x: x_fold - (self.x - x_fold),
                y: self.y,
            },
            AlongY(y_fold) if self.y < *y_fold => self.clone(),
            AlongY(y_fold) if self.y > *y_fold => CoordsXY {
                x: self.x,
                y: y_fold - (self.y - y_fold),
            },
            _ => panic!("Unexpected {self:?} {instruction:?}"),
        }
    }
}

impl FromStr for CoordsXY {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (x_str, y_str) = split_into_two_strings(s, ",")?;
        let x = x_str.parse().map_err(|err| format!("{err}"))?;
        let y = y_str.parse().map_err(|err| format!("{err}"))?;

        Ok(CoordsXY { x, y })
    }
}

#[derive(Clone, Debug)]
enum FoldInstruction {
    AlongX(usize),
    AlongY(usize),
}

impl FromStr for FoldInstruction {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(along_x) = s.strip_prefix("fold along x=") {
            let x = along_x.parse().map_err(|err| format!("{err}"))?;
            Ok(AlongX(x))
        } else if let Some(along_y) = s.strip_prefix("fold along y=") {
            let y = along_y.parse().map_err(|err| format!("{err}"))?;
            Ok(AlongY(y))
        } else {
            Err(format!("Unrecognized {s}"))
        }
    }
}

#[derive(Clone)]
struct Manual {
    dots: HashSet<CoordsXY>,
    instructions: Vec<FoldInstruction>,
}

impl Manual {
    fn fold_dots(dots: &HashSet<CoordsXY>, instruction: &FoldInstruction) -> HashSet<CoordsXY> {
        dots.iter().map(|dot| dot.apply(instruction)).collect()
    }

    fn apply_next_instruction(&self) -> Manual {
        let (head, tail) = head_tail(&self.instructions);
        match head {
            None => self.clone(),
            Some(head) => Manual {
                dots: Manual::fold_dots(&self.dots, head),
                instructions: tail.to_vec(),
            },
        }
    }
}

impl Display for Manual {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let max_x = self.dots.iter().map(|c| c.x).max().unwrap();
        let max_y = self.dots.iter().map(|c| c.y).max().unwrap();
        let result = (0..=max_y)
            .map(|y| {
                (0..=max_x)
                    .map(|x| {
                        if self.dots.contains(&CoordsXY { x, y }) {
                            '#'
                        } else {
                            '·'
                        }
                    })
                    .join("")
            })
            .join("\n");

        f.write_str(&result)
    }
}

impl FromStr for Manual {
    type Err = Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let (a, b) = split_into_two_segments_separated_by_double_newline(input)?;
        let dots = parse_lines_to_hashset(&a)?;
        let instructions = parse_lines_to_vec(&b)?;
        Ok(Manual { dots, instructions })
    }
}

fn solve_1(input: &str) -> Result<usize, Error> {
    let mut manual: Manual = input.parse()?;
    manual = manual.apply_next_instruction();
    Ok(manual.dots.len())
}

fn solve_2(input: &str) -> Result<String, Error> {
    let mut manual: Manual = input.parse()?;
    while !manual.instructions.is_empty() {
        manual = manual.apply_next_instruction();
    }
    Ok(manual.to_string())
}

fn main() {
    let result_1 = solve_1(DATA);
    println!("Part 1: {result_1:?}");

    let result_2 = solve_2(DATA);
    println!("Part 2:\n{}", result_2.unwrap()); // RGZLBHFP
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = include_str!("../../resources/13-test.txt");

    #[test]
    fn test_fold_y() {
        let fold = AlongY(7);
        assert_eq!(
            CoordsXY { x: 4, y: 14 }.apply(&fold),
            CoordsXY { x: 4, y: 0 }
        );
        assert_eq!(
            CoordsXY { x: 3, y: 13 }.apply(&fold),
            CoordsXY { x: 3, y: 1 }
        );
    }

    #[test]
    fn test_fold_x() {
        let fold = AlongX(5);
        assert_eq!(
            CoordsXY { x: 6, y: 0 }.apply(&fold),
            CoordsXY { x: 4, y: 0 }
        );
        assert_eq!(
            CoordsXY { x: 9, y: 0 }.apply(&fold),
            CoordsXY { x: 1, y: 0 }
        );
    }

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(TEST_DATA), Ok(17));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(DATA), Ok(729));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(DATA).map(|x| x.len()), Ok(373));
    }
}
