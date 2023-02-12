use std::collections::HashSet;
use std::fmt::{Display, Formatter};
use std::str::FromStr;

use advent_of_code_common::coords2d::Coords2D;
use advent_of_code_common::parsing::{
    parse_lines_to_hashset, parse_lines_to_vec,
    split_into_two_segments_separated_by_double_newline, Error,
};
use advent_of_code_common::utils::head_tail;
use itertools::Itertools;

use crate::FoldInstruction::{AlongX, AlongY};

const DATA: &str = include_str!("../../resources/13.txt");

type Coords = Coords2D<usize>;

#[derive(Clone, Copy, Debug)]
enum FoldInstruction {
    AlongX(usize),
    AlongY(usize),
}

impl FoldInstruction {
    fn apply_to_coords(self, coords: Coords) -> Coords {
        match self {
            AlongX(x_fold) if coords.x < x_fold => coords,
            AlongX(x_fold) if coords.x > x_fold => {
                Coords {
                    x: x_fold - (coords.x - x_fold),
                    y: coords.y,
                }
            },
            AlongY(y_fold) if coords.y < y_fold => coords,
            AlongY(y_fold) if coords.y > y_fold => {
                Coords {
                    x: coords.x,
                    y: y_fold - (coords.y - y_fold),
                }
            },
            _ => panic!("Unexpected {self:?} {self:?}"),
        }
    }
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
    dots:         HashSet<Coords>,
    instructions: Vec<FoldInstruction>,
}

impl Manual {
    fn fold_dots(dots: &HashSet<Coords>, instruction: &FoldInstruction) -> HashSet<Coords> {
        dots.iter()
            .map(|dot| instruction.apply_to_coords(*dot))
            .collect()
    }

    fn apply_next_instruction(&self) -> Manual {
        let (head, tail) = head_tail(&self.instructions);
        match head {
            None => self.clone(),
            Some(head) => {
                Manual {
                    dots:         Manual::fold_dots(&self.dots, head),
                    instructions: tail.to_vec(),
                }
            },
        }
    }
}

impl Display for Manual {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let max_x = self.dots.iter().map(|c| c.x).max().unwrap();
        let max_y = self.dots.iter().map(|c| c.y).max().unwrap();
        let result = (0 ..= max_y)
            .map(|y| {
                (0 ..= max_x)
                    .map(|x| {
                        if self.dots.contains(&Coords { x, y }) {
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
            fold.apply_to_coords(Coords { x: 4, y: 14 }),
            Coords { x: 4, y: 0 }
        );
        assert_eq!(
            fold.apply_to_coords(Coords { x: 3, y: 13 }),
            Coords { x: 3, y: 1 }
        );
    }

    #[test]
    fn test_fold_x() {
        let fold = AlongX(5);
        assert_eq!(
            fold.apply_to_coords(Coords { x: 6, y: 0 }),
            Coords { x: 4, y: 0 }
        );
        assert_eq!(
            fold.apply_to_coords(Coords { x: 9, y: 0 }),
            Coords { x: 1, y: 0 }
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
