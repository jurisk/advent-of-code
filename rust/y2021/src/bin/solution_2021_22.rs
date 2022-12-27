use advent_of_code_common::coords3d::Coords3D;
use advent_of_code_common::cuboid::Cuboid;
use advent_of_code_common::parsing::{parse_lines_to_vec, Error};
use memoize::lazy_static::lazy_static;
use regex::Regex;
use std::str::FromStr;

const DATA: &str = include_str!("../../resources/22.txt");

#[repr(u8)]
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Instruction {
    Off = 0,
    On = 1,
}

impl Instruction {
    fn reverse(self) -> Instruction {
        match self {
            Instruction::Off => Instruction::On,
            Instruction::On => Instruction::Off,
        }
    }
}

impl FromStr for Instruction {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "on" {
            Ok(Instruction::On)
        } else if s == "off" {
            Ok(Instruction::Off)
        } else {
            Err(format!("Unrecognized {s}"))
        }
    }
}

fn task_1_bounds() -> Cuboid<isize> {
    Cuboid {
        min: Coords3D::new(-50, -50, -50),
        max: Coords3D::new(50, 50, 50),
    }
}

fn valid_for_task_1(cuboid: &Cuboid<isize>) -> bool {
    cuboid.overlap(&task_1_bounds()).is_some()
}

#[derive(Copy, Clone, Debug)]
struct Step {
    instruction: Instruction,
    cuboid: Cuboid<isize>,
}

impl Step {
    fn size(&self) -> isize {
        self.cuboid.size()
            * (match self.instruction {
                Instruction::Off => -1,
                Instruction::On => 1,
            })
    }
}

impl FromStr for Step {
    type Err = Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        lazy_static! {
            static ref RE: Regex = Regex::new(
                r"(\w+)\sx=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)"
            )
            .unwrap();
        }

        let qq = RE.captures(input).unwrap();
        let groups = (
            qq.get(1),
            qq.get(2),
            qq.get(3),
            qq.get(4),
            qq.get(5),
            qq.get(6),
            qq.get(7),
        );

        match groups {
            (Some(m1), Some(m2), Some(m3), Some(m4), Some(m5), Some(m6), Some(m7)) => {
                let instruction: Instruction = m1.as_str().parse()?;
                let x1: isize = m2.as_str().parse().map_err(|err| format!("{err}"))?;
                let x2: isize = m3.as_str().parse().map_err(|err| format!("{err}"))?;
                let y1: isize = m4.as_str().parse().map_err(|err| format!("{err}"))?;
                let y2: isize = m5.as_str().parse().map_err(|err| format!("{err}"))?;
                let z1: isize = m6.as_str().parse().map_err(|err| format!("{err}"))?;
                let z2: isize = m7.as_str().parse().map_err(|err| format!("{err}"))?;

                let min = Coords3D::new(x1, y1, z1);
                let max = Coords3D::new(x2, y2, z2);

                let cuboid = Cuboid { min, max };

                Ok(Step {
                    instruction,
                    cuboid,
                })
            },
            _ => Err(format!("Did not match: {groups:?}")),
        }
    }
}

struct Steps {
    steps: Vec<Step>,
}

impl FromStr for Steps {
    type Err = Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let steps: Vec<Step> = parse_lines_to_vec(input)?;
        Ok(Steps { steps })
    }
}

#[derive(Debug)]
struct Cuboids {
    steps: Vec<Step>,
}

impl Cuboids {
    fn empty() -> Cuboids {
        Cuboids { steps: vec![] }
    }

    fn apply_step(&mut self, step: &Step) {
        let mut more: Vec<Step> = vec![];
        for existing in &self.steps {
            if let Some(overlap) = existing.cuboid.overlap(&step.cuboid) {
                more.push(Step {
                    instruction: existing.instruction.reverse(),
                    cuboid: overlap,
                });
            }
        }

        if step.instruction == Instruction::On {
            self.steps.push(*step);
        }
        for step in more {
            self.steps.push(step);
        }
    }

    fn cubes_on(&self) -> isize {
        self.steps.iter().map(Step::size).sum()
    }
}

impl Steps {
    fn filter_for_task_1(&self) -> Steps {
        Steps {
            steps: self
                .steps
                .iter()
                .filter(|s| valid_for_task_1(&s.cuboid))
                .copied()
                .collect(),
        }
    }

    fn cubes_on(&self) -> isize {
        let mut cuboids: Cuboids = Cuboids::empty();
        for step in &self.steps {
            // println!("Applying: {:?}", step);
            cuboids.apply_step(step);
            // println!("Cuboids now: {:?}", cuboids);
        }
        cuboids.cubes_on()
    }
}

fn solve_1(input: &str) -> Result<isize, Error> {
    let steps: Steps = input.parse()?;
    let filtered_steps = steps.filter_for_task_1();
    Ok(filtered_steps.cubes_on())
}

fn solve_2(input: &str) -> Result<isize, Error> {
    let steps: Steps = input.parse()?;
    Ok(steps.cubes_on())
}

fn main() {
    let result_1 = solve_1(DATA);
    println!("Part 1: {result_1:?}");

    let result_2 = solve_2(DATA);
    println!("Part 2: {result_2:?}");
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA_1: &str = include_str!("../../resources/22-test-1.txt");
    const TEST_DATA_2: &str = include_str!("../../resources/22-test-2.txt");
    const TEST_DATA_3: &str = include_str!("../../resources/22-test-3.txt");

    #[test]
    fn test_overlap_1() {
        let a = Cuboid {
            min: Coords3D::new(0, -1, 4),
            max: Coords3D::new(3, 17, 4),
        };

        let b = Cuboid {
            min: Coords3D::new(2, 2, 4),
            max: Coords3D::new(6, 5, 4),
        };

        let result = a.overlap(&b);

        let expected = Cuboid {
            min: Coords3D::new(2, 2, 4),
            max: Coords3D::new(3, 5, 4),
        };

        assert_eq!(result, Some(expected));
    }

    #[test]
    fn test_solve_1_test_0() {
        assert_eq!(solve_1("on x=10..12,y=10..12,z=10..12"), Ok(27));
    }

    #[test]
    fn test_solve_1_test_1() {
        assert_eq!(
            solve_1("on x=10..12,y=10..12,z=10..12\non x=11..13,y=11..13,z=11..13"),
            Ok(46)
        );
    }

    #[test]
    fn test_solve_1_test_2() {
        assert_eq!(solve_1("on x=10..12,y=10..12,z=10..12\non x=11..13,y=11..13,z=11..13\noff x=9..11,y=9..11,z=9..11"), Ok(38));
    }

    #[test]
    fn test_solve_1_test_3() {
        assert_eq!(solve_1(TEST_DATA_1), Ok(39));
    }

    #[test]
    fn test_solve_1_test_4() {
        assert_eq!(solve_1(TEST_DATA_2), Ok(590_784));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(DATA), Ok(588_120));
    }

    #[test]
    fn test_solve_2_test_1() {
        assert_eq!(solve_2(TEST_DATA_3), Ok(2_758_514_936_282_235));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(DATA), Ok(1_134_088_247_046_731));
    }
}
