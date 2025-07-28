use std::str::FromStr;

use advent_of_code_common::parsing::{Error, parse_lines_to_vec, parse_str};

use crate::Instruction::{Hlf, Inc, Jie, Jio, Jmp, Tpl};
use crate::Register::{A, B};

#[derive(Copy, Clone, Debug)]
enum Register {
    A,
    B,
}

impl FromStr for Register {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.chars().next().unwrap() {
            'a' => Ok(A),
            'b' => Ok(B),
            _ => Err(format!("Unrecognized register {s}")),
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum Instruction {
    Hlf(Register),
    Tpl(Register),
    Inc(Register),
    Jmp(i32),
    Jie(Register, i32),
    Jio(Register, i32),
}

impl FromStr for Instruction {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let v: Vec<_> = s.split_whitespace().collect();
        if v.len() < 2 {
            Err(format!("Too short {s}"))
        } else {
            match v[0] {
                "hlf" => Ok(Hlf(v[1].parse()?)),
                "tpl" => Ok(Tpl(v[1].parse()?)),
                "inc" => Ok(Inc(v[1].parse()?)),
                "jmp" => Ok(Jmp(parse_str(v[1])?)),
                "jie" => Ok(Jie(v[1].parse()?, parse_str(v[2])?)),
                "jio" => Ok(Jio(v[1].parse()?, parse_str(v[2])?)),
                _ => Err(format!("Unrecognized {s}")),
            }
        }
    }
}

#[derive(Copy, Clone, Debug)]
struct State {
    ip: i32,
    a:  u32,
    b:  u32,
}

impl State {
    fn new() -> Self {
        State {
            ip: 0,
            a:  0,
            b:  0,
        }
    }

    fn updated<F>(self, register: Register, f: F) -> Self
    where
        F: Fn(u32) -> u32,
    {
        State {
            ip: self.ip + 1,
            a:  match register {
                A => f(self.a),
                B => self.a,
            },
            b:  match register {
                A => self.b,
                B => f(self.b),
            },
        }
    }

    fn jump(self, offset: i32) -> Self {
        State {
            ip: self.ip + offset,
            ..self
        }
    }

    fn register(self, register: Register) -> u32 {
        match register {
            A => self.a,
            B => self.b,
        }
    }

    fn execute(self, instruction: Instruction) -> State {
        match instruction {
            Hlf(register) => self.updated(register, |n| n / 2),
            Tpl(register) => self.updated(register, |n| n * 3),
            Inc(register) => self.updated(register, |n| n + 1),
            Jmp(offset) => self.jump(offset),
            Jie(register, offset) => {
                self.jump(
                    if self.register(register).is_multiple_of(2) {
                        offset
                    } else {
                        1
                    },
                )
            },
            Jio(register, offset) => {
                self.jump(
                    if self.register(register) == 1 {
                        offset
                    } else {
                        1
                    },
                )
            },
        }
    }
}

fn parse(input: &str) -> Result<Vec<Instruction>, Error> {
    parse_lines_to_vec(input)
}

fn execute(instructions: &[Instruction], start: State) -> State {
    let mut state = start;
    loop {
        if let Some(&instruction) = usize::try_from(state.ip)
            .ok()
            .and_then(|idx| instructions.get(idx))
        {
            println!("{state:?} {instruction:?}");
            state = state.execute(instruction);
        } else {
            return state;
        }
    }
}

fn solve_1(instructions: &[Instruction]) -> u32 {
    execute(instructions, State::new()).b
}

fn solve_2(instructions: &[Instruction]) -> u32 {
    let state = State {
        a: 1,
        ..State::new()
    };

    execute(instructions, state).b
}

fn part_1(input: &str) -> Result<u32, Error> {
    let data = parse(input)?;
    Ok(solve_1(&data))
}

fn part_2(input: &str) -> Result<u32, Error> {
    let data = parse(input)?;
    Ok(solve_2(&data))
}

const DATA: &str = include_str!("../../resources/23.txt");

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

    #[test]
    fn test_execute() {
        const TEST: &str = "inc a
jio a, +2
tpl a
inc a";
        let instructions = parse(TEST).unwrap();
        let result = execute(&instructions, State::new());
        assert_eq!(result.a, 2);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(170));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(247));
    }
}
