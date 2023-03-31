use std::collections::BTreeMap;
use std::str::FromStr;

use advent_of_code_common::parsing::{parse_lines_to_vec, parse_str, Error};
use itertools::Either;

use crate::Instruction::{CpyRegister, CpyValue, DecRegister, IncRegister, Jump, JumpIfNotZero};

type N = i32;

type RegisterId = char;

#[derive(Debug, Copy, Clone)]
enum Instruction {
    CpyValue {
        value: N,
        to:    RegisterId,
    },
    CpyRegister {
        from: RegisterId,
        to:   RegisterId,
    },
    IncRegister(RegisterId),
    DecRegister(RegisterId),
    Jump {
        offset: N,
    },
    JumpIfNotZero {
        check_register: RegisterId,
        offset:         N,
    },
}

#[derive(Debug, Clone)]
struct State {
    instruction_pointer: usize,
    registers:           BTreeMap<RegisterId, N>,
}

impl State {
    fn modify_register<F>(&self, register: RegisterId, f: F) -> Self
    where
        F: Fn(N) -> N,
    {
        let current = self.registers.get(&register).unwrap_or(&0);
        let updated = f(*current);
        let mut registers = self.registers.clone();
        registers.insert(register, updated);
        Self {
            registers,
            instruction_pointer: self.instruction_pointer,
        }
    }

    fn read_register(&self, register: RegisterId) -> N {
        *self.registers.get(&register).unwrap_or(&0)
    }

    fn increment_ip(&self) -> Self {
        Self {
            instruction_pointer: self.instruction_pointer + 1,
            registers:           self.registers.clone(),
        }
    }

    fn execute_instruction(&self, instruction: Instruction) -> Self {
        match instruction {
            CpyValue { value, to } => self.modify_register(to, |_| value).increment_ip(),
            CpyRegister { from, to } => {
                self.modify_register(to, |_| self.read_register(from))
                    .increment_ip()
            },
            IncRegister(register_id) => self.modify_register(register_id, |x| x + 1).increment_ip(),
            DecRegister(register_id) => self.modify_register(register_id, |x| x - 1).increment_ip(),
            Jump { offset } => {
                let instruction_pointer = usize::try_from(
                    (isize::try_from(self.instruction_pointer).unwrap())
                        + isize::try_from(offset).unwrap(),
                )
                .unwrap();
                Self {
                    instruction_pointer,
                    registers: self.registers.clone(),
                }
            },
            JumpIfNotZero {
                check_register,
                offset,
            } => {
                if self.read_register(check_register) == 0 {
                    self.increment_ip()
                } else {
                    self.execute_instruction(Jump { offset })
                }
            },
        }
    }
}

impl FromStr for Instruction {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        fn parse_register_or_value(x: &str) -> Result<Either<RegisterId, N>, Error> {
            let value: Result<N, Error> = parse_str(x);
            match value {
                Ok(x) => Ok(Either::Right(x)),
                Err(_) => parse_register(x).map(Either::Left),
            }
        }

        fn parse_register(x: &str) -> Result<RegisterId, Error> {
            if x.len() == 1 {
                let ch = x.chars().next().unwrap();
                if matches!(ch, 'a' | 'b' | 'c' | 'd') {
                    Ok(ch)
                } else {
                    Err(format!("Unrecognized register {x}"))
                }
            } else {
                Err(format!("Unrecognized register {x}"))
            }
        }

        fn nth_elem(v: &[&str], n: usize) -> Result<String, Error> {
            match v.get(n) {
                None => Err(format!("Failed to extract {n}-th element from {v:?}")),
                Some(s) => Ok((*s).to_string()),
            }
        }

        let v: Vec<&str> = s.split(' ').collect();
        let op: &str = &nth_elem(&v, 0)?;
        let x: &str = &nth_elem(&v, 1)?;
        match op {
            "cpy" => {
                let y: &str = &nth_elem(&v, 2)?;
                let from = parse_register_or_value(x)?;
                let to = parse_register(y)?;

                match from {
                    Either::Left(from) => Ok(CpyRegister { from, to }),
                    Either::Right(value) => Ok(CpyValue { value, to }),
                }
            },
            "inc" => {
                let register_id = parse_register(x)?;
                Ok(IncRegister(register_id))
            },
            "dec" => {
                let register_id = parse_register(x)?;
                Ok(DecRegister(register_id))
            },
            "jnz" => {
                let y: &str = &nth_elem(&v, 2)?;
                let offset = parse_str(y)?;
                if x == "1" {
                    Ok(Jump { offset })
                } else {
                    let check_register = parse_register(x)?;
                    Ok(JumpIfNotZero {
                        check_register,
                        offset,
                    })
                }
            },
            _ => Err(format!("Unrecognized {op} in {s}")),
        }
    }
}

fn solve(instructions: &[Instruction], initial_registers: &[(RegisterId, N)]) -> State {
    let mut state = State {
        instruction_pointer: 0,
        registers:           BTreeMap::from_iter(initial_registers.to_vec()),
    };

    while let Some(instruction) = instructions.get(state.instruction_pointer) {
        state = state.execute_instruction(*instruction);
    }

    state
}

fn solve_1(instructions: &[Instruction]) -> N {
    solve(instructions, &[]).registers[&'a']
}

fn solve_2(instructions: &[Instruction]) -> N {
    solve(instructions, &[('c', 1)]).registers[&'a']
}

fn parse(input: &str) -> Result<Vec<Instruction>, Error> {
    parse_lines_to_vec(input)
}

fn part_1(input: &str) -> Result<N, Error> {
    parse(input).map(|instructions| solve_1(&instructions))
}

fn part_2(input: &str) -> Result<N, Error> {
    parse(input).map(|instructions| solve_2(&instructions))
}

const DATA: &str = include_str!("../../resources/12.txt");

fn main() -> Result<(), Error> {
    let result_1 = part_1(DATA)?;
    println!("Part 1: {result_1:?}");

    let result_2 = part_2(DATA);
    println!("Part 2: {result_2:?}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST: &str = include_str!("../../resources/12-test.txt");

    #[test]
    fn test_solve_1_test() {
        assert_eq!(part_1(TEST), Ok(42));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(317993));
    }

    #[test]
    #[ignore]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(9227647));
    }
}
