use std::collections::{BTreeMap, VecDeque};
use std::str::FromStr;

use advent_of_code_common::parsing::{Error, parse_lines_to_vec, parse_str};
use itertools::Either;

use crate::assembunny::Instruction::{Cpy, DecRegister, IncRegister, JumpIfNotZero, Out, Tgl};

pub type N = i32;

type RegisterId = char;

#[derive(Debug, Copy, Clone)]
pub enum RegisterOrValue {
    Register(RegisterId),
    Value(N),
}

#[derive(Debug, Copy, Clone)]
pub enum Instruction {
    Cpy {
        from: RegisterOrValue,
        to:   RegisterOrValue,
    },
    IncRegister {
        register: RegisterId,
    },
    DecRegister {
        register: RegisterId,
    },
    JumpIfNotZero {
        check:  RegisterOrValue,
        offset: RegisterOrValue,
    },
    Tgl {
        offset: RegisterId,
    },
    Out {
        register: RegisterId,
    },
}

#[derive(Debug)]
pub struct State {
    pub instructions:        Vec<Instruction>,
    pub instruction_pointer: usize,
    pub registers:           BTreeMap<RegisterId, N>,
    pub output:              VecDeque<N>,
}

impl State {
    #[must_use]
    pub fn new(instructions: &[Instruction], initial_registers: &[(RegisterId, N)]) -> Self {
        Self {
            instructions:        Vec::from(instructions),
            instruction_pointer: 0,
            registers:           BTreeMap::from_iter(initial_registers.to_vec()),
            output:              VecDeque::new(),
        }
    }

    fn modify_register<F>(&mut self, register: RegisterId, f: F)
    where
        F: Fn(N) -> N,
    {
        let entry = self.registers.entry(register).or_insert(0);
        *entry = f(*entry);
    }

    fn read_register(&self, register: RegisterId) -> N {
        *self.registers.get(&register).unwrap_or(&0)
    }

    fn read_register_or_value(&self, register_or_value: RegisterOrValue) -> N {
        match register_or_value {
            RegisterOrValue::Register(register) => self.read_register(register),
            RegisterOrValue::Value(value) => value,
        }
    }

    fn increment_ip(&mut self) {
        self.modify_ip(1);
    }

    fn next_instruction(&self) -> Option<&Instruction> {
        self.instructions.get(self.instruction_pointer)
    }

    #[must_use]
    fn ip_plus_offset(&self, offset: N) -> usize {
        usize::try_from(
            (isize::try_from(self.instruction_pointer).unwrap()) + isize::try_from(offset).unwrap(),
        )
        .unwrap()
    }

    fn modify_ip(&mut self, offset: N) {
        self.instruction_pointer = self.ip_plus_offset(offset);
    }

    fn execute_instruction(&mut self, instruction: Instruction) {
        match instruction {
            Cpy { from, to } => {
                match to {
                    RegisterOrValue::Register(register) => {
                        let value = self.read_register_or_value(from);
                        self.modify_register(register, |_| value);
                    },
                    RegisterOrValue::Value(_) => {
                        // Ignore such useless instructions
                    },
                }

                self.increment_ip();
            },
            IncRegister { register } => {
                self.modify_register(register, |x| x + 1);
                self.increment_ip();
            },
            DecRegister { register } => {
                self.modify_register(register, |x| x - 1);
                self.increment_ip();
            },
            JumpIfNotZero { check, offset } => {
                if self.read_register_or_value(check) == 0 {
                    self.increment_ip();
                } else {
                    let offset = self.read_register_or_value(offset);
                    self.modify_ip(offset);
                }
            },
            Tgl { offset } => {
                let offset = self.read_register(offset);
                let offset = self.ip_plus_offset(offset);

                if let Some(&existing) = self.instructions.get(offset) {
                    let updated = match existing {
                        Cpy { from, to } => {
                            JumpIfNotZero {
                                check:  from,
                                offset: to,
                            }
                        },
                        IncRegister { register } => DecRegister { register },
                        DecRegister { register } | Out { register } => IncRegister { register },
                        JumpIfNotZero { check, offset } => {
                            Cpy {
                                from: check,
                                to:   offset,
                            }
                        },
                        Tgl { offset } => IncRegister { register: offset },
                    };
                    self.instructions[offset] = updated;
                }

                self.increment_ip();
            },
            Out { register } => {
                let value = self.read_register(register);
                self.output.push_back(value);
                self.increment_ip();
            },
        }
    }

    #[must_use]
    pub fn run_to_termination(mut self) -> State {
        while let Some(instruction) = self.next_instruction() {
            self.execute_instruction(*instruction);
        }

        self
    }

    #[must_use]
    // Left - matched predicate, Right - terminated normally
    pub fn run_until_matches_predicate<F>(mut self, predicate: F) -> Either<Self, Self>
    where
        F: Fn(&Self) -> bool,
    {
        loop {
            if predicate(&self) {
                return Either::Left(self);
            }

            if let Some(instruction) = self.next_instruction() {
                self.execute_instruction(*instruction);
            } else {
                return Either::Right(self);
            }
        }
    }
}

impl FromStr for Instruction {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        fn parse_register_or_value(x: &str) -> Result<RegisterOrValue, Error> {
            let value: Result<N, Error> = parse_str(x);
            match value {
                Ok(x) => Ok(RegisterOrValue::Value(x)),
                Err(_) => parse_register(x).map(RegisterOrValue::Register),
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
                let to = RegisterOrValue::Register(to);

                Ok(Cpy { from, to })
            },
            "inc" => {
                let register = parse_register(x)?;
                Ok(IncRegister { register })
            },
            "dec" => {
                let register = parse_register(x)?;
                Ok(DecRegister { register })
            },
            "jnz" => {
                let y: &str = &nth_elem(&v, 2)?;
                let check = parse_register_or_value(x)?;
                let offset = parse_register_or_value(y)?;

                Ok(JumpIfNotZero { check, offset })
            },
            "tgl" => {
                let offset = parse_register(x)?;
                Ok(Tgl { offset })
            },
            "out" => {
                let register = parse_register(x)?;
                Ok(Out { register })
            },
            _ => Err(format!("Unrecognized {op} in {s}")),
        }
    }
}

#[expect(clippy::missing_errors_doc)]
pub fn parse_instructions(input: &str) -> Result<Vec<Instruction>, Error> {
    parse_lines_to_vec(input)
}
