use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use std::cell::Cell;
use std::collections::VecDeque;

pub type Entry = i32;
pub type MachineCode = Vec<Entry>;
pub type MachineCodeRef = [Entry];
pub type Input = VecDeque<Entry>;
pub type Output = VecDeque<Entry>;
type Index = usize;

#[derive(Eq, PartialEq, Debug, FromPrimitive)]
#[repr(u8)]
enum OperationCode {
    Add = 1,
    Multiply = 2,
    Input = 3,
    Output = 4,
    JumpIfTrue = 5,
    JumpIfFalse = 6,
    LessThan = 7,
    Equals = 8,
    Halt = 99,
}

#[derive(Eq, PartialEq, Debug, FromPrimitive)]
#[repr(u8)]
enum ParameterMode {
    Position = 0,
    Immediate = 1,
}

enum Parameter {
    Position(Index),
    Immediate(Entry),
}

enum Operation {
    Add(Parameter, Parameter, Index),
    Multiply(Parameter, Parameter, Index),
    Input(Index),
    Output(Parameter),
    JumpIfTrue(Parameter, Parameter),
    JumpIfFalse(Parameter, Parameter),
    LessThan(Parameter, Parameter, Index),
    Equals(Parameter, Parameter, Index),
    Halt,
}

pub fn parse_machine_code(input: &str) -> MachineCode {
    input
        .split(',')
        .map(|x| x.trim())
        .filter(|x| !x.is_empty())
        .map(|str| str.parse::<Entry>().unwrap())
        .collect()
}

pub struct Process {
    input: Input,
    pub output: Output,

    pub memory: Vec<Entry>,
    ip: Cell<Index>,
}

impl Process {
    pub fn new(program: &MachineCodeRef) -> Process {
        Process {
            input: VecDeque::new(),
            output: VecDeque::new(),
            memory: program.to_vec(),
            ip: Cell::new(0),
        }
    }

    pub fn provide_input(&mut self, x: Entry) {
        self.input.push_back(x);
    }

    fn param(&self, offset: Index, mode: i32) -> Parameter {
        let x = self.memory[self.ip.get() + offset];

        match FromPrimitive::from_i32(mode) {
            Some(ParameterMode::Position) => Parameter::Position(x as Index),
            Some(ParameterMode::Immediate) => Parameter::Immediate(x),
            None => panic!("Unexpected parameter mode {}", mode),
        }
    }

    fn op_at_ip(&self) -> Operation {
        let op = self.memory[self.ip.get()];
        let op_code = (op % 100) as u8;
        let mode_1 = (op / 100) % 10;
        let mode_2 = (op / 1_000) % 10;
        let _mode_3 = op / 10_000;

        match FromPrimitive::from_u8(op_code) {
            Some(OperationCode::Add) => Operation::Add(
                self.param(1, mode_1),
                self.param(2, mode_2),
                self.memory[self.ip.get() + 3] as Index,
            ),
            Some(OperationCode::Multiply) => Operation::Multiply(
                self.param(1, mode_1),
                self.param(2, mode_2),
                self.memory[self.ip.get() + 3] as Index,
            ),
            Some(OperationCode::Input) => Operation::Input(self.memory[self.ip.get() + 1] as Index),
            Some(OperationCode::Output) => Operation::Output(self.param(1, mode_1)),
            Some(OperationCode::JumpIfTrue) => {
                Operation::JumpIfTrue(self.param(1, mode_1), self.param(2, mode_2))
            }
            Some(OperationCode::JumpIfFalse) => {
                Operation::JumpIfFalse(self.param(1, mode_1), self.param(2, mode_2))
            }
            Some(OperationCode::LessThan) => Operation::LessThan(
                self.param(1, mode_1),
                self.param(2, mode_2),
                self.memory[self.ip.get() + 3] as Index,
            ),
            Some(OperationCode::Equals) => Operation::Equals(
                self.param(1, mode_1),
                self.param(2, mode_2),
                self.memory[self.ip.get() + 3] as Index,
            ),
            Some(OperationCode::Halt) => Operation::Halt,
            None => panic!(
                "Unexpected opcode at {}: {:?}",
                self.ip.get(),
                self.memory[self.ip.get()]
            ),
        }
    }

    fn resolve(&self, parameter: Parameter) -> Entry {
        match parameter {
            Parameter::Position(index) => self.memory[index],
            Parameter::Immediate(x) => x,
        }
    }

    fn write_to_memory(&mut self, index: Index, value: Entry) {
        self.memory[index] = value;
    }

    fn adjust_ip(&mut self, delta: Index) {
        self.set_ip(self.ip.get() + delta);
    }

    fn set_ip(&mut self, new_ip: Index) {
        self.ip.set(new_ip);
    }

    fn execute_op(&mut self, op: Operation) {
        match op {
            // Opcode 1 adds together numbers read from two positions and stores the result in a third position. The three integers immediately after the opcode tell you these three positions - the first two indicate the positions from which you should read the input values, and the third indicates the position at which the output should be stored.
            Operation::Add(a, b, destination) => {
                self.write_to_memory(destination, self.resolve(a) + self.resolve(b));
                self.adjust_ip(4);
            }
            // Opcode 2 works exactly like opcode 1, except it multiplies the two inputs instead of adding them. Again, the three integers after the opcode indicate where the inputs and outputs are, not their values.
            Operation::Multiply(a, b, destination) => {
                self.write_to_memory(destination, self.resolve(a) * self.resolve(b));
                self.adjust_ip(4);
            }
            // Opcode 3 takes a single integer as input and saves it to the position given by its only parameter. For example, the instruction 3,50 would take an input value and store it at address 50.
            Operation::Input(index) => {
                let value = self.input.pop_front().unwrap();
                self.write_to_memory(index, value);
                self.adjust_ip(2);
            }
            // Opcode 4 outputs the value of its only parameter. For example, the instruction 4,50 would output the value at address 50.
            Operation::Output(parameter) => {
                self.output.push_back(self.resolve(parameter));
                self.adjust_ip(2);
            }
            // Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
            Operation::JumpIfTrue(a, b) => {
                if self.resolve(a) != 0 {
                    self.set_ip(self.resolve(b) as Index);
                } else {
                    self.adjust_ip(3);
                }
            }
            // Opcode 6 is jump-if-false: if the first parameter is zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
            Operation::JumpIfFalse(a, b) => {
                if self.resolve(a) == 0 {
                    self.set_ip(self.resolve(b) as Index);
                } else {
                    self.adjust_ip(3);
                }
            }
            // Opcode 7 is less than: if the first parameter is less than the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
            Operation::LessThan(a, b, c) => {
                let value = (self.resolve(a) < self.resolve(b)) as Entry;
                self.write_to_memory(c, value);
                self.adjust_ip(4);
            }
            // Opcode 8 is equals: if the first parameter is equal to the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
            Operation::Equals(a, b, c) => {
                let value = (self.resolve(a) == self.resolve(b)) as Entry;
                self.write_to_memory(c, value);
                self.adjust_ip(4);
            }
            Operation::Halt => panic!("Halt was supposed to handled above"),
        }
    }

    /* Returns true if Halt was encountered, false if Output was encountered instead */
    pub fn run_to_halt_or_output(&mut self) -> bool {
        loop {
            if !self.output.is_empty() {
                return false;
            } else {
                let next_op: Operation = self.op_at_ip();
                match next_op {
                    Operation::Halt => return true,
                    other => self.execute_op(other),
                }
            }
        }
    }

    pub fn run_to_halt(&mut self) {
        loop {
            let next_op: Operation = self.op_at_ip();
            match next_op {
                Operation::Halt => break,
                other => self.execute_op(other),
            }
        }
    }
}