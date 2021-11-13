use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use std::collections::{HashMap, VecDeque};

pub type Entry = i128;
pub type MachineCode = Vec<Entry>;
pub type MachineCodeRef = [Entry];
pub type Input = VecDeque<Entry>;
pub type Output = VecDeque<Entry>;
pub type Index = i128;

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
    AdjustRelativeBase = 9,
    Halt = 99,
}

impl OperationCode {
    fn of(x: u8) -> OperationCode {
        FromPrimitive::from_u8(x).unwrap_or_else(|| panic!("Unexpected operation code: {}", x))
    }
}

#[derive(Eq, PartialEq, Debug, FromPrimitive)]
#[repr(u8)]
enum ParameterMode {
    Position = 0,
    Immediate = 1,
    Relative = 2,
}

impl ParameterMode {
    fn of(x: Entry) -> ParameterMode {
        FromPrimitive::from_i128(x).unwrap_or_else(|| panic!("Unexpected parameter mode: {}", x))
    }
}

#[derive(Debug)]
enum Parameter {
    Position(Index),
    Immediate(Entry),
    Relative(Index),
}

#[derive(Debug)]
enum Operation {
    Add(Parameter, Parameter, Parameter),
    Multiply(Parameter, Parameter, Parameter),
    Input(Parameter),
    Output(Parameter),
    JumpIfTrue(Parameter, Parameter),
    JumpIfFalse(Parameter, Parameter),
    LessThan(Parameter, Parameter, Parameter),
    Equals(Parameter, Parameter, Parameter),
    AdjustRelativeBase(Parameter),
    Halt,
}

pub fn parse_machine_code(input: &str) -> MachineCode {
    input
        .split(',')
        .map(|x| x.trim())
        .filter(|x| !x.is_empty())
        .map(|str| {
            str.parse::<Entry>()
                .unwrap_or_else(|_| panic!("Unrecognized: {}", str))
        })
        .collect()
}

pub fn test_program_as_string(program: &str, expected: &str) {
    let program = parse_machine_code(program);
    let mut process = Process::new(&program);
    process.run_to_halt();
    let obtained = process.memory_as_comma_delimited_string(program.len() as Index);
    assert_eq!(expected, obtained)
}

pub struct Memory {
    map: HashMap<Index, Entry>,
}

impl Memory {
    pub fn from_program(program: &MachineCodeRef) -> Memory {
        let mut result = Memory {
            map: HashMap::new(),
        };

        program
            .iter()
            .enumerate()
            .for_each(|(index, value)| result.write(index as Index, *value));

        result
    }

    pub fn read(&self, index: Index) -> Entry {
        *self.map.get(&index).unwrap_or(&0)
    }

    pub fn write(&mut self, index: Index, value: Entry) {
        self.map.insert(index, value);
    }
}

pub struct Process {
    input: Input,
    output: Output,

    memory: Memory,
    ip: Index,
    relative_base: Index,
}

impl Process {
    pub fn new(program: &MachineCodeRef) -> Process {
        Process {
            input: VecDeque::new(),
            output: VecDeque::new(),
            memory: Memory::from_program(program),
            ip: 0,
            relative_base: 0,
        }
    }

    pub fn from_string(s: &str) -> Process {
        let program = parse_machine_code(s);
        Process::new(&program)
    }

    pub fn provide_input(&mut self, x: Entry) {
        self.input.push_back(x);
    }

    fn param(&self, offset: Index, mode: ParameterMode) -> Parameter {
        let x = self.read_from_memory_offset_by_ip(offset);

        match mode {
            ParameterMode::Position => Parameter::Position(x as Index),
            ParameterMode::Immediate => Parameter::Immediate(x),
            ParameterMode::Relative => Parameter::Relative(x as Index),
        }
    }

    fn op_at_ip(&self) -> Operation {
        let op = self.read_from_memory_offset_by_ip(0);
        let op_code = OperationCode::of((op % 100) as u8);
        let mode_1 = ParameterMode::of((op / 100) % 10);
        let mode_2 = ParameterMode::of((op / 1_000) % 10);
        let mode_3 = ParameterMode::of(op / 10_000);

        match op_code {
            OperationCode::Add => Operation::Add(
                self.param(1, mode_1),
                self.param(2, mode_2),
                self.param(3, mode_3),
            ),
            OperationCode::Multiply => Operation::Multiply(
                self.param(1, mode_1),
                self.param(2, mode_2),
                self.param(3, mode_3),
            ),
            OperationCode::Input => Operation::Input(self.param(1, mode_1)),
            OperationCode::Output => Operation::Output(self.param(1, mode_1)),
            OperationCode::JumpIfTrue => {
                Operation::JumpIfTrue(self.param(1, mode_1), self.param(2, mode_2))
            }
            OperationCode::JumpIfFalse => {
                Operation::JumpIfFalse(self.param(1, mode_1), self.param(2, mode_2))
            }
            OperationCode::LessThan => Operation::LessThan(
                self.param(1, mode_1),
                self.param(2, mode_2),
                self.param(3, mode_3),
            ),
            OperationCode::Equals => Operation::Equals(
                self.param(1, mode_1),
                self.param(2, mode_2),
                self.param(3, mode_3),
            ),
            OperationCode::AdjustRelativeBase => {
                Operation::AdjustRelativeBase(self.param(1, mode_1))
            }
            OperationCode::Halt => Operation::Halt,
        }
    }

    fn resolve(&self, parameter: Parameter) -> Entry {
        match parameter {
            Parameter::Position(index) => self.read_from_memory(index),
            Parameter::Immediate(x) => x,
            Parameter::Relative(index) => self.read_from_memory(self.relative_base + index),
        }
    }

    pub fn read_from_memory(&self, index: Index) -> Entry {
        self.memory.read(index)
    }

    pub fn read_from_memory_offset_by_ip(&self, offset: Index) -> Entry {
        self.memory.read(self.ip + offset)
    }

    fn write_to_memory(&mut self, index: Index, value: Entry) {
        self.memory.write(index, value);
    }

    fn adjust_ip(&mut self, delta: Index) {
        self.set_ip(self.ip + delta);
    }

    fn set_ip(&mut self, new_ip: Index) {
        self.ip = new_ip;
    }

    fn resolve_index(&self, parameter: Parameter) -> Index {
        match parameter {
            Parameter::Position(x) => x,
            // can be done cleaner with stronger types, but it is OK for now
            Parameter::Immediate(_) => {
                panic!("Did not expect immediate parameter for resolving index")
            }
            Parameter::Relative(x) => x + self.relative_base,
        }
    }

    fn execute_op(&mut self, op: Operation) {
        // println!("{:04}: {:?}", self.ip, op);
        match op {
            // Opcode 1 adds together numbers read from two positions and stores the result in a third position. The three integers immediately after the opcode tell you these three positions - the first two indicate the positions from which you should read the input values, and the third indicates the position at which the output should be stored.
            Operation::Add(a, b, destination) => {
                let index = self.resolve_index(destination);
                self.write_to_memory(index, self.resolve(a) + self.resolve(b));
                self.adjust_ip(4);
            }
            // Opcode 2 works exactly like opcode 1, except it multiplies the two inputs instead of adding them. Again, the three integers after the opcode indicate where the inputs and outputs are, not their values.
            Operation::Multiply(a, b, destination) => {
                let index = self.resolve_index(destination);
                self.write_to_memory(index, self.resolve(a) * self.resolve(b));
                self.adjust_ip(4);
            }
            // Opcode 3 takes a single integer as input and saves it to the position given by its only parameter. For example, the instruction 3,50 would take an input value and store it at address 50.
            Operation::Input(parameter) => {
                let value = self.input.pop_front().unwrap();
                let index = self.resolve_index(parameter);
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
                self.write_to_memory(self.resolve_index(c), value);
                self.adjust_ip(4);
            }
            // Opcode 8 is equals: if the first parameter is equal to the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
            Operation::Equals(a, b, c) => {
                let value = (self.resolve(a) == self.resolve(b)) as Entry;
                self.write_to_memory(self.resolve_index(c), value);
                self.adjust_ip(4);
            }
            Operation::AdjustRelativeBase(x) => {
                self.relative_base += self.resolve(x);
                self.adjust_ip(2);
            }
            Operation::Halt => {
                panic!("Halt was supposed to handled above")
            }
        }
    }

    pub fn output_len(&self) -> usize {
        self.output.len()
    }

    pub fn run_next_op(&mut self) -> bool {
        let next = self.op_at_ip();
        match next {
            Operation::Halt => true,
            _ => {
                self.execute_op(self.op_at_ip());
                false
            }
        }
    }

    fn run_to_condition<F>(&mut self, f: F) -> bool
    where
        F: Fn(&Process) -> bool,
    {
        loop {
            if f(self) {
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

    pub fn unsatisfied_input(&self) -> bool {
        match self.op_at_ip() {
            Operation::Input(_) => self.input.is_empty(),
            _ => false,
        }
    }

    pub fn next_output_unsafe(&mut self) -> Entry {
        self.output.pop_front().expect("No output found")
    }

    pub fn read_output(&mut self) -> Vec<Entry> {
        let result = self.output.iter().cloned().collect();
        self.output.clear();
        result
    }

    pub fn output_as_string(&self) -> String {
        format!("{:?}", self.output)
    }

    pub fn run_to_halt_or_output_size(&mut self, min_output_size: usize) -> bool {
        let compare_with = min_output_size;
        self.run_to_condition(|x| x.output_len() >= compare_with)
    }

    /* Returns true if Halt was encountered, false if Output was encountered instead */
    pub fn run_to_halt_or_output(&mut self) -> bool {
        self.run_to_halt_or_output_size(1)
    }

    pub fn run_to_halt(&mut self) {
        self.run_to_condition(|_| false);
    }

    pub fn memory_as_comma_delimited_string(&self, max_index: Index) -> String {
        (0..max_index)
            .map(|idx| format!("{}", self.read_from_memory(idx)))
            .collect::<Vec<String>>()
            .join(",")
    }
}
