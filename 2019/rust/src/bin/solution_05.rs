use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use std::collections::VecDeque;

type Entry = i32;
type MachineCode = Vec<Entry>;
type MachineCodeRef = [Entry];
type Input = VecDeque<Entry>;
type Output = VecDeque<Entry>;
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

trait Executable {
    fn execute(
        self,
        program: &mut MachineCode,
        ip: &mut Index,
        input: &mut Input,
        output: &mut Output,
    );
}

fn resolve(program: &MachineCodeRef, parameter: Parameter) -> Entry {
    match parameter {
        Parameter::Position(index) => program[index],
        Parameter::Immediate(x) => x,
    }
}

impl Executable for Operation {
    fn execute(
        self,
        program: &mut MachineCode,
        ip: &mut Index,
        input: &mut Input,
        output: &mut Output,
    ) {
        match self {
            // Opcode 1 adds together numbers read from two positions and stores the result in a third position. The three integers immediately after the opcode tell you these three positions - the first two indicate the positions from which you should read the input values, and the third indicates the position at which the output should be stored.
            Operation::Add(a, b, destination) => {
                program[destination] = resolve(program, a) + resolve(program, b);
                *ip += 4;
            }
            // Opcode 2 works exactly like opcode 1, except it multiplies the two inputs instead of adding them. Again, the three integers after the opcode indicate where the inputs and outputs are, not their values.
            Operation::Multiply(a, b, destination) => {
                program[destination] = resolve(program, a) * resolve(program, b);
                *ip += 4;
            }
            // Opcode 3 takes a single integer as input and saves it to the position given by its only parameter. For example, the instruction 3,50 would take an input value and store it at address 50.
            Operation::Input(index) => {
                let value = input.pop_front().unwrap();
                program[index] = value;
                *ip += 2;
            }
            // Opcode 4 outputs the value of its only parameter. For example, the instruction 4,50 would output the value at address 50.
            Operation::Output(parameter) => {
                output.push_back(resolve(program, parameter));
                *ip += 2;
            }
            // Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
            Operation::JumpIfTrue(a, b) => {
                if resolve(program, a) != 0 {
                    *ip = resolve(program, b) as Index;
                } else {
                    *ip += 3;
                }
            }
            // Opcode 6 is jump-if-false: if the first parameter is zero, it sets the instruction pointer to the value from the second parameter. Otherwise, it does nothing.
            Operation::JumpIfFalse(a, b) => {
                if resolve(program, a) == 0 {
                    *ip = resolve(program, b) as Index;
                } else {
                    *ip += 3;
                }
            }
            // Opcode 7 is less than: if the first parameter is less than the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
            Operation::LessThan(a, b, c) => {
                let value = (resolve(program, a) < resolve(program, b)) as Entry;
                program[c] = value;
                *ip += 4;
            }
            // Opcode 8 is equals: if the first parameter is equal to the second parameter, it stores 1 in the position given by the third parameter. Otherwise, it stores 0.
            Operation::Equals(a, b, c) => {
                let value = (resolve(program, a) == resolve(program, b)) as Entry;
                program[c] = value;
                *ip += 4;
            }
            Operation::Halt => panic!("Halt was supposed to handled above"),
        }
    }
}

fn parse(input: &str) -> MachineCode {
    input
        .split(',')
        .map(|x| x.trim())
        .filter(|x| !x.is_empty())
        .map(|str| str.parse::<Entry>().unwrap())
        .collect()
}

fn param(value: Entry, mode: i32) -> Parameter {
    match FromPrimitive::from_i32(mode) {
        Some(ParameterMode::Position) => Parameter::Position(value as Index),
        Some(ParameterMode::Immediate) => Parameter::Immediate(value),
        None => panic!("Unexpected parameter mode {}", mode),
    }
}

fn op_at_ip(program: &MachineCodeRef, ip: Index) -> Operation {
    let op = program[ip];
    let op_code = (op % 100) as u8;
    let mode_1 = (op / 100) % 10;
    let mode_2 = (op / 1_000) % 10;
    let _mode_3 = op / 10_000;

    match FromPrimitive::from_u8(op_code) {
        Some(OperationCode::Add) => Operation::Add(
            param(program[ip + 1], mode_1),
            param(program[ip + 2], mode_2),
            program[ip + 3] as Index,
        ),
        Some(OperationCode::Multiply) => Operation::Multiply(
            param(program[ip + 1], mode_1),
            param(program[ip + 2], mode_2),
            program[ip + 3] as Index,
        ),
        Some(OperationCode::Input) => Operation::Input(program[ip + 1] as Index),
        Some(OperationCode::Output) => Operation::Output(param(program[ip + 1], mode_1)),
        Some(OperationCode::JumpIfTrue) => Operation::JumpIfTrue(
            param(program[ip + 1], mode_1),
            param(program[ip + 2], mode_2),
        ),
        Some(OperationCode::JumpIfFalse) => Operation::JumpIfFalse(
            param(program[ip + 1], mode_1),
            param(program[ip + 2], mode_2),
        ),
        Some(OperationCode::LessThan) => Operation::LessThan(
            param(program[ip + 1], mode_1),
            param(program[ip + 2], mode_2),
            program[ip + 3] as Index,
        ),
        Some(OperationCode::Equals) => Operation::Equals(
            param(program[ip + 1], mode_1),
            param(program[ip + 2], mode_2),
            program[ip + 3] as Index,
        ),
        Some(OperationCode::Halt) => Operation::Halt,
        None => panic!("Unexpected opcode at {}: {:?}", ip, program[ip]),
    }
}

fn run_program(input_program: &MachineCodeRef, input: &[Entry]) -> (MachineCode, Output) {
    let mut input_deque = VecDeque::from(input.to_vec());
    let mut program: MachineCode = input_program.to_vec();

    let mut ip: Index = 0;
    let mut output: Output = VecDeque::new();

    loop {
        let next_op: Operation = op_at_ip(&program, ip);
        match next_op {
            Operation::Halt => break,
            other => {
                other.execute(&mut program, &mut ip, &mut input_deque, &mut output);
            }
        }
    }

    (program.to_vec(), output)
}

fn main() {
    let data = include_str!("../../resources/05.txt");
    let program = parse(data);

    let (_resulting_1, mut output_1) = run_program(&program, &[1]);
    let part_1 = output_1.pop_back().unwrap();
    assert_eq!(part_1, 9938601);
    println!("Part 1: {:?}", part_1);

    let (_resulting_2, mut output_2) = run_program(&program, &[5]);
    let part_2 = output_2.pop_back().unwrap();
    assert_eq!(part_2, 4283952);
    println!("Part 2: {:?}", part_2);
}
