use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

type Entry = usize;
type Program = Vec<Entry>;
type ProgramForRef = [Entry];

#[derive(Eq, PartialEq, Debug, FromPrimitive)]
#[repr(usize)]
enum OperationCode {
    Add = 1,
    Multiply = 2,
    Halt = 99,
}

fn parse(input: &str) -> Program {
    input
        .split(',')
        .map(|x| x.trim())
        .filter(|x| !x.is_empty())
        .map(|str| str.parse::<Entry>().unwrap())
        .collect()
}

fn binary_op(program: &mut Program, ip: usize, f: fn(Entry, Entry) -> Entry) {
    let idx_1 = program[ip + 1];
    let idx_2 = program[ip + 2];
    let result = f(program[idx_1], program[idx_2]);
    let idx_3 = program[ip + 3];
    program[idx_3] = result;
}

fn run_program(program: &mut Program) -> &mut Program {
    let mut ip = 0;

    loop {
        match FromPrimitive::from_usize(program[ip]) {
            Some(OperationCode::Add) => {
                binary_op(program, ip, |a, b| a + b);
                ip += 4;
            }
            Some(OperationCode::Multiply) => {
                binary_op(program, ip, |a, b| a * b);
                ip += 4;
            }
            Some(OperationCode::Halt) => break,
            None => panic!("Unexpected opcode at {}: {:?}", ip, program[ip]),
        }
    }

    program
}

fn run_program_as_strings(s: &str) -> String {
    run_program(&mut parse(s))
        .iter()
        .map(|x| format!("{}", x))
        .collect::<Vec<String>>()
        .join(",")
}

fn run_with_modifications(incoming: &ProgramForRef, noun: Entry, verb: Entry) -> Entry {
    let mut program = incoming.to_owned();
    program[1] = noun;
    program[2] = verb;

    let after = run_program(&mut program);

    after[0]
}

fn main() {
    assert_eq!(
        run_program_as_strings("1,9,10,3,2,3,11,0,99,30,40,50"),
        "3500,9,10,70,2,3,11,0,99,30,40,50"
    );
    assert_eq!(run_program_as_strings("1,0,0,0,99"), "2,0,0,0,99");
    assert_eq!(run_program_as_strings("2,3,0,3,99"), "2,3,0,6,99");
    assert_eq!(run_program_as_strings("2,4,4,5,99,0"), "2,4,4,5,99,9801");
    assert_eq!(
        run_program_as_strings("1,1,1,4,99,5,6,0,99"),
        "30,1,1,4,2,5,6,0,99"
    );

    let data = include_str!("../../resources/02.txt");
    let program = parse(data);

    let part1 = run_with_modifications(&program, 12, 2);

    assert_eq!(part1, 3_654_868);
    println!("Part 1: {}", part1);

    let expected_output = 19_690_720;

    for noun in 0..=99 {
        for verb in 0..=99 {
            let output = run_with_modifications(&program, noun, verb);
            if output == expected_output {
                let answer = 100 * noun + verb;
                assert_eq!(answer, 7_014);
                println!("Part 2: {}", answer);
            }
        }
    }
}
