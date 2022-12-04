use advent_of_code_2019::intcode::{parse_machine_code, Entry, MachineCode, Process};

fn machine_code() -> MachineCode {
    parse_machine_code(include_str!("../../resources/21.txt"))
}

fn solve(commands: Vec<&str>) -> Entry {
    let mut process = Process::new(&machine_code());

    for command in commands {
        process.provide_input_as_string(&[command, "\n"].join(""));
    }

    process.run_to_halt();

    let results = process.read_output();
    let result = *results.last().unwrap();
    result
}

// Jump if landing is clear and there is a hole in one of the 3 squares upcoming
// (!A || !B || !C) && D
const PART_1: [&str; 6] = [
    "NOT A T", // !A
    "NOT B J", // !B
    "OR T J",  // .. || ..
    "NOT C T", // !C
    "OR T J",  // || ..
    "AND D J", // && D
];

fn solve_1() {
    let result = solve(vec![PART_1.to_vec(), vec!["WALK"]].concat());

    assert_eq!(result, 19_357_544);
    println!("Part 1: {result}");
}

// Avoid situation where we will be immediately forced to jump (!E) however landing will be a hole (!H)
// (part 1) && !(!E && !H)
//    ... is the same as ...
// (part 1) && (E || H)
const PART_2: [&str; 4] = [
    "NOT E T", // ...
    "NOT T T", // E
    "OR H T",  // || H
    "AND T J", // ... && ...
];

fn solve_2() {
    let commands = [PART_1.to_vec(), PART_2.to_vec(), vec!["RUN"]].concat();
    let result = solve(commands);
    assert_eq!(result, 1_144_498_646);
    println!("Part 2: {result}");
}

fn main() {
    solve_1();
    solve_2();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve_1() {
        solve_1();
    }

    #[test]
    fn test_solve_2() {
        solve_2();
    }
}
