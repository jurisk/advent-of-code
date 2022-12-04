use advent_of_code_2019::intcode::{
    parse_machine_code, Entry, MachineCode, MachineCodeRef, Process,
};

fn solve(program: &MachineCodeRef, input: Entry, expected_output: Entry) {
    let mut process = Process::new(program);
    process.provide_input(input);
    process.run_to_halt();
    if process.output_len() == 1 {
        let result_1 = process.next_output_unsafe();
        println!("Output for input {input}: {result_1}");
        assert_eq!(result_1, expected_output);
    } else {
        panic!("Invalid output: {:?}", process.output_as_string());
    }
}

fn test_program() -> MachineCode {
    parse_machine_code(include_str!("../../resources/09.txt"))
}

fn solve_1() {
    let program = test_program();
    solve(&program, 1, 2_351_176_124);
}

fn solve_2() {
    let program = test_program();
    solve(&program, 2, 73_110);
}

fn main() {
    solve_1();
    solve_2();
}

#[cfg(test)]
mod tests {
    use super::*;
    use advent_of_code_2019::intcode::{parse_machine_code, Index};

    #[test]
    fn test_solve_1() {
        solve_1();
    }

    #[test]
    fn test_solve_2() {
        solve_2();
    }

    #[test]
    fn test_1() {
        //  takes no input and produces a copy of itself as output
        let input = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99";
        let program = parse_machine_code(input);
        let mut process = Process::new(&program);
        process.provide_input(1);
        process.run_to_halt();
        let result = process.memory_as_comma_delimited_string(program.len() as Index);
        assert_eq!(result, input);
    }

    #[test]
    fn test_2() {
        let mut process = Process::from_string("1102,34915192,34915192,7,4,7,99,0");
        process.run_to_halt();
        let result = process.next_output_unsafe();
        // should output a 16-digit number
        assert_eq!(result, 1_219_070_632_396_864);
    }

    #[test]
    fn test_3() {
        let number = 1_125_899_906_842_624;
        let program = "104,1125899906842624,99";
        let mut process = Process::from_string(program);
        process.run_to_halt();
        let result = process.next_output_unsafe();
        // should output the large number in the middle
        assert_eq!(result, number);
    }
}
