use advent_of_code_2019::intcode::{Entry, MachineCodeRef, Process, parse_machine_code};

fn run_with_modifications(incoming: &MachineCodeRef, noun: Entry, verb: Entry) -> Entry {
    let mut program = incoming.to_owned();
    program[1] = noun;
    program[2] = verb;

    let mut process = Process::new(&program);
    process.run_to_halt();

    process.read_from_memory(0)
}

fn main() {
    let data = include_str!("../../resources/02.txt");
    let program = parse_machine_code(data);

    let part1 = run_with_modifications(&program, 12, 2);

    assert_eq!(part1, 3_654_868);
    println!("Part 1: {part1}");

    let expected_output = 19_690_720;

    for noun in 0 ..= 99 {
        for verb in 0 ..= 99 {
            let output = run_with_modifications(&program, noun, verb);
            if output == expected_output {
                let answer = 100 * noun + verb;
                assert_eq!(answer, 7_014);
                println!("Part 2: {answer}");
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use advent_of_code_2019::intcode::test_program_as_string;

    #[test]
    fn test_day_02() {
        test_program_as_string(
            "1,9,10,3,2,3,11,0,99,30,40,50",
            "3500,9,10,70,2,3,11,0,99,30,40,50",
        );
        test_program_as_string("1,0,0,0,99", "2,0,0,0,99");
        test_program_as_string("2,3,0,3,99", "2,3,0,6,99");
        test_program_as_string("2,4,4,5,99,0", "2,4,4,5,99,9801");
        test_program_as_string("1,1,1,4,99,5,6,0,99", "30,1,1,4,2,5,6,0,99");
    }
}
