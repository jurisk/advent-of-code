use std::ops::RangeInclusive;

use advent_of_code_2019::intcode::{parse_machine_code, Entry, MachineCodeRef, Process};
use itertools::Itertools;

fn amplifier(program: &MachineCodeRef, phase_setting: u8, input: Entry) -> Entry {
    let mut process = Process::new(program);
    process.provide_input(i128::from(phase_setting));
    process.provide_input(input);
    process.run_to_halt();
    assert_eq!(process.output_len(), 1);
    process.next_output_unsafe()
}

fn signal_1(program: &MachineCodeRef, phases: &[u8]) -> Entry {
    phases.iter().fold(0, |acc, phase_setting| {
        amplifier(program, *phase_setting, acc)
    })
}

fn signal_2(program: &MachineCodeRef, phases: &[u8]) -> Entry {
    let mut processes: Vec<Process> = phases
        .iter()
        .map(|phase| {
            let mut process = Process::new(program);
            process.provide_input(i128::from(*phase));
            process
        })
        .collect();

    let mut next_input = 0;

    for (i, _) in phases.iter().enumerate().cycle() {
        processes[i].provide_input(next_input);
        let was_halted = processes[i].run_to_halt_or_output();
        if was_halted {
            break;
        }
        next_input = processes[i].next_output_unsafe();
    }

    next_input
}

fn solve(
    program: &MachineCodeRef,
    range: RangeInclusive<u8>,
    f: fn(&MachineCodeRef, &[u8]) -> Entry,
) -> Entry {
    range.permutations(5).map(|x| f(program, &x)).max().unwrap()
}

fn part_1(program: &MachineCodeRef) -> Entry {
    solve(program, 0 ..= 4, signal_1)
}

fn part_2(program: &MachineCodeRef) -> Entry {
    solve(program, 5 ..= 9, signal_2)
}

fn main() {
    let data = include_str!("../../resources/07.txt");
    let program = parse_machine_code(data);

    let part_1_result = part_1(&program);
    assert_eq!(part_1_result, 272_368);
    println!("Part 1: {part_1_result}");

    let part_2_result = part_2(&program);
    assert_eq!(part_2_result, 19_741_286);
    println!("Part 2: {part_2_result}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_day_07_part_1() {
        assert_eq!(
            part_1(&parse_machine_code(
                "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0"
            )),
            43210
        );

        assert_eq!(
            part_1(&parse_machine_code(
                "3,23,3,24,1002,24,10,24,1002,23,-1,23,
101,5,23,23,1,24,23,23,4,23,99,0,0"
            )),
            54321
        );

        assert_eq!(
            part_1(&parse_machine_code(
                "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
            )),
            65210
        );
    }

    #[test]
    fn test_day_07_part_2() {
        assert_eq!(
            part_2(&parse_machine_code(
                "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5"
            )),
            139_629_729
        );

        assert_eq!(
            part_2(&parse_machine_code(
                "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10"
            )),
            18216
        );
    }
}
