use advent_of_code::intcode::{parse_machine_code, Process};

fn main() {
    let data = include_str!("../../resources/05.txt");
    let program = parse_machine_code(data);

    let mut process_1 = Process::new(&program);
    process_1.provide_input(1);
    process_1.run_to_halt();
    let part_1 = process_1.next_output_unsafe();
    assert_eq!(part_1, 9_938_601);
    println!("Part 1: {:?}", part_1);

    let mut process_2 = Process::new(&program);
    process_2.provide_input(5);
    process_2.run_to_halt();
    let part_2 = process_2.next_output_unsafe();
    assert_eq!(part_2, 4_283_952);
    println!("Part 2: {:?}", part_2);
}
