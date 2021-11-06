use advent_of_code::intcode::{parse_machine_code, run_program};

fn main() {
    let data = include_str!("../../resources/05.txt");
    let program = parse_machine_code(data);

    let (_resulting_1, mut output_1) = run_program(&program, &[1]);
    let part_1 = output_1.pop_back().unwrap();
    assert_eq!(part_1, 9938601);
    println!("Part 1: {:?}", part_1);

    let (_resulting_2, mut output_2) = run_program(&program, &[5]);
    let part_2 = output_2.pop_back().unwrap();
    assert_eq!(part_2, 4283952);
    println!("Part 2: {:?}", part_2);
}
