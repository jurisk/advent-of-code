use itertools::Itertools;

const BASE_PATTERN: [i32; 4] = [0, 1, 0, -1];

fn coef(index: usize, phase: usize) -> i32 {
    BASE_PATTERN[((index + 1) / (phase + 1)) % 4]
}

fn apply_phase_1(numbers: &[u8]) -> Vec<u8> {
    numbers
        .iter()
        .enumerate()
        .map(|(phase, _)| {
            println!("Phase {phase}");

            let summed: i32 = numbers
                .iter()
                .enumerate()
                .map(|(index, n)| i32::from(*n) * coef(index, phase))
                .sum();
            u8::try_from(summed.abs() % 10).unwrap()
        })
        .collect()
}

fn parse(data: &str) -> Vec<u8> {
    data.trim().chars().map(|x| (x as u8) - b'0').collect()
}

fn display(v: &[u8]) -> String {
    v.iter().map(|n| (n + b'0') as char).join("")
}

fn solve_1(data: &str, phases: u8) -> String {
    let numbers: Vec<_> = parse(data);
    let result = apply_phases_1(numbers, phases);
    display(&result)
}

fn apply_phases_1(numbers: Vec<u8>, phases: u8) -> Vec<u8> {
    (0..phases).fold(numbers, |a, phase| {
        println!("Applying phase {phase}");
        apply_phase_1(&a)
    })
}

fn apply_phase_2(numbers: &[u8]) -> Vec<u8> {
    let mut sum: i32 = numbers.iter().map(|x| i32::from(*x)).sum();
    let mut out: Vec<u8> = Vec::new();
    for x in numbers.iter() {
        let res = u8::try_from(sum % 10).unwrap();
        out.push(res);
        sum -= i32::from(*x);
    }
    out
}

fn apply_phases_2(numbers: Vec<u8>, phases: u8) -> Vec<u8> {
    (0..phases).fold(numbers, |a, phase| {
        println!("Applying phase {phase}");
        apply_phase_2(&a)
    })
}

fn solve_2(data: &str, repeat: usize, phases: u8) -> String {
    let single: Vec<u8> = parse(data);
    let repeated: Vec<u8> = single
        .iter()
        .cycle()
        .take(repeat * single.len())
        .copied()
        .collect();
    println!("{}", repeated.len());
    let result = apply_phases_2(repeated, phases);
    display(&result)
}

fn part_1() {
    let data = include_str!("../../resources/16.txt");
    let result = &solve_1(data, 100)[..8];
    println!("Part 1: {result}");
    assert_eq!(result, "68764632");
}

fn part_2() {
    let data = include_str!("../../resources/16.txt");
    let message_offset: usize = (data[..7]).parse::<usize>().unwrap();
    println!("Message offset: {message_offset}");
    let result = solve_2(data, 10000, 100);
    let message = &result[message_offset..message_offset + 8];
    println!("Part 2: {message}");
    assert_eq!(message, "52825021");
}

fn main() {
    part_1();
    part_2();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve_1() {
        assert_eq!(solve_1("12345678", 1), "48226158");
        assert_eq!(solve_1("12345678", 2), "34040438");
        assert_eq!(solve_1("12345678", 3), "03415518");
        assert_eq!(solve_1("12345678", 4), "01029498");
        assert!(solve_1("80871224585914546619083218645595", 100).starts_with("24176176"));
        assert!(solve_1("19617804207202209144916044189917", 100).starts_with("73745418"));
        assert!(solve_1("69317163492948606335995924319873", 100).starts_with("52432133"));
        part_1();
    }

    #[test]
    #[ignore] // Slow
    fn test_solve_2() {
        part_2();
    }
}
