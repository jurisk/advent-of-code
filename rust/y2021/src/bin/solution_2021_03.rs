use itertools::Itertools;

fn parse(input: &str) -> Vec<Vec<char>> {
    input
        .split('\n')
        .filter(|x| !x.is_empty())
        .map(|x| x.chars().collect())
        .collect()
}

fn helper_for_something_common(line: &[char], arr: [char; 2]) -> char {
    let zeros = line.iter().filter(|ch| **ch == '0').count();
    if zeros > line.len() / 2 {
        arr[0]
    } else {
        arr[1]
    }
}

fn most_common(line: &[char]) -> char {
    helper_for_something_common(line, ['0', '1'])
}

fn least_common(line: &[char]) -> char {
    helper_for_something_common(line, ['1', '0'])
}

fn to_binary(a: &[char]) -> usize {
    usize::from_str_radix(&a.to_vec().iter().join(""), 2).unwrap()
}

fn binary_mult(a: &[char], b: &[char]) -> usize {
    to_binary(a) * to_binary(b)
}

fn nth_from_all(lines: &[Vec<char>], idx: usize) -> Vec<char> {
    lines.iter().map(|line| line[idx]).collect()
}

fn part_1(input: &str) -> usize {
    let lines = parse(input);
    let number_of_digits = lines[0].len();

    let transposed: Vec<Vec<char>> = (0..number_of_digits)
        .map(|idx| nth_from_all(&lines, idx))
        .collect();

    let gamma_rate: Vec<char> = transposed.iter().map(|x| most_common(x)).collect();
    let epsilon_rate: Vec<char> = transposed.iter().map(|x| least_common(x)).collect();

    binary_mult(&gamma_rate, &epsilon_rate)
}

fn solve_2(numbers: &[Vec<char>], idx: usize, f: fn(&[char]) -> char) -> Vec<char> {
    if numbers.len() == 1 {
        numbers[0].clone()
    } else {
        let appropriate_chars = nth_from_all(numbers, idx);
        let needed = f(&appropriate_chars);
        let valid: Vec<_> = numbers
            .iter()
            .filter(|s| s[idx] == needed)
            .cloned()
            .collect();
        solve_2(&valid, idx + 1, f)
    }
}

fn part_2(input: &str) -> usize {
    let lines = parse(input);
    let oxygen_generator_rating = solve_2(&lines, 0, most_common);
    let co2_scrubber_rating = solve_2(&lines, 0, least_common);
    binary_mult(&oxygen_generator_rating, &co2_scrubber_rating)
}

#[allow(clippy::unreadable_literal)]
fn main() {
    let test_data = "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010";

    let test_result_1 = part_1(test_data);
    assert_eq!(test_result_1, 198);

    let data = include_str!("../../resources/03.txt");
    let result_1 = part_1(data);

    assert_eq!(result_1, 2261546);
    println!("Part 1: {result_1}");

    let test_result_2 = part_2(test_data);
    assert_eq!(test_result_2, 230);

    let result_2 = part_2(data);
    assert_eq!(result_2, 6775520);

    println!("Part 2: {result_2}");
}
