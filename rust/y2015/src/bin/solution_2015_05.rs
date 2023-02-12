use advent_of_code_common::parsing::{parse_lines_to_vec, Error};
use itertools::Itertools;

fn parse(input: &str) -> Result<Vec<String>, Error> {
    parse_lines_to_vec(input)
}

const VOWELS: [char; 5] = ['a', 'e', 'i', 'o', 'u'];
const FORBIDDEN: [&str; 4] = ["ab", "cd", "pq", "xy"];

fn is_nice_1(s: &str) -> bool {
    let enough_vowels = s.chars().filter(|ch| VOWELS.contains(ch)).count() >= 3;
    let letter_in_a_row = s.chars().tuple_windows().any(|(a, b)| a == b);
    let does_not_contain_forbidden = !FORBIDDEN.iter().any(|x| s.contains(x));
    enough_vowels && letter_in_a_row && does_not_contain_forbidden
}

fn pair_appears_twice_without_overlapping(s: &str) -> bool {
    if s.len() < 4 {
        false
    } else {
        let a = &s[.. 2];
        let b = &s[2 ..];
        if b.contains(a) {
            true
        } else {
            pair_appears_twice_without_overlapping(&s[1 ..])
        }
    }
}

fn is_nice_2(s: &str) -> bool {
    let letter_repeats_with_another_inbetween = s.chars().tuple_windows().any(|(a, _b, c)| a == c);
    pair_appears_twice_without_overlapping(s) && letter_repeats_with_another_inbetween
}

fn solve_1(data: &[String]) -> usize {
    data.iter().filter(|s| is_nice_1(s)).count()
}

fn solve_2(data: &[String]) -> usize {
    data.iter().filter(|s| is_nice_2(s)).count()
}

fn part_1(input: &str) -> Result<usize, Error> {
    let data = parse(input)?;
    Ok(solve_1(&data))
}

fn part_2(input: &str) -> Result<usize, Error> {
    let data = parse(input)?;
    Ok(solve_2(&data))
}

const DATA: &str = include_str!("../../resources/05.txt");

fn main() -> Result<(), Error> {
    let result_1 = part_1(DATA)?;
    println!("Part 1: {result_1}");

    let result_2 = part_2(DATA)?;
    println!("Part 2: {result_2}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(236));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(51));
    }
}
