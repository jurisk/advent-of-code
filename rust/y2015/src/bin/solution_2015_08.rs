use advent_of_code_common::parsing::{parse_lines_to_vec, Error};
use advent_of_code_common::utils::head_tail;

const DATA: &str = include_str!("../../resources/08.txt");

fn parse(input: &str) -> Result<Vec<String>, Error> {
    parse_lines_to_vec(input)
}

fn unescape(input: &str) -> String {
    fn f(s: &[char]) -> Vec<char> {
        let (h, t) = head_tail(s);
        match h {
            None => vec![],
            Some(&h) => match h {
                '\\' => match t[0] {
                    '\\' => vec![vec!['\\'], f(&t[1..])].concat(),
                    '"' => vec![vec!['"'], f(&t[1..])].concat(),
                    'x' => vec![
                        vec![char::from_u32(
                            u32::from_str_radix(&format!("{}{}", t[1], t[2]), 16).unwrap(),
                        )
                        .unwrap()],
                        f(&t[3..]),
                    ]
                    .concat(),
                    _ => panic!("Unexpected character after escape: {}", t[0]),
                },
                _ => vec![vec![h], f(&t[0..t.len()])].concat(),
            },
        }
    }

    assert!(input.starts_with('"'));
    assert!(input.ends_with('"'));
    let chars: Vec<char> = input[1..input.len() - 1].chars().collect();
    f(&chars).into_iter().collect()
}

fn solve_1(data: &[String]) -> usize {
    data.iter()
        .map(|s| s.chars().count() - unescape(s).chars().count())
        .sum()
}

fn solve_2(data: &[String]) -> usize {
    data.iter()
        .map(|s| s.chars().filter(|&ch| ch == '"' || ch == '\\').count() + 2)
        .sum()
}

fn part_1(input: &str) -> Result<usize, Error> {
    let data = parse(input)?;
    Ok(solve_1(&data))
}

fn part_2(input: &str) -> Result<usize, Error> {
    let data = parse(input)?;
    Ok(solve_2(&data))
}

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

    const TEST: &str = include_str!("../../resources/08-test.txt");

    #[test]
    fn test_solve_1_test() {
        assert_eq!(part_1(TEST), Ok(12));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(1333));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(2046));
    }
}
