use advent_of_code_common::parsing::{Error, parse_lines_to_vec};
use advent_of_code_common::utils::head_tail;

fn parse(input: &str) -> Result<Vec<String>, Error> {
    parse_lines_to_vec(input)
}

fn unescape(input: &str) -> String {
    fn f(s: &[char]) -> Vec<char> {
        let mut result: Vec<char> = Vec::new();

        match head_tail(s) {
            None => {},
            Some((&h, t)) => {
                let (ch, idx) = if h == '\\' {
                    match t[0] {
                        '\\' => ('\\', 1),
                        '"' => ('"', 1),
                        'x' => {
                            let hex_string = format!("{}{}", t[1], t[2]);
                            let code_point =
                                u32::from_str_radix(&hex_string, 16).unwrap_or_else(|err| {
                                    panic!("Invalid hexadecimal number {hex_string}: {err}")
                                });
                            let ch: char = char::from_u32(code_point).unwrap_or_else(|| {
                                panic!("Invalid Unicode code point: {hex_string}")
                            });
                            (ch, 3)
                        },
                        _ => panic!("Unexpected character after escape: {}", t[0]),
                    }
                } else {
                    (h, 0)
                };

                result.push(ch);
                result.append(&mut f(&t[idx ..])); // Not tail rec
            },
        }

        result
    }

    assert!(input.starts_with('"'));
    assert!(input.ends_with('"'));
    let chars: Vec<char> = input[1 .. input.len() - 1].chars().collect();
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

const DATA: &str = include_str!("../../resources/08.txt");

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
