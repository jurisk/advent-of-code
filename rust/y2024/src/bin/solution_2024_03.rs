const DATA: &str = include_str!("../../resources/03.txt");

type R = usize;
type Data = &'static str;

use regex::Regex;

const MUL: &str = r"mul\((?<a>\d+),(?<b>\d+)\)";
const DO: &str = r"do()";
const DONT: &str = r"don't()";

fn to_regex(s: &str) -> String {
    s.replace('(', r"\(").replace(')', r"\)")
}

fn solve(data: &Data, regex: &str) -> Result<R, String> {
    let regex = Regex::new(regex).map_err(|err| format!("Error compiling regex: {err}"))?;

    let mul = Regex::new(MUL).map_err(|err| format!("Error compiling regex: {err}"))?;

    let mut active = true;
    let mut result = 0;

    for s in regex.find_iter(data).map(|m| m.as_str()) {
        if s == DO {
            active = true;
        } else if s == DONT {
            active = false;
        } else if let Some(captures) = mul.captures(s) {
            if active {
                let c_a = captures.name("a").ok_or(format!("Invalid capture: {s}"))?;
                let a = c_a
                    .as_str()
                    .parse::<usize>()
                    .map_err(|err| format!("Failed to parse: {err}"))?;
                let c_b = captures.name("b").ok_or(format!("Invalid capture: {s}"))?;
                let b = c_b
                    .as_str()
                    .parse::<usize>()
                    .map_err(|err| format!("Failed to parse: {err}"))?;
                result += a * b;
            }
        } else {
            return Err(format!("Invalid operation: {s}"));
        }
    }

    Ok(result)
}

fn solve_1(data: &Data) -> Result<R, String> {
    solve(data, MUL)
}

fn solve_2(data: &Data) -> Result<R, String> {
    let regex = [MUL.to_string(), to_regex(DO), to_regex(DONT)].join("|");
    solve(data, regex.as_str())
}

fn main() -> Result<(), String> {
    let result_1 = solve_1(&DATA)?;
    println!("Part 1: {result_1}");

    let result_2 = solve_2(&DATA)?;
    println!("Part 2: {result_2}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA_0: &str = include_str!("../../resources/03-test-00.txt");
    const TEST_DATA_1: &str = include_str!("../../resources/03-test-01.txt");

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(&TEST_DATA_0), Ok(2 * 4 + 5 * 5 + 11 * 8 + 8 * 5));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(&DATA), Ok(187_825_547));
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve_2(&TEST_DATA_1), Ok(2 * 4 + 8 * 5));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(&DATA), Ok(85_508_223));
    }
}
