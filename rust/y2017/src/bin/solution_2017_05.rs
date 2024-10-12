use advent_of_code_common::parsing::{Error, parse_lines_to_vec};

type Data = Vec<i32>;

fn parse(input: &str) -> Result<Data, Error> {
    parse_lines_to_vec(input)
}

fn solve<F>(data: &Data, f: F) -> usize
where
    F: Fn(i32) -> i32,
{
    let mut program = data.clone();
    let mut ip: isize = 0;
    let mut steps: usize = 0;

    loop {
        let ip_usize = usize::try_from(ip).unwrap();
        if let Some(&current_jmp) = program.get(ip_usize) {
            let new_ip = ip + current_jmp as isize;
            program[ip_usize] = current_jmp + f(current_jmp);
            steps += 1;
            ip = new_ip;
        } else {
            return steps;
        }
    }
}

fn solve_1(data: &Data) -> usize {
    solve(data, |_| 1)
}

fn solve_2(data: &Data) -> usize {
    solve(data, |x| if x >= 3 { -1 } else { 1 })
}

fn part_1(input: &str) -> Result<usize, Error> {
    parse(input).map(|input| solve_1(&input))
}

fn part_2(input: &str) -> Result<usize, Error> {
    parse(input).map(|input| solve_2(&input))
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

    const TEST: &str = include_str!("../../resources/05-test.txt");

    #[test]
    fn test_solve_1_test() {
        assert_eq!(part_1(TEST), Ok(5));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(373160));
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(part_2(TEST), Ok(10));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(26395586));
    }
}
