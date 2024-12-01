use advent_of_code_common::cryptography::md5_for_string;

fn solve<F>(secret_key: &str, p: F) -> Option<u32>
where
    F: Fn(&str) -> bool,
{
    (0 .. u32::MAX).find(|n| {
        let input = format!("{secret_key}{n}");
        let md5 = md5_for_string(&input);
        p(&md5)
    })
}

fn solve_1(secret_key: &str) -> Option<u32> {
    solve(secret_key, |md5| md5.starts_with("00000"))
}

fn solve_2(secret_key: &str) -> Option<u32> {
    solve(secret_key, |md5| md5.starts_with("000000"))
}

const DATA: &str = "bgvyzdsv";

fn main() {
    let result_1 = solve_1(DATA);
    println!("Part 1: {result_1:?}");

    let result_2 = solve_2(DATA);
    println!("Part 2: {result_2:?}");
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_solve_1_test_1() {
        assert_eq!(solve_1("abcdef"), Some(609_043));
    }

    #[test]
    #[ignore]
    fn test_solve_1_test_2() {
        assert_eq!(solve_1("pqrstuv"), Some(1_048_970));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(DATA), Some(254_575));
    }

    #[test]
    #[ignore]
    fn test_solve_2_real() {
        assert_eq!(solve_2(DATA), Some(1_038_736));
    }
}
