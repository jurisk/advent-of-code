fn md5_for_string(input: &str) -> String {
    let digest = md5::compute(input);
    format!("{digest:x}")
}

fn solve_1(secret_key: &str) -> u32 {
    for n in 0..u32::MAX {
        let input = format!("{secret_key}{n}");
        let md5 = md5_for_string(&input);
        if md5.starts_with("00000") {
            return n;
        }
    }

    panic!("Not found!")
}

fn solve_2(secret_key: &str) -> u32 {
    for n in 0..u32::MAX {
        let input = format!("{secret_key}{n}");
        let md5 = md5_for_string(&input);
        if md5.starts_with("000000") {
            return n;
        }
    }

    panic!("Not found!")
}

const DATA: &str = "bgvyzdsv";

fn main() {
    let result_1 = solve_1(DATA);
    println!("Part 1: {result_1}");

    let result_2 = solve_2(DATA);
    println!("Part 2: {result_2}");
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_solve_1_test_1() {
        assert_eq!(solve_1("abcdef"), 609043);
    }
    #[test]
    fn test_solve_1_test_2() {
        assert_eq!(solve_1("pqrstuv"), 1048970);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(DATA), 254575);
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(DATA), 1038736);
    }
}
