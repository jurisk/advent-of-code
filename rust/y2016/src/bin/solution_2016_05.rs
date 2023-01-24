use advent_of_code_common::cryptography::md5_for_string;

fn solve_1(door_id: &str) -> String {
    let mut result: String = String::new();
    let mut n: u32 = 0;
    while result.len() < 8 {
        let s = format!("{door_id}{n}");
        let hash = md5_for_string(&s);
        if &hash[..5] == "00000" {
            let picked_char = hash
                .chars()
                .nth(5)
                .unwrap_or_else(|| panic!("Hash is too short: {hash}"));
            result.push(picked_char);
            println!("Hash of {s} is {hash}, result is {result}");
        }
        n += 1;
    }
    result
}

fn solve_2(door_id: &str) -> String {
    const PLACEHOLDER: char = '_';
    const REQUIRED_PREFIX: &str = "00000";

    let mut result: String = String::from_utf8(vec![PLACEHOLDER as u8; 8])
        .unwrap_or_else(|_| panic!("Failed to create empty string"));
    let mut n: u32 = 0;

    loop {
        let s = format!("{door_id}{n}");
        let hash = md5_for_string(&s);

        if hash.starts_with(REQUIRED_PREFIX) {
            let position_char = hash
                .chars()
                .nth(5)
                .unwrap_or_else(|| panic!("Hash is too short: {hash}"));

            let picked_char = hash
                .chars()
                .nth(6)
                .unwrap_or_else(|| panic!("Hash is too short: {hash}"));

            let position: usize = (position_char as u8 - b'0') as usize;
            if position < result.len() {
                if result
                    .chars()
                    .nth(position)
                    .unwrap_or_else(|| panic!("Result too short: {result}"))
                    == PLACEHOLDER
                {
                    result.replace_range(position..=position, &picked_char.to_string());
                    println!("{result}: Updated character - hash of {s} is {hash}, position char is {position_char}, picked char is {picked_char}");
                } else {
                    println!("{result}: Ignoring duplicate update - hash of {s} is {hash}");
                }
            } else {
                println!("{result}: Ignoring invalid position - hash of {s} is {hash}");
            }

            if !result.contains(PLACEHOLDER) {
                return result;
            }
        }
        n += 1;
    }
}

const DATA: &str = "uqwqemis";

fn main() {
    solve_2("abc");

    let result_1 = solve_1(DATA);
    println!("Part 1: {result_1}");

    let result_2 = solve_2(DATA);
    println!("Part 2: {result_2}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore]
    fn test_solve_1_test() {
        assert_eq!(solve_1("abc"), "18f47a30");
    }

    #[test]
    #[ignore]
    fn test_solve_1_real() {
        assert_eq!(solve_1(DATA), "1a3099aa");
    }

    #[test]
    #[ignore]
    fn test_solve_2_test() {
        assert_eq!(solve_1("abc"), "05ace8e3");
    }

    #[test]
    #[ignore]
    fn test_solve_2_real() {
        assert_eq!(solve_2(DATA), "694190cd");
    }
}
