use advent_of_code_2017::knot_hash::{knot_hash_as_string, knot_hash_make_folds};
use advent_of_code_common::parsing::parse_str;

fn part_1(max_element: u8, fold_lengths: &[u8]) -> u16 {
    let mut data = (0 ..= max_element).collect();
    knot_hash_make_folds(&mut data, fold_lengths, 1);
    u16::from(data[0]) * u16::from(data[1])
}

const DATA: &str = "34,88,2,222,254,93,150,0,199,255,39,32,137,136,1,167";

fn part_1_real_data() -> Vec<u8> {
    DATA.split(',').map(|x| parse_str(x).unwrap()).collect()
}

fn part_2(input: &str) -> String {
    knot_hash_as_string(input)
}

fn main() {
    let result_1 = part_1(255, &part_1_real_data());
    println!("Part 1: {result_1}");

    let result_2 = part_2(DATA);
    println!("Part 2: {result_2}");
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST: [u8; 4] = [3, 4, 1, 5];

    #[test]
    fn test_solve_1_test() {
        assert_eq!(part_1(4, &TEST), 3 * 4);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(255, &part_1_real_data()), 225 * 243);
    }

    #[test]
    fn test_solve_2_test_0() {
        assert_eq!(part_2(""), "a2582a3a0e66e6e86e3812dcb672a272");
    }

    #[test]
    fn test_solve_2_test_1() {
        assert_eq!(part_2("AoC 2017"), "33efeb34ea91902bb2f59c9920caa6cd");
    }

    #[test]
    fn test_solve_2_test_2() {
        assert_eq!(part_2("1,2,3"), "3efbe78a8d82f29979031a4aa0b16a9d");
    }

    #[test]
    fn test_solve_2_test_3() {
        assert_eq!(part_2("1,2,4"), "63960835bcdc130f0b66d7ff4f6a5a8e");
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), "a7af2706aa9a09cf5d848c1e6605dd2a");
    }
}
