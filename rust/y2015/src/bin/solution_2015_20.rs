use advent_of_code_common::math::factors;

fn solve<F>(input: u32, f: F) -> u32
where
    F: Fn(u32) -> u32,
{
    for i in 1 ..= u32::MAX {
        let result = f(i);

        if i % 100_000 == 0 {
            println!("presents_for_house({i}) == {result}");
        }

        if result >= input {
            return i;
        }
    }

    panic!("Not found!")
}

fn presents_for_house_1(house: u32) -> u32 {
    factors(house).iter().sum::<u32>() * 10
}

fn solve_1(input: u32) -> u32 {
    solve(input, presents_for_house_1)
}

const PRESENTS_PER_ELF: u32 = 50;
fn presents_for_house_2(house: u32) -> u32 {
    factors(house)
        .iter()
        .filter(|&&n| (house / n) <= PRESENTS_PER_ELF)
        .sum::<u32>()
        * 11
}

fn solve_2(input: u32) -> u32 {
    solve(input, presents_for_house_2)
}

const DATA: u32 = 36_000_000;
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
        assert_eq!(presents_for_house_1(1), 10); // #1
    }

    #[test]
    fn test_solve_1_test_2() {
        assert_eq!(presents_for_house_1(2), 30); // #1, #2
    }

    #[test]
    fn test_solve_1_test_3() {
        assert_eq!(presents_for_house_1(3), 40); // #1, #3
    }

    #[test]
    fn test_solve_1_test_4() {
        assert_eq!(presents_for_house_1(4), 70); // #1, #2, #4
    }

    #[test]
    fn test_solve_1_test_5() {
        assert_eq!(presents_for_house_1(5), 60); // #1, #5
    }

    #[test]
    fn test_solve_1_test_6() {
        assert_eq!(presents_for_house_1(6), 120); // #1, #2, #3, #6
    }

    #[test]
    fn test_solve_1_test_7() {
        assert_eq!(presents_for_house_1(7), 80); // #1, #7
    }

    #[test]
    fn test_solve_1_test_8() {
        assert_eq!(presents_for_house_1(8), 150); // #1, #2, #4, #8
    }

    #[test]
    fn test_solve_1_test_9() {
        assert_eq!(presents_for_house_1(9), 130); // #1, #3, #9
    }

    #[test]
    #[ignore]
    fn test_solve_1_real() {
        assert_eq!(solve_1(DATA), 831600);
    }

    #[test]
    fn test_solve_2_test_51() {
        assert_eq!(presents_for_house_2(51), (3 + 17 + 51) * 11); // #1, #3, #9
    }

    #[test]
    #[ignore]
    fn test_solve_2_real() {
        assert_eq!(solve_2(DATA), 884520);
    }
}
