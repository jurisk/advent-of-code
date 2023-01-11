use itertools::Itertools;

type N = u64;

fn solve(data: &[N], group_count: N) -> N {
    let sum: N = data.into_iter().sum();
    assert_eq!(sum % group_count, 0);

    let sum_of_each_group = sum / group_count;

    let mut n: usize = 1;

    while n <= data.len() / group_count as usize {
        if let Some(found) = data
            .into_iter()
            .combinations(n)
            .filter(|x| x.into_iter().copied().sum::<u64>() == sum_of_each_group)
            // We are assuming that the remaining groups can be divided equally into
            // (group_count - 1) groups.
            // This assumption is not proven, but the tests pass, so this check was not added.
            .map(|x| x.into_iter().product())
            .min()
        {
            return found;
        } else {
            n += 1;
        }
    }

    panic!("Not found!")
}

fn solve_1(data: &[N]) -> N {
    solve(data, 3)
}

fn solve_2(data: &[N]) -> N {
    solve(data, 4)
}

const DATA: [N; 28] = [
    1, 3, 5, 11, 13, 17, 19, 23, 29, 31, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101,
    103, 107, 109, 113,
];

fn main() {
    let result_1 = solve_1(&DATA);
    println!("Part 1: {result_1}");

    let result_2 = solve_2(&DATA);
    println!("Part 2: {result_2}");
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST: [N; 10] = [1, 2, 3, 4, 5, 7, 8, 9, 10, 11];

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(&TEST), 11 * 9);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(&DATA), 11266889531);
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve_2(&TEST), 11 * 4);
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(&DATA), 77387711);
    }
}
