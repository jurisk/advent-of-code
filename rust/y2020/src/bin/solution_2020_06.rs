use std::collections::HashSet;

const DATA: &str = include_str!("../../resources/06.txt");

type R = usize;
type Data = Vec<Group>;

type Answer = char;
type Form = HashSet<Answer>;
type Group = Vec<Form>;

fn parse_multi_line(input: &str) -> Vec<Vec<&str>> {
    input
        .split("\n\n")
        .map(|x| x.lines().collect::<Vec<_>>())
        .collect::<Vec<_>>()
}

type MergeFunction<T> = fn(HashSet<T>, other: HashSet<T>) -> HashSet<T>;

fn parse(input: &str) -> Data {
    parse_multi_line(input)
        .into_iter()
        .map(|x| {
            x.into_iter()
                .map(|s| s.chars().collect::<HashSet<Answer>>())
                .collect()
        })
        .collect()
}

#[expect(clippy::ptr_arg)]
fn merge_group(group: &Vec<HashSet<Answer>>, f: MergeFunction<Answer>) -> HashSet<Answer> {
    group.clone().into_iter().reduce(f).unwrap_or_default()
}

fn solve(groups: &[Group], f: MergeFunction<Answer>) -> usize {
    groups.iter().map(|g| merge_group(g, f).len()).sum()
}

fn solve_1(data: &Data) -> R {
    let union: MergeFunction<Answer> = |a: HashSet<Answer>, b: HashSet<Answer>| {
        HashSet::union(&a, &b).copied().collect::<HashSet<Answer>>()
    };
    solve(data, union)
}

fn solve_2(data: &Data) -> R {
    let intersection: MergeFunction<Answer> = |a: HashSet<Answer>, b: HashSet<Answer>| {
        HashSet::intersection(&a, &b)
            .copied()
            .collect::<HashSet<Answer>>()
    };
    solve(data, intersection)
}

fn main() {
    let data = parse(DATA);

    let result_1 = solve_1(&data);
    println!("Part 1: {result_1}");

    let result_2 = solve_2(&data);
    println!("Part 2: {result_2}");
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = include_str!("../../resources/06-test.txt");

    fn test_data() -> Data {
        parse(TEST_DATA)
    }

    fn real_data() -> Data {
        parse(DATA)
    }

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(&test_data()), 11);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(&real_data()), 6351);
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve_2(&test_data()), 6);
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(&real_data()), 3143);
    }
}
