use std::collections::HashMap;

use advent_of_code_common::parsing::{Error, parse_lines_to_vec};

const DATA: &str = include_str!("../../resources/22.txt");

type N = u64;
type Diff = i8;
type Input = Vec<N>;
const SEQUENCE_LENGTH: usize = 4;
type Sequence = [Diff; SEQUENCE_LENGTH];

fn parse(input: &str) -> Result<Input, Error> {
    parse_lines_to_vec(input)
}

fn next(n: N) -> N {
    fn mix_prune(a: N, b: N) -> N {
        (a ^ b) % 16_777_216
    }

    let a = mix_prune(n * 64, n);
    let b = mix_prune(a / 32, a);
    mix_prune(b * 2048, b)
}

fn nth_secret_number(n: N, n_th: usize) -> N {
    if n_th == 0 {
        n
    } else {
        nth_secret_number(next(n), n_th - 1)
    }
}

fn create_bananas_and_diffs(n: N, how_many: usize) -> (Vec<N>, Vec<Diff>) {
    let mut bananas = Vec::with_capacity(how_many + 1);

    bananas.push(n);

    for i in 0 .. how_many {
        bananas.push(next(bananas[i]));
    }

    let mut diffs = Vec::with_capacity(how_many);
    for i in 0 .. how_many {
        diffs.push((bananas[i + 1] % 10) as Diff - (bananas[i] % 10) as Diff);
    }

    bananas.iter_mut().for_each(|b| *b %= 10);

    (bananas, diffs)
}

fn create_banana_map(n: N) -> HashMap<Sequence, N> {
    let (bananas, all_diffs) = create_bananas_and_diffs(n, 2000);

    let mut results = HashMap::new();

    for i in 0 .. (all_diffs.len() - SEQUENCE_LENGTH) {
        let slice = &all_diffs[i .. (i + SEQUENCE_LENGTH)];
        let diffs: Sequence = slice.try_into().expect("Slice with incorrect length");
        let result = bananas[i + SEQUENCE_LENGTH];
        results.entry(diffs).or_insert(result);
    }

    results
}

fn solve_1(data: &Input) -> N {
    data.iter().map(|n| nth_secret_number(*n, 2000)).sum()
}

fn solve_2(data: &Input) -> N {
    let mut results: HashMap<Sequence, N> = HashMap::new();
    for n in data {
        let banana_map = create_banana_map(*n);
        for (k, v) in banana_map {
            let new_value = results.entry(k).or_insert(0);
            *new_value += v;
        }
    }
    *results.values().max().expect("No max value")
}

fn main() -> Result<(), Error> {
    let data = parse(DATA)?;

    let result_1 = solve_1(&data);
    println!("Part 1: {result_1}");

    let result_2 = solve_2(&data);
    println!("Part 2: {result_2}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA_0: &str = include_str!("../../resources/22-test-00.txt");
    const TEST_DATA_1: &str = include_str!("../../resources/22-test-01.txt");

    fn test_data_0() -> Input {
        parse(TEST_DATA_0).unwrap()
    }
    fn test_data_1() -> Input {
        parse(TEST_DATA_1).unwrap()
    }

    fn real_data() -> Input {
        parse(DATA).unwrap()
    }

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(&test_data_0()), 37_327_623);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(&real_data()), 13_022_553_808);
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve_2(&test_data_1()), 7 + 7 + 9);
    }

    #[test]
    #[ignore = "Slow"]
    fn test_solve_2_real() {
        assert_eq!(solve_2(&real_data()), 1555);
    }
}
