use std::collections::BTreeMap;

fn solve(numbers: &[u64], target: usize) -> u64 {
    let mut number_positions: BTreeMap<u64, u64> = BTreeMap::new();

    for (i, n) in numbers.iter().enumerate().take(numbers.len() - 1) {
        number_positions.insert(*n, i as u64);
    }

    let mut current: u64 = *numbers.last().unwrap();

    for current_idx in (numbers.len() as u64 - 1)..(target as u64 - 1) {
        if current_idx % 100_000 == 0 {
            println!(
                "Processing index {}, size of map is {}",
                current_idx,
                number_positions.len()
            )
        }

        let previous: Option<u64> = number_positions.insert(current, current_idx);
        current = previous.map_or(0, |x| current_idx - x);
    }

    current
}

fn main() {
    assert_eq!(solve(&[0, 3, 6], 4), 0);
    assert_eq!(solve(&[0, 3, 6], 5), 3);
    assert_eq!(solve(&[0, 3, 6], 6), 3);
    assert_eq!(solve(&[0, 3, 6], 7), 1);
    assert_eq!(solve(&[0, 3, 6], 8), 0);
    assert_eq!(solve(&[0, 3, 6], 9), 4);
    assert_eq!(solve(&[0, 3, 6], 10), 0);

    assert_eq!(solve(&[0, 3, 6], 2020), 436);
    assert_eq!(solve(&[1, 3, 2], 2020), 1);
    assert_eq!(solve(&[2, 1, 3], 2020), 10);
    assert_eq!(solve(&[1, 2, 3], 2020), 27);
    assert_eq!(solve(&[2, 3, 1], 2020), 78);
    assert_eq!(solve(&[3, 2, 1], 2020), 438);
    assert_eq!(solve(&[3, 1, 2], 2020), 1836);

    let solved_2020 = solve(&[0, 5, 4, 1, 10, 14, 7], 2020);
    println!("{solved_2020}");
    assert_eq!(solved_2020, 203);

    let solved_30m = solve(&[0, 5, 4, 1, 10, 14, 7], 30_000_000);
    println!("{solved_30m}");
    assert_eq!(solved_30m, 9_007_186);
}
