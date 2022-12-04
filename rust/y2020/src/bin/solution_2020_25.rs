#[inline]
fn transform(number: u64, subject_number: u64) -> u64 {
    const N: u64 = 20_201_227;
    (number * subject_number) % N
}

fn determine_loop_size(subject_number: u64, target: u64) -> u64 {
    let mut current = 1;
    let mut loops = 0;
    while current != target {
        loops += 1;
        current = transform(current, subject_number);
    }
    loops
}

fn do_loops(number: u64, loops: u64) -> u64 {
    let mut current = number;
    for _ in 1..loops {
        current = transform(current, number);
    }
    current
}

fn solve(cards_public_key: u64, doors_public_key: u64) -> u64 {
    let card_loops = determine_loop_size(7, cards_public_key);
    let door_loops = determine_loop_size(7, doors_public_key);
    let result1 = do_loops(cards_public_key, door_loops);
    let result2 = do_loops(doors_public_key, card_loops);
    assert_eq!(result1, result2);
    result1
}

fn main() {
    assert_eq!(solve(5_764_801, 17_807_724), 14_897_079);
    let solution = solve(13_316_116, 13_651_422);
    assert_eq!(solution, 12929);
    println!("{solution}");
}
