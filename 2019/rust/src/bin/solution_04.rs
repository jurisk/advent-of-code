use itertools::Itertools;
use std::ops::RangeInclusive;

type Number = u32;

fn to_digits(x: Number) -> Vec<u8> {
    x.to_string()
        .chars()
        .map(|x| x.to_string().parse::<u8>().unwrap())
        .collect()
}

fn never_decrease(digits: &[u8]) -> bool {
    let mut copy = digits.to_owned();
    copy.sort_unstable(); // not the most efficient, but really simple
    copy == *digits
}

fn group(digits: &[u8]) -> Vec<u8> {
    digits
        .iter()
        .into_group_map_by(|x| *x)
        .iter()
        .map(|(_k, v)| v.len() as u8)
        .collect()
}

fn is_valid(x: Number, f: fn(Vec<u8>) -> bool) -> bool {
    let digits = to_digits(x);
    f(group(&digits)) && never_decrease(&digits)
}

fn is_valid_1(x: Number) -> bool {
    is_valid(x, |grouped| grouped.iter().any(|x| *x >= 2))
}

fn is_valid_2(x: Number) -> bool {
    is_valid(x, |grouped| grouped.contains(&2))
}

fn main() {
    assert!(is_valid_1(111_111));
    assert!(!is_valid_1(223_450));
    assert!(!is_valid_1(123_789));

    let range: RangeInclusive<Number> = 387_638..=919_123;
    let part_1 = range.clone().filter(|x| is_valid_1(*x)).count();
    assert_eq!(part_1, 466);
    println!("Part 1: {}", part_1);

    assert!(is_valid_2(112_233));
    assert!(!is_valid_2(123_444));
    assert!(is_valid_2(111_122));

    let part_2 = range.filter(|x| is_valid_2(*x)).count();
    assert_eq!(part_2, 292);
    println!("Part 2: {}", part_2);
}
