use std::collections::HashSet;

use advent_of_code_2017::knot_hash::knot_hash_as_u8;
use advent_of_code_common::coords2d::Coords2D;
use pathfinding::prelude::connected_components;

type Coords = Coords2D<i32>;

fn bytes(input: &str) -> Vec<Vec<u8>> {
    (0u8 .. 128)
        .map(|x| {
            let s = format!("{input}-{x}");
            knot_hash_as_u8(&s)
        })
        .collect()
}

fn part_1(input: &str) -> u32 {
    let b = bytes(input);
    b.iter()
        .map(|r| r.iter().map(|x| x.count_ones()).sum::<u32>())
        .sum()
}

fn part_2(input: &str) -> usize {
    let b = bytes(input);

    let mut graph: HashSet<Coords> = HashSet::new();

    for (y, r) in b.iter().enumerate() {
        for (idx, c) in r.iter().enumerate() {
            for bit_idx in 0usize .. 8 {
                let is_set = c & (1 << bit_idx) > 0;
                if is_set {
                    let x = idx * 8 + (8 - bit_idx);
                    let coords = Coords::new(i32::try_from(x).unwrap(), i32::try_from(y).unwrap());
                    graph.insert(coords);
                }
            }
        }
    }

    let starts: Vec<Coords> = graph.iter().copied().collect::<Vec<_>>();
    connected_components(&starts, |x| {
        x.adjacent4()
            .into_iter()
            .filter(|n| graph.contains(n))
            .collect::<Vec<_>>()
    })
    .len()
}

const DATA: &str = "xlqgujun";

fn main() {
    let result_1 = part_1(DATA);
    println!("Part 1: {result_1}");

    let result_2 = part_2(DATA);
    println!("Part 2: {result_2}");
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST: &str = "flqrgnkx";

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), 8204);
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(part_2(TEST), 1242);
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), 1089);
    }
}
