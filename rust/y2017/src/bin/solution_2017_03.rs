use std::collections::HashMap;

use advent_of_code_common::coords2d::Coords2D;
use integer_sqrt::IntegerSquareRoot;

type Coords = Coords2D<i32>;

#[allow(clippy::cast_possible_wrap)]
fn location_1(n: u32) -> Coords {
    let sqrt = n.integer_sqrt();
    if sqrt * sqrt == n && sqrt % 2 == 1 {
        Coords::new((sqrt / 2) as i32, (sqrt / 2) as i32)
    } else {
        let s = if sqrt.is_multiple_of(2) {
            sqrt - 1
        } else {
            sqrt
        };
        let extra = (n - s * s) as i32;

        let east_x = (s / 2 + 1) as i32;
        let west_x = -east_x;
        let south_y = (s / 2 + 1) as i32;
        let north_y = -south_y;

        let cut_1 = (s as i32) + 1;
        let cut_2 = cut_1 * 2;
        let cut_3 = cut_1 * 3;
        let cut_4 = cut_1 * 4;

        if extra <= cut_1 {
            // east
            let offset = extra;
            Coords2D::new(east_x, south_y - offset)
        } else if extra <= cut_2 {
            // north
            let offset = extra - cut_1;
            Coords2D::new(east_x - offset, north_y)
        } else if extra <= cut_3 {
            // west
            let offset = extra - cut_2;
            Coords2D::new(west_x, north_y + offset)
        } else if extra <= cut_4 {
            // south
            let offset = extra - cut_3;
            Coords2D::new(west_x + offset, south_y)
        } else {
            panic!("Unexpected {extra} {sqrt} {s}")
        }
    }
}

fn solve_1(n: u32) -> i32 {
    location_1(n).manhattan_distance_to_origin()
}

#[allow(clippy::redundant_else)]
fn solve_2(limit: u32) -> u32 {
    let mut values: HashMap<Coords, u32> = HashMap::new();
    values.insert(Coords2D::origin(), 1);
    for n in 2 ..= u32::MAX {
        let coords = location_1(n);
        let value = coords
            .adjacent8()
            .iter()
            .map(|c| values.get(c).unwrap_or(&0))
            .sum();
        if value > limit {
            return value;
        } else {
            println!("n == {n}: writing {value} to {coords:?}");
            values.insert(coords, value);
        }
    }

    panic!("Unexpected")
}

const DATA: u32 = 368_078;

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
    fn test_check_unique_locations() {
        let mut seen: HashMap<Coords, u32> = HashMap::new();
        for n in 1 ..= 4096 {
            let coords = location_1(n);
            if let Some(already_seen) = seen.get(&coords) {
                panic!(
                    "location({n}) == {coords:?} but it was already selected for location({already_seen})"
                );
            } else {
                seen.insert(coords, n);
            }
        }
    }

    #[test]
    fn test_solve_1_test_1() {
        assert_eq!(location_1(1), Coords2D::origin());
        assert_eq!(solve_1(1), 0);
    }

    #[test]
    fn test_solve_1_test_9() {
        assert_eq!(location_1(9), Coords2D::new(1, 1));
        assert_eq!(solve_1(9), 2);
    }

    #[test]
    fn test_solve_1_test_12() {
        assert_eq!(location_1(12), Coords2D::new(2, -1));
        assert_eq!(solve_1(12), 3);
    }

    #[test]
    fn test_solve_1_test_23() {
        assert_eq!(location_1(23), Coords2D::new(0, 2));
        assert_eq!(solve_1(23), 2);
    }

    #[test]
    fn test_solve_1_test_1024() {
        assert_eq!(solve_1(1024), 31);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(DATA), 371);
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(DATA), 369_601);
    }
}
