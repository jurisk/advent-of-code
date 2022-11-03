use itertools::Itertools;
use std::collections::HashMap;
use std::num::ParseIntError;

type Error = String;

const DATA: &str = include_str!("../../resources/06.txt");

type FishState = u8;
type FishCount = HashMap<FishState, usize>;

fn parse(data: &str) -> Result<FishCount, Error> {
    let vec: Result<Vec<FishState>, ParseIntError> =
        data.trim().split(',').map(str::parse).collect();
    match vec {
        Ok(vec) => {
            let result: HashMap<FishState, Vec<&FishState>> = vec.iter().into_group_map_by(|x| **x);
            Ok(result.iter().map(|(k, v)| (*k, v.len())).collect())
        },
        Err(e) => Err(format!("{}", e)),
    }
}

/*
   0 comes from 1
   1 comes from 2
   2 comes from 3
   3 comes from 4
   4 comes from 5
   5 comes from 6
   6 comes from 7 and 0
   7 comes from 8
   8 comes from 0
*/
fn next_day(fishes: &FishCount) -> FishCount {
    (0..=8)
        .map(|k| {
            let new_v = if k == 6 {
                fishes.get(&0).unwrap_or(&0) + fishes.get(&7).unwrap_or(&0)
            } else if k == 8 {
                *fishes.get(&0).unwrap_or(&0)
            } else {
                *fishes.get(&(k + 1)).unwrap_or(&0)
            };

            (k, new_v)
        })
        .collect()
}

fn fish_count(fishes: &FishCount) -> usize {
    fishes.values().sum()
}

fn solve(data: &str, days: u32) -> Result<usize, Error> {
    let mut fishes = parse(data)?;
    for idx in 0..days {
        println!("{} {} {:?}", idx, fish_count(&fishes), fishes);
        fishes = next_day(&fishes);
    }
    println!("{} {} {:?}", days, fish_count(&fishes), fishes);

    Ok(fish_count(&fishes))
}

fn main() {
    let result_1 = solve(DATA, 80);
    println!("Part 1: {:?}", result_1);

    let result_2 = solve(DATA, 256);
    println!("Part 2: {:?}", result_2);
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = "3,4,3,1,2";

    #[test]
    fn test_solve_1_test_1() {
        assert_eq!(solve(TEST_DATA, 18), Ok(26));
    }

    #[test]
    fn test_solve_1_test_2() {
        assert_eq!(solve(TEST_DATA, 80), Ok(5934));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve(DATA, 80), Ok(362_740));
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve(TEST_DATA, 256), Ok(26_984_457_539));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve(DATA, 256), Ok(1_644_874_076_764));
    }
}
