use advent_of_code_common::coords2d::Coords2D;
use std::collections::{HashSet, VecDeque};

type Coords = Coords2D<isize>;

struct HeightMap {
    data: Vec<Vec<u8>>,
}

impl HeightMap {
    const MAX_HEIGHT: u8 = 9;

    #[allow(clippy::cast_possible_truncation)]
    fn parse(input: &str) -> HeightMap {
        HeightMap {
            data: input
                .lines()
                .map(|line| {
                    line.chars()
                        .map(|c| c.to_digit(10).unwrap() as u8)
                        .collect()
                })
                .collect(),
        }
    }

    #[allow(clippy::cast_possible_wrap)]
    fn coordinates(&self) -> Vec<Coords> {
        self.data
            .iter()
            .enumerate()
            .flat_map(|(a, b)| {
                b.iter().enumerate().map(move |(c, _)| Coords {
                    x: c as isize,
                    y: a as isize,
                })
            })
            .collect()
    }

    fn is_low_point(&self, c: &Coords) -> bool {
        c.adjacent4()
            .iter()
            .all(|n| self.height(n) > self.height(c))
    }

    fn low_points(&self) -> Vec<Coords> {
        self.coordinates()
            .iter()
            .filter(|c| self.is_low_point(c))
            .copied()
            .collect()
    }

    #[allow(clippy::cast_sign_loss)]
    fn height(&self, coords: &Coords) -> u8 {
        if coords.x < 0 || coords.y < 0 {
            HeightMap::MAX_HEIGHT
        } else {
            *self
                .data
                .get(coords.y as usize)
                .and_then(|row| row.get(coords.x as usize))
                .unwrap_or(&HeightMap::MAX_HEIGHT)
        }
    }

    fn basin_size(&self, c: &Coords) -> usize {
        let mut visited: HashSet<Coords> = HashSet::new();
        let mut queue: VecDeque<Coords> = VecDeque::new();
        queue.push_back(*c);

        while !queue.is_empty() {
            let next = queue.pop_front().unwrap();
            let height_at_next = self.height(&next);
            if height_at_next < HeightMap::MAX_HEIGHT && !visited.contains(&next) {
                visited.insert(next);

                next.adjacent4().iter().for_each(|n| {
                    if !visited.contains(n) {
                        queue.push_back(*n);
                    }
                });
            }
        }

        visited.len()
    }
}

#[allow(clippy::cast_lossless)]
fn part_1(height_map: &HeightMap) -> u32 {
    height_map
        .low_points()
        .iter()
        .map(|c| 1 + height_map.height(c) as u32)
        .sum()
}

fn part_2(height_map: &HeightMap) -> usize {
    let mut basin_sizes: Vec<usize> = height_map
        .low_points()
        .iter()
        .map(|c| height_map.basin_size(c))
        .collect();

    basin_sizes.sort_unstable();

    basin_sizes.iter().rev().take(3).product()
}

#[allow(clippy::unreadable_literal)]
fn main() {
    let test_data = HeightMap::parse(
        "2199943210
3987894921
9856789892
8767896789
9899965678",
    );
    let test_result_1 = part_1(&test_data);
    assert_eq!(test_result_1, 15);

    let data = HeightMap::parse(include_str!("../../resources/09.txt"));
    let result_1 = part_1(&data);
    println!("Part 1: {result_1}");
    assert_eq!(result_1, 545);

    let test_result_2 = part_2(&test_data);
    assert_eq!(test_result_2, 1134);

    let result_2 = part_2(&data);
    println!("Part 2: {result_2}");
    assert_eq!(result_2, 950600);
}
