use num_traits::pow;
use std::collections::{HashMap, HashSet};

const SIZE: usize = 5;

type Field = [[bool; SIZE]; SIZE];

fn parse(data: &str) -> Field {
    data.split('\n')
        .filter(|x| !x.is_empty())
        .map(|r| {
            r.chars()
                .map(|ch| ch == '#')
                .collect::<Vec<_>>()
                .try_into()
                .unwrap()
        })
        .collect::<Vec<_>>()
        .try_into()
        .unwrap()
}

const DATA: &str = include_str!("../../resources/24.txt");

fn at(field: &Field, x: i32, y: i32) -> bool {
    if x < 0 || x as usize >= SIZE || y < 0 || y as usize >= SIZE {
        false
    } else {
        field[y as usize][x as usize]
    }
}

fn neighbour_count_simple(field: Field, x: usize, y: usize) -> usize {
    vec![(-1, 0), (1, 0), (0, -1), (0, 1)]
        .iter()
        .filter(|(dx, dy)| {
            let nx = (x as i32) + dx;
            let ny = (y as i32) + dy;
            at(&field, nx, ny)
        })
        .count()
}

fn empty_field() -> Field {
    [[false; SIZE]; SIZE]
}

fn next_simple(field: Field) -> Field {
    let mut result: Field = empty_field();

    for x in 0..SIZE {
        for y in 0..SIZE {
            let neighbours = neighbour_count_simple(field, x, y);

            result[y][x] = {
                if field[y][x] {
                    // A bug dies (becoming an empty space) unless there is exactly one bug adjacent to it
                    neighbours == 1
                } else {
                    // An empty space becomes infested with a bug if exactly one or two bugs are adjacent to it.
                    neighbours == 1 || neighbours == 2
                }
            }
        }
    }

    result
}

fn find_loop_simple(field: Field) -> Field {
    let mut visited: HashSet<Field> = HashSet::from([field]);
    let mut current = field;
    loop {
        let obtained = next_simple(current);
        let inserted = visited.insert(obtained);
        if !inserted {
            return obtained;
        }
        current = obtained;
    }
}

fn biodiversity(field: Field) -> u128 {
    let mut result: u128 = 0;
    for x in 0..SIZE {
        for y in 0..SIZE {
            if field[y][x] {
                result += pow(2, x + y * SIZE);
            }
        }
    }

    result
}

fn solve_1(data: &str) -> u128 {
    let parsed = parse(data);
    let looped = find_loop_simple(parsed);
    biodiversity(looped)
}

fn new_level(data: &HashMap<i32, Field>, level: i32) -> Field {
    let lesser: Field = *data.get(&(level - 1)).unwrap_or(&empty_field());
    let current: Field = *data.get(&level).unwrap_or(&empty_field());
    let greater: Field = *data.get(&(level + 1)).unwrap_or(&empty_field());
    let mut result: Field = empty_field();

    for x in 0..SIZE {
        for y in 0..SIZE {
            if x == 2 && y == 2 {
                continue;
            }

            let neighbours = neighbour_count_complex(lesser, current, greater, x, y);

            result[y][x] = {
                if current[y][x] {
                    // A bug dies (becoming an empty space) unless there is exactly one bug adjacent to it
                    neighbours == 1
                } else {
                    // An empty space becomes infested with a bug if exactly one or two bugs are adjacent to it.
                    neighbours == 1 || neighbours == 2
                }
            }
        }
    }

    result
}

fn bool_count(vec: Vec<bool>) -> usize {
    vec.iter().filter(|x| **x).count()
}

fn neighbour_count_complex(
    inner: Field,
    current: Field,
    outer: Field,
    x: usize,
    y: usize,
) -> usize {
    let current_level_neighbours: usize = neighbour_count_simple(current, x, y);
    let from_inner = match (x, y) {
        (1, 2) => bool_count((0..SIZE).map(|idx| inner[idx][0]).collect()), // L
        (2, 1) => bool_count((0..SIZE).map(|idx| inner[0][idx]).collect()), // H
        (3, 2) => bool_count((0..SIZE).map(|idx| inner[idx][4]).collect()), // N
        (2, 3) => bool_count((0..SIZE).map(|idx| inner[4][idx]).collect()), // R
        (_, _) => 0,
    };

    let from_1_2 = x == 0 && outer[2][1]; // add 12
    let from_3_2 = x == 4 && outer[2][3]; // add 14
    let from_2_1 = y == 0 && outer[1][2]; // add 8
    let from_2_3 = y == 4 && outer[3][2]; // add 18
    let from_outer = vec![from_1_2, from_3_2, from_2_1, from_2_3];

    current_level_neighbours + from_inner + bool_count(from_outer)
}

fn next_complex(data: &HashMap<i32, Field>) -> HashMap<i32, Field> {
    let keys: Vec<i32> = data.keys().copied().collect();
    let smallest = keys.iter().min().unwrap();
    let largest = keys.iter().max().unwrap();
    (*smallest - 1..=*largest + 1)
        .map(|level| (level, new_level(data, level)))
        .collect()
}

fn count_bugs_complex(field: Field) -> u128 {
    let mut result = 0;
    for x in 0..SIZE {
        for y in 0..SIZE {
            if !(x == 2 && y == 2) {
                result += u128::from(field[y][x]);
            }
        }
    }
    result
}

fn solve_2(data: &str, minutes: u8) -> u128 {
    let mut data: HashMap<i32, Field> = HashMap::from([(0, parse(data))]);
    for _ in 0..minutes {
        data = next_complex(&data);
    }
    data.values().map(|f| count_bugs_complex(*f)).sum()
}

fn main() {
    let result = solve_1(DATA);
    println!("Part 1: {result}");

    let result = solve_2(DATA, 200);
    println!("Part 2: {result}");
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = "....#
#..#.
#..##
..#..
#....";

    #[test]
    fn test_biodiversity() {
        let test_data = ".....
.....
.....
#....
.#...";

        assert_eq!(biodiversity(parse(test_data)), 2_129_920);
    }

    #[test]
    fn test_solve_1_1() {
        let result = solve_1(TEST_DATA);
        assert_eq!(result, 2_129_920);
    }

    #[test]
    fn test_solve_1_2() {
        let result = solve_1(DATA);
        assert_eq!(result, 32_506_911);
    }

    #[test]
    fn test_solve_2_1() {
        let result = solve_2(TEST_DATA, 10);
        assert_eq!(result, 99);
    }

    #[test]
    fn test_solve_2_2() {
        let result = solve_2(DATA, 200);
        assert_eq!(result, 2025);
    }
}
