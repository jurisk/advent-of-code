use std::collections::HashSet;
use std::f32::consts::PI;
use std::ops::Sub;

#[derive(Debug, Eq, PartialEq)]
enum Cell {
    Empty,
    Asteroid,
}

type Number = i32;
type Angle = i32;

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
struct Coords {
    x: Number,
    y: Number,
}

impl Coords {
    fn new(x: Number, y: Number) -> Coords {
        Coords { x, y }
    }
}

impl Sub for Coords {
    type Output = Coords;

    fn sub(self, rhs: Self) -> Self::Output {
        Coords {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
        }
    }
}

fn parse(s: &str) -> Vec<Vec<Cell>> {
    s.lines()
        .filter(|x| !x.is_empty())
        .map(|row| {
            row.trim()
                .chars()
                .map(|ch| match ch {
                    '.' => Cell::Empty,
                    '#' => Cell::Asteroid,
                    _ => panic!("Unexpected cell: {ch}"),
                })
                .collect()
        })
        .collect()
}

fn asteroid_angle(from: Coords, to: Coords) -> Angle {
    const PI_2: f64 = 2f64 * PI as f64;
    let c = from - to;
    let result = f64::from(c.y).atan2(f64::from(c.x)) - (f64::from(PI) / 2f64);
    let res = if result < 0f64 { result + PI_2 } else { result };

    (res * 1_000_000_f64) as Angle
}

// Note - Using a sorted set (e.g. BTreeSet) would make a lot of operations on these angles better
fn asteroid_angles(from: Coords, asteroid_coordinates: &HashSet<Coords>) -> Vec<Angle> {
    asteroid_coordinates
        .iter()
        .filter(|&&x| x != from) // skip own
        .map(|to| asteroid_angle(from, *to))
        .collect()
}

fn count_visible(from: Coords, asteroid_coordinates: &HashSet<Coords>) -> Number {
    let set: HashSet<_> = asteroid_angles(from, asteroid_coordinates)
        .iter()
        .map(|x| format!("{x:.10}")) // A minor hack - but no hash function for f64 and also this solves rounding issues
        .collect();

    set.len() as Number
}

fn asteroid_coordinates(input: &str) -> HashSet<Coords> {
    let data = parse(input);
    (0..data.len())
        .flat_map(|y| {
            let row = &data[y];
            (0..row.len()).filter_map(move |x| {
                if row[x] == Cell::Asteroid {
                    Some(Coords {
                        x: x as Number,
                        y: y as Number,
                    })
                } else {
                    None
                }
            })
        })
        .collect()
}

fn best_placement(coords: &HashSet<Coords>) -> (Coords, Number) {
    let options: Vec<_> = coords
        .iter()
        .map(|c| (*c, count_visible(*c, coords)))
        .collect();

    *options.iter().max_by_key(|(_, num)| num).unwrap()
}

fn part_1(input: &str) -> (Coords, Number) {
    let coords = asteroid_coordinates(input);
    best_placement(&coords)
}

fn solve_1() {
    let (best_coords, detected) = part_1(include_str!("../../resources/10.txt"));
    assert_eq!(detected, 274);
    assert_eq!(best_coords, Coords { x: 19, y: 14 });
    println!("{best_coords:?} {detected}");
}

fn solve_2() {
    let result = part_2(include_str!("../../resources/10.txt"), 200);
    assert_eq!(result, Coords::new(3, 5));
    println!("{}", result.x * 100 + result.y);
}

fn distance(from: Coords, to: Coords) -> i32 {
    (from.x - to.x).pow(2) + (from.y - to.y).pow(2)
}

fn closest_at_angle(from: Coords, coords: &HashSet<Coords>, angle: Angle) -> Coords {
    let at_this_angle: Vec<&Coords> = coords
        .iter()
        .filter(|&&x| (x != from) && asteroid_angle(from, x) == angle)
        .collect();
    **at_this_angle
        .iter()
        .min_by_key(|&x| distance(from, **x))
        .unwrap()
}

fn next_destroyable(
    from: Coords,
    coords: &HashSet<Coords>,
    previous_angle: Angle,
) -> (Coords, Angle) {
    let angles = asteroid_angles(from, coords);
    let max = angles.iter().max().unwrap();
    let next = if previous_angle == *max {
        angles.iter().min().unwrap()
    } else {
        angles
            .iter()
            .filter(|&&x| x > previous_angle)
            .min()
            .unwrap()
    };

    (closest_at_angle(from, coords, *next), *next)
}

fn destroyed_asteroid(
    from: Coords,
    coords: &mut HashSet<Coords>,
    n_th: Number,
    previous_angle: Angle,
) -> Coords {
    let (next, angle) = next_destroyable(from, coords, previous_angle);
    if n_th <= 0 {
        panic!("Unexpected situation - n_th = {n_th}");
    } else if n_th == 1 {
        next
    } else {
        coords.remove(&next);
        destroyed_asteroid(from, coords, n_th - 1, angle)
    }
}

fn part_2(data: &str, n_th: Number) -> Coords {
    const EPS: Angle = 1000; // dealing with floating point precision
    let mut coords = asteroid_coordinates(data);
    let (start, _) = best_placement(&coords);
    let start_angle = *asteroid_angles(start, &coords).iter().max().unwrap() - EPS;
    destroyed_asteroid(start, &mut coords, n_th, start_angle)
}

fn main() {
    solve_1();
    solve_2();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve_1() {
        solve_1();
    }

    fn test_part_1(data: &str, expected_x: Number, expected_y: Number, expected_detected: Number) {
        let (best_coords, detected) = part_1(data);
        assert_eq!(
            best_coords,
            Coords {
                x: expected_x,
                y: expected_y
            }
        );
        assert_eq!(detected, expected_detected);
    }

    #[test]
    fn test_solve_1_0() {
        let test = "\
.#..#
.....
#####
....#
...##
        ";

        test_part_1(test, 3, 4, 8);
    }

    #[test]
    fn test_solve_1_1() {
        let test = "
......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####
";
        test_part_1(test, 5, 8, 33);
    }

    #[test]
    fn test_solve_1_2() {
        let test = "
#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.
";
        test_part_1(test, 1, 2, 35);
    }

    #[test]
    fn test_solve_1_3() {
        let test = "
.#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..
";
        test_part_1(test, 6, 3, 41);
    }

    const LARGE_TEST: &str = "
.#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##
";

    #[test]
    fn test_solve_1_4() {
        test_part_1(LARGE_TEST, 11, 13, 210);
    }

    fn test_part_2(data: &str, n_th: Number, expected_x: Number, expected_y: Number) {
        let result = part_2(data, n_th);
        assert_eq!(result, Coords::new(expected_x, expected_y));
    }

    #[test]
    fn test_solve_2_test() {
        test_part_2(LARGE_TEST, 1, 11, 12);
        test_part_2(LARGE_TEST, 2, 12, 1);
        test_part_2(LARGE_TEST, 3, 12, 2);
        test_part_2(LARGE_TEST, 10, 12, 8);
        test_part_2(LARGE_TEST, 20, 16, 0);
        test_part_2(LARGE_TEST, 50, 16, 9);
        test_part_2(LARGE_TEST, 100, 10, 16);
        test_part_2(LARGE_TEST, 199, 9, 6);
        test_part_2(LARGE_TEST, 200, 8, 2);
        test_part_2(LARGE_TEST, 201, 10, 9);
        test_part_2(LARGE_TEST, 299, 11, 1);
    }

    #[test]
    fn test_solve_2_real() {
        solve_2();
    }
}
