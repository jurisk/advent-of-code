use std::str::FromStr;

use advent_of_code_common::bool_ops::BoolResultOps;
use advent_of_code_common::coords2d::Coords2D;
use advent_of_code_common::math::solve_two_variable_integer_linear_equation_system;
use advent_of_code_common::parsing::{
    Error, parse_pair_unsafe, parse_segments_separated_by_double_newline,
};

const DATA: &str = include_str!("../../resources/13.txt");

type N = i64;
type Data = Vec<Machine>;

#[derive(Debug)]
struct Machine {
    button_a: Coords2D<N>,
    button_b: Coords2D<N>,
    prize:    Coords2D<N>,
}

impl Machine {
    #[must_use]
    pub fn solve(&self) -> Option<N> {
        solve_two_variable_integer_linear_equation_system(
            self.button_a.x,
            self.button_b.x,
            self.button_a.y,
            self.button_b.y,
            self.prize.x,
            self.prize.y,
        )
        .map(|(a, b)| a * 3 + b)
    }
}

impl FromStr for Machine {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let lines: Vec<_> = s.lines().collect();
        (lines.len() == 3).then_ok_unit(|| format!("Invalid number of lines: {}", lines.len()))?;
        let a = lines[0]
            .strip_prefix("Button A: ")
            .ok_or("Missing prefix 'Button A: '")?;
        let b = lines[1]
            .strip_prefix("Button B: ")
            .ok_or("Missing prefix 'Button B: '")?;
        let p = lines[2]
            .strip_prefix("Prize: ")
            .ok_or("Missing prefix 'Prize: '")?;

        let int_helper = |prefix: &str, s: &str| -> Result<N, Error> {
            s.strip_prefix(prefix)
                .ok_or(format!("Missing prefix '{}'", prefix))?
                .parse()
                .map_err(|e| format!("{e:?}"))
        };
        let (ax, ay) =
            parse_pair_unsafe(a, ", ", |x| int_helper("X+", x), |y| int_helper("Y+", y))?;
        let (bx, by) =
            parse_pair_unsafe(b, ", ", |x| int_helper("X+", x), |y| int_helper("Y+", y))?;
        let (px, py) =
            parse_pair_unsafe(p, ", ", |x| int_helper("X=", x), |y| int_helper("Y=", y))?;

        Ok(Machine {
            button_a: Coords2D::new(ax, ay),
            button_b: Coords2D::new(bx, by),
            prize:    Coords2D::new(px, py),
        })
    }
}

fn parse(input: &str) -> Result<Data, Error> {
    parse_segments_separated_by_double_newline(input)
}

fn solve_1(data: &Data) -> N {
    data.iter().flat_map(Machine::solve).sum()
}

fn solve_2(data: &Data) -> N {
    let adjusted = data
        .iter()
        .map(|m| {
            Machine {
                button_a: m.button_a,
                button_b: m.button_b,
                prize:    m.prize + Coords2D::new(10000000000000, 10000000000000),
            }
        })
        .collect();

    solve_1(&adjusted)
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

    const TEST_DATA: &str = include_str!("../../resources/13-test-00.txt");

    fn test_data() -> Data {
        parse(TEST_DATA).unwrap()
    }

    fn real_data() -> Data {
        parse(DATA).unwrap()
    }

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(&test_data()), 480);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(&real_data()), 25751);
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(&real_data()), 108528956728655);
    }
}
