use std::str::FromStr;

use advent_of_code_common::parsing::{
    Error, parse_comma_separated_vec, parse_str, split_into_two_strings,
};
use advent_of_code_common::simulate::run_n_steps_removing_loops;

use crate::Move::{Exchange, Partner, Spin};

type Program = char;
type Programs = Vec<Program>;

#[derive(Debug)]
enum Move {
    Spin(usize),
    Exchange(usize, usize),
    Partner(Program, Program),
}

impl FromStr for Move {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.is_empty() {
            Err("empty".to_string())
        } else {
            let t = &s[1 ..];
            match s.chars().next() {
                Some('s') => {
                    let n = parse_str(t)?;
                    Ok(Spin(n))
                },
                Some('x') => {
                    let (a, b) = split_into_two_strings(t, "/")?;
                    let an = parse_str(&a)?;
                    let bn = parse_str(&b)?;
                    Ok(Exchange(an, bn))
                },
                Some('p') => {
                    let (a, b) = split_into_two_strings(t, "/")?;
                    assert_eq!(a.len(), 1);
                    assert_eq!(b.len(), 1);
                    Ok(Partner(
                        a.chars().next().unwrap(),
                        b.chars().next().unwrap(),
                    ))
                },
                _ => Err(format!("Unrecognized {s}")),
            }
        }
    }
}

impl Move {
    fn apply_to(&self, v: &mut Programs) {
        match self {
            Spin(n) => {
                let m = v.len() - n;
                let updated = [v[m ..].to_vec(), v[.. m].to_vec()].concat();
                *v = updated;
            },
            Exchange(a, b) => {
                v.swap(*a, *b);
            },
            Partner(a, b) => {
                let ai = v.iter().position(|&x| x == *a).unwrap();
                let bi = v.iter().position(|&x| x == *b).unwrap();
                Exchange(ai, bi).apply_to(v);
            },
        }
    }
}

fn starting_programs() -> Programs {
    ('a' ..= 'p').collect()
}

fn parse_moves(input: &str) -> Result<Vec<Move>, Error> {
    parse_comma_separated_vec(input)
}

fn solve_1(moves: &[Move], programs: &Programs) -> Programs {
    let mut result = programs.clone();
    for m in moves {
        m.apply_to(&mut result);
    }
    result
}

fn part_1(input: &str, start: &Programs) -> Result<String, Error> {
    let moves = parse_moves(input)?;
    let result = solve_1(&moves, start);
    Ok(result.into_iter().collect())
}

fn part_2(input: &str, start: &Programs) -> Result<String, Error> {
    let moves = parse_moves(input)?;
    let result = run_n_steps_removing_loops(start, 1_000_000_000, |x| solve_1(&moves, x));
    Ok(result.into_iter().collect())
}

const DATA: &str = include_str!("../../resources/16.txt");

fn main() -> Result<(), Error> {
    let result_1 = part_1(DATA, &starting_programs())?;
    println!("Part 1: {result_1}");

    let result_2 = part_2(DATA, &starting_programs())?;
    println!("Part 2: {result_2}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST: &str = "s1,x3/4,pe/b";

    fn test_starting_programs() -> Programs {
        ('a' ..= 'e').collect()
    }

    #[test]
    fn test_solve_1_test() {
        assert_eq!(
            part_1(TEST, &test_starting_programs()),
            Ok("baedc".to_string())
        );
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(
            part_1(DATA, &starting_programs()),
            Ok("hmefajngplkidocb".to_string())
        );
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(
            part_2(DATA, &starting_programs()),
            Ok("fbidepghmjklcnoa".to_string())
        );
    }
}
