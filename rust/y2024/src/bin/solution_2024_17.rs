use itertools::Itertools;
use advent_of_code_common::utils::head_tail;

const DATA: [N; 16] = [2, 4, 1, 5, 7, 5, 1, 6, 0, 3, 4, 1, 5, 5, 3, 0];
const PART_1_A: N = 44_374_556;

type N = u64;
type Output = u8;

#[expect(clippy::cast_possible_truncation)]
fn f(a: N) -> (N, Output) {
    let output = ((a % 8) ^ 3 ^ ((a / (1u64 << ((a % 8) ^ 5))) % 8)) as u8;
    let new_a   = a / 8;
    (new_a, output)
}

fn solve_1(a: N) -> String {
    let mut output = vec![];
    let mut a = a;
    while a != 0 {
        let (new_a, o) = f(a);
        output.push(o);
        a = new_a;
    }
    output.iter().join(",")
}

fn reverse_f(a_new: N, expected_output: Output) -> impl Iterator<Item = N> {
    (a_new * 8..(a_new + 1) * 8).filter(move |candidate| {
        let (n_a, o) = f(*candidate);
        o == expected_output && a_new == n_a
    })
}

fn solver(a: N, expectations: &[Output]) -> Option<N> {
    match head_tail(expectations) {
        (Some(h), t) => {
            for candidate in reverse_f(a, *h) {
                if let Some(result) = solver(candidate, t) {
                    return Some(result);
                }
            }

            None
        }
        (None, _) => Some(a),
    }
}

#[expect(clippy::cast_possible_truncation)]
fn solve_2() -> N {
    solver(0, &DATA.into_iter().map(|n| n as Output).rev().collect::<Vec<Output>>()).expect("Expected a solution")
}

fn main() {
    let result_1 = solve_1(PART_1_A);
    println!("Part 1: {result_1}");

    let result_2 = solve_2();
    println!("Part 2: {result_2}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(PART_1_A), "1,5,0,3,7,3,0,3,1");
    }


    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(), 105_981_155_568_026);
    }
}
