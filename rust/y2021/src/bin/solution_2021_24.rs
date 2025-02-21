use std::collections::HashMap;

use advent_of_code_common::parsing::Error;
use itertools::Itertools;

const DATA: &str = include_str!("../../resources/24.txt");

type N = i64;

#[derive(Debug)]
#[allow(clippy::struct_field_names)]
struct Subroutine {
    line_4:  N,
    line_5:  N,
    line_15: N,
}

impl Subroutine {
    fn solve(&self, old_z: N, input: N) -> N {
        let mut z: N = old_z;
        // 0: inp w
        let w: N = input;
        // 1: mul x 0
        // 2: add x z
        // 3: mod x 26
        // 5: add x line_5
        let mut x = z % 26 + self.line_5;
        // 4: div z line_4;
        z /= self.line_4;
        // 6: eql x w
        // 7: eql x 0
        x = i64::from(x != w);

        // 8: mul y 0
        // 9: add y 25
        // 10: mul y x
        // 11: add y 1
        let mut y = 25 * x + 1;
        // 12: mul z y
        z *= y;
        // 13: mul y 0
        // 14: add y w
        // 15: add y line_15
        // 16: mul y x
        y = (w + self.line_15) * x;

        // 17: add z y
        z += y;

        // println!("Subroutine #{self:?}: z = {old_z}, input = {input} => {z}");

        z
    }
}

const C: usize = 14;
const L: usize = 18;

// Note - This is not "general purpose", but if we needed to, we could have
// stored all registers in the state.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
struct State {
    instruction_pointer: usize,
    z:                   N,
}

impl State {
    fn start() -> State {
        State {
            instruction_pointer: 0,
            z:                   0,
        }
    }

    fn is_success(&self) -> bool {
        self.instruction_pointer >= C && self.z == 0
    }

    fn successors(&self, program: &[Subroutine], search_order: &[N]) -> Vec<(N, State)> {
        if let Some(subroutine) = program.get(self.instruction_pointer) {
            let instruction_pointer = self.instruction_pointer + 1;
            search_order
                .iter()
                .map(|input| {
                    let z = subroutine.solve(self.z, *input);
                    (
                        *input,
                        State {
                            instruction_pointer,
                            z,
                        },
                    )
                })
                .collect()
        } else {
            vec![]
        }
    }
}

// Not a general purpose solution, though a general purpose solution could be
// done in a somewhat similar way, by memoizing all (register_state,
// instruction_pointer, input_value) combinations.
#[allow(clippy::similar_names, clippy::redundant_closure_for_method_calls)]
fn parse(input: &str) -> Result<Vec<Subroutine>, Error> {
    let lines: Vec<String> = input
        .lines()
        .filter(|x| !x.is_empty())
        .map(ToString::to_string)
        .collect();

    assert_eq!(lines.len() % C, 0);

    let chunks: Vec<Vec<String>> = lines.chunks(L).map(|slice| slice.to_vec()).collect();

    for idx in 0 .. L {
        let options: Vec<String> = chunks
            .iter()
            .map(|chunk| chunk[idx].to_string())
            .unique()
            .collect();
        println!("Line {idx}: {options:?}");
    }

    let mut subroutines: Vec<Subroutine> = Vec::new();
    for chunk in chunks {
        let line_4 = chunk[4]
            .split(' ')
            .next_back()
            .ok_or("Failed to split Line 4")?;
        let line_5 = chunk[5]
            .split(' ')
            .next_back()
            .ok_or("Failed to split Line 5")?;
        let line_15 = chunk[15]
            .split(' ')
            .next_back()
            .ok_or("Failed to split Line 4")?;
        let subroutine = Subroutine {
            line_4:  line_4.parse().unwrap(),
            line_5:  line_5.parse().unwrap(),
            line_15: line_15.parse().unwrap(),
        };
        subroutines.push(subroutine);
    }
    Ok(subroutines)
}

#[derive(Copy, Clone)]
enum SearchOrder {
    Smallest,
    Largest,
}

impl SearchOrder {
    fn range(self) -> [N; 9] {
        match self {
            SearchOrder::Smallest => [1, 2, 3, 4, 5, 6, 7, 8, 9],
            SearchOrder::Largest => [9, 8, 7, 6, 5, 4, 3, 2, 1],
        }
    }
}

fn best(
    program: &[Subroutine],
    cached: &mut HashMap<State, Option<N>>,
    state: &State,
    range: &[N],
) -> Option<N> {
    if let Some(found) = cached.get(state) {
        return *found;
    }

    let successors = state.successors(program, range);
    if successors.is_empty() {
        if state.is_success() { Some(0) } else { None }
    } else {
        for (input, new_state) in successors {
            let search = best(program, cached, &new_state, range);
            if let Some(found) = search {
                let value = found * 10 + input;
                println!("Inserting {new_state:?}, {value}");
                cached.insert(new_state, Some(value));
                return Some(value);
            }
        }

        cached.insert(*state, None);
        None
    }
}

fn rev(mut u: N) -> N {
    let mut r = 0;
    while u != 0 {
        r = r * 10 + u % 10;
        u /= 10;
    }
    r
}

fn solve(program: &[Subroutine], search_order: SearchOrder) -> Result<N, Error> {
    let search_order_range = search_order.range();
    let mut cached: HashMap<State, Option<N>> = HashMap::new();

    let result = best(program, &mut cached, &State::start(), &search_order_range).map(rev);

    println!("{result:?}");
    result.ok_or_else(|| "Failed".to_string())
}

fn solve_both(input: &str) -> Result<(i64, i64), Error> {
    let program = parse(input)?;
    println!("{program:?}");
    let largest = solve(&program, SearchOrder::Largest)?;
    let smallest = solve(&program, SearchOrder::Smallest)?;
    Ok((largest, smallest))
}

fn main() {
    let result = solve_both(DATA);
    println!("{result:?}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore]
    fn test_solve() {
        assert_eq!(
            solve_both(DATA),
            Ok((99_298_993_199_873, 73_181_221_197_111))
        );
    }
}
