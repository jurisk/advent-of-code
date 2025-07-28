use std::collections::BTreeMap;

struct State {
    tree:     BTreeMap<usize, u32>,
    previous: usize,
}

impl State {
    fn new(count: usize) -> State {
        let mut tree: BTreeMap<usize, u32> = BTreeMap::new();
        for i in 1 ..= count {
            tree.insert(i, 1);
        }
        State { tree, previous: 0 }
    }

    fn winner(&self) -> Option<usize> {
        let count = self.tree.len();

        if count == 1 {
            Some(*self.tree.iter().next().unwrap().0)
        } else {
            if count % 1000 == 0 {
                println!("{count} remaining...");
            }

            None
        }
    }

    fn successor(&self, idx: usize) -> usize {
        match self.tree.range(idx + 1 ..).next() {
            None => *self.tree.iter().next().unwrap().0,
            Some((k, _v)) => *k,
        }
    }

    // This is not optimal at all and takes a very long time to run for large
    // inputs, but I used `BTreeMap` for Part 1 and this was the quickest to
    // write.
    fn opposite(&self, idx: usize) -> usize {
        let add = self.tree.len() / 2 - 1;
        let followers: Vec<_> = self.tree.range(idx + 1 ..).collect();

        if add < followers.len() {
            *followers[add].0
        } else {
            let from_start = add - followers.len();
            *self.tree.iter().nth(from_start).unwrap().0
        }
    }

    fn do_step<F>(&mut self, select_victim: F)
    where
        F: Fn(&State, usize) -> usize,
    {
        let attacker = self.successor(self.previous);
        let victim = select_victim(self, attacker);

        let attacker_items = self.tree.get(&attacker).unwrap();
        let victim_items = self.tree.get(&victim).unwrap();

        let new_attacker_items = attacker_items + victim_items;
        self.tree.remove(&victim);
        self.tree.insert(attacker, new_attacker_items);
        self.previous = attacker;
    }
}

fn solve<F>(input: usize, select_victim: F) -> usize
where
    F: Fn(&State, usize) -> usize,
{
    let mut state = State::new(input);

    loop {
        if let Some(winner) = state.winner() {
            return winner;
        }

        state.do_step(&select_victim);
    }
}

fn part_1(input: usize) -> usize {
    solve(input, State::successor)
}

fn part_2(input: usize) -> usize {
    solve(input, State::opposite)
}

const DATA: usize = 3_001_330;

fn main() {
    let result_1 = part_1(DATA);
    println!("Part 1: {result_1}");

    let result_2 = part_2(DATA);
    println!("Part 2: {result_2}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve_1_test_0() {
        assert_eq!(part_1(5), 3);
    }

    #[test]
    fn test_solve_1_test_1() {
        assert_eq!(part_1(1_000), 977);
    }

    #[test]
    fn test_solve_1_test_2() {
        assert_eq!(part_1(10_000), 3_617);
    }

    #[test]
    #[ignore = "Slow"]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), 1_808_357);
    }

    #[test]
    fn test_opposite_5() {
        let state = State::new(5);
        assert_eq!(state.opposite(1), 3);
        assert_eq!(state.opposite(2), 4);
        assert_eq!(state.opposite(3), 5);
        assert_eq!(state.opposite(4), 1);
        assert_eq!(state.opposite(5), 2);
    }

    #[test]
    fn test_opposite_4() {
        let mut state = State::new(5);
        state.tree.remove(&3);
        assert_eq!(state.opposite(1), 4);
        assert_eq!(state.opposite(2), 5);
        assert_eq!(state.opposite(4), 1);
        assert_eq!(state.opposite(5), 2);
    }

    #[test]
    fn test_opposite_2() {
        let state = State::new(2);
        assert_eq!(state.opposite(1), 2);
        assert_eq!(state.opposite(2), 1);
    }

    #[test]
    fn test_solve_2_test_0() {
        assert_eq!(part_2(5), 2);
    }

    #[test]
    fn test_solve_2_test_1() {
        assert_eq!(part_2(1_000), 271);
    }

    #[test]
    #[ignore = "Slow"]
    fn test_solve_2_test_2() {
        assert_eq!(part_2(10_000), 3_439);
    }

    #[test]
    #[ignore = "Slow"]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), 1_407_007);
    }
}
