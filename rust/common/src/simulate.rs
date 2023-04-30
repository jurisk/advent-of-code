use std::collections::{HashMap, HashSet};
use std::hash::Hash;

#[must_use]
pub fn n_steps<F, T: Clone>(start: &T, steps: usize, next: F) -> T
where
    F: Fn(&T) -> T,
{
    let mut current = start.clone();
    for _ in 0 .. steps {
        current = next(&current);
    }

    current
}

pub fn run_n_steps_removing_loops<F, T: Clone + Eq + Hash>(
    start: &T,
    steps_needed: usize,
    next: F,
) -> T
where
    F: Fn(&T) -> T,
{
    let mut current = start.clone();
    let mut seen: HashMap<T, usize> = HashMap::new();
    seen.insert(current.clone(), 0);

    let mut steps = 0;
    while steps < steps_needed {
        current = next(&current);
        steps += 1;
        if let Some(when_seen_previously) = seen.get(&current) {
            let loop_size = steps - when_seen_previously;
            let loops_to_remove = (steps_needed - steps) / loop_size;
            let remaining = steps_needed - steps - loops_to_remove * loop_size;
            // In theory, you already have this calculated and stored in `seen` but I did
            // not want to over-complicate it
            return n_steps(&current, remaining, next);
        }
        seen.insert(current.clone(), steps);
    }

    current
}

#[must_use]
pub fn until_state_repeats<F, T: Clone + Eq + Hash>(start: &T, next: F) -> (usize, T)
where
    F: Fn(&T) -> T,
{
    let mut current = start.clone();
    let mut seen: HashSet<T> = HashSet::new();
    seen.insert(current.clone());

    let mut steps = 0;
    loop {
        current = next(&current);
        steps += 1;
        if seen.contains(&current) {
            return (steps, current);
        }
        seen.insert(current.clone());
    }
}

pub fn until_matches_predicate<F, P, T: Clone>(start: &T, next: F, predicate: P) -> (usize, T)
where
    F: Fn(&T) -> T,
    P: Fn(&T) -> bool,
{
    let mut current = start.clone();

    let mut steps = 0;
    loop {
        current = next(&current);
        steps += 1;
        if predicate(&current) {
            return (steps, current);
        }
    }
}
