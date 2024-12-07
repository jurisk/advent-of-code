use std::collections::{HashMap, HashSet};
use std::hash::Hash;

use crate::mutable_bit_set::MutableBitSet;
use crate::set::Set;

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
    let mut seen: HashSet<T> = HashSet::from([current.clone()]);

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

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum SimulationOutcome {
    Repeats,
    Finished,
}

pub enum SimulationStepResult<T> {
    Continue(T),
    Finished(T),
}

pub fn until_repeats_or_finishes_using_hash_set<T: Clone + Eq + Hash, F>(
    start: T,
    next: F,
) -> (SimulationOutcome, usize, T)
where
    F: Fn(T) -> SimulationStepResult<T>,
{
    let seen: HashSet<T> = HashSet::new();
    until_repeats_or_finishes(start, next, seen)
}

#[expect(clippy::missing_panics_doc)]
pub fn until_repeats_or_finishes_using_bit_set<T: Clone + Eq + Hash, F>(
    start: T,
    next: F,
    capacity: usize,
    to_u32: fn(T) -> u32,
) -> (SimulationOutcome, usize, T)
where
    F: Fn(T) -> SimulationStepResult<T>,
{
    let from_u32 = |_value: u32| {
        panic!("`from_u32` should never be called");
    };
    let seen = MutableBitSet::with_capacity(capacity, to_u32, from_u32);
    until_repeats_or_finishes(start, next, seen)
}

fn until_repeats_or_finishes<T: Clone + Eq + Hash, F, S: Set<T>>(
    start: T,
    next: F,
    set: S,
) -> (SimulationOutcome, usize, T)
where
    F: Fn(T) -> SimulationStepResult<T>,
{
    let mut current = start;
    let mut steps = 0;
    let mut seen = set;
    seen.insert(current.clone());

    loop {
        match next(current) {
            SimulationStepResult::Finished(result) => {
                return (SimulationOutcome::Finished, steps, result);
            },
            SimulationStepResult::Continue(result) => {
                current = result;

                if seen.contains(&current) {
                    return (SimulationOutcome::Repeats, steps, current);
                }

                seen.insert(current.clone());
            },
        }

        steps += 1;
    }
}
