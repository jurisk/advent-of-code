use std::collections::HashSet;
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
