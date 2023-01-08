pub fn n_steps<F, T: Clone>(start: &T, steps: usize, next: F) -> T
where
    F: Fn(&T) -> T,
{
    let mut current = start.clone();
    for _ in 0..steps {
        current = next(&current);
    }

    current
}
