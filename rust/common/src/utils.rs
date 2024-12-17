use std::collections::HashMap;
use std::hash::Hash;

use itertools::Itertools;
use num_traits::Num;

#[must_use]
pub fn head_tail<T>(slice: &[T]) -> Option<(&T, &[T])> {
    if slice.is_empty() {
        None
    } else {
        Some((&slice[0], &slice[1 ..]))
    }
}

#[must_use]
pub fn additive_hashmap_from_vec<K: Eq + Hash, V: Num + Copy>(vec: Vec<(K, V)>) -> HashMap<K, V> {
    let mut result = HashMap::new();
    for (k, v) in vec {
        let zero = V::zero();
        let existing = result.get(&k).unwrap_or(&zero);
        result.insert(k, (*existing).add(v));
    }
    result
}

#[must_use]
pub fn transpose<T: Copy>(lines: &[Vec<T>]) -> Vec<Vec<T>> {
    if let Some(first_line) = lines.first() {
        let number_of_digits = first_line.len();
        (0 .. number_of_digits)
            .map(|idx| lines.iter().map(|line| line[idx]).collect())
            .collect()
    } else {
        vec![]
    }
}

#[must_use]
pub fn single_result<T>(elements: &[T]) -> Option<&T> {
    if elements.len() == 1 {
        elements.first()
    } else {
        None
    }
}

#[must_use]
pub fn least_frequent<T: Eq + Hash>(elements: &[T]) -> Option<&T> {
    let counts: HashMap<&T, usize> = elements.iter().counts();
    let min_count = counts.values().min()?;
    let matching_min_count: Vec<&T> = counts
        .iter()
        .filter(|(_, v)| **v == *min_count)
        .map(|(k, _)| *k)
        .collect();
    if matching_min_count.len() == 1 {
        matching_min_count.first().copied()
    } else {
        None
    }
}

#[must_use]
pub fn most_frequent<T: Eq + Hash>(elements: &[T]) -> Option<&T> {
    let counts: HashMap<&T, usize> = elements.iter().counts();
    let max_count = counts.values().max()?;
    let matching_max_count: Vec<&T> = counts
        .iter()
        .filter(|(_, v)| **v == *max_count)
        .map(|(k, _)| *k)
        .collect();
    if matching_max_count.len() == 1 {
        matching_max_count.first().copied()
    } else {
        None
    }
}
