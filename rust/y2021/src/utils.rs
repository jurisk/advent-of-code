use num_traits::Num;
use std::collections::HashMap;
use std::hash::Hash;

#[must_use]
pub fn head_tail<T>(slice: &[T]) -> (Option<&T>, &[T]) {
    if slice.is_empty() {
        (None, &[])
    } else {
        (Some(&slice[0]), &slice[1..])
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
