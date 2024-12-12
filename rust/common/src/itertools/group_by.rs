use std::collections::HashMap;
use std::hash::Hash;

pub trait GroupBy: Iterator {
    #[must_use]
    fn group_by_key<F, K>(self, f: F) -> HashMap<K, Vec<Self::Item>>
    where
        Self: Sized,
        Self::Item: Eq,
        K: Eq + Hash,
        F: Fn(&Self::Item) -> K,
    {
        let mut results: HashMap<K, Vec<Self::Item>> = HashMap::new();
        for x in self {
            let k = f(&x);
            results.entry(k).or_default().push(x);
        }
        results
    }
}

impl<T> GroupBy for T where T: Iterator {}
