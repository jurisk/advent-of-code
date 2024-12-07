use std::collections::HashSet;
use std::hash::{BuildHasher, Hash};

pub trait Set<T> {
    fn insert(&mut self, value: T) -> bool;
    fn contains(&self, value: &T) -> bool;
}

impl<T: Eq + Hash, S: BuildHasher> Set<T> for HashSet<T, S> {
    fn insert(&mut self, value: T) -> bool {
        HashSet::insert(self, value)
    }

    fn contains(&self, value: &T) -> bool {
        HashSet::contains(self, value)
    }
}
