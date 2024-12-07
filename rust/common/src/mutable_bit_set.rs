use std::marker::PhantomData;

use bit_set::BitSet;

use crate::set::Set;

pub struct MutableBitSet<T> {
    underlying: BitSet,
    to_u32:     fn(T) -> u32,
    from_u32:   fn(u32) -> T,
    phantom:    PhantomData<T>,
}

impl<T> MutableBitSet<T> {
    #[must_use]
    pub fn new(to_u32: fn(T) -> u32, from_u32: fn(u32) -> T) -> Self {
        Self {
            underlying: BitSet::default(),
            to_u32,
            from_u32,
            phantom: PhantomData,
        }
    }

    #[must_use]
    pub fn with_capacity(capacity: usize, to_u32: fn(T) -> u32, from_u32: fn(u32) -> T) -> Self {
        Self {
            underlying: BitSet::with_capacity(capacity),
            to_u32,
            from_u32,
            phantom: PhantomData,
        }
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.underlying.is_empty()
    }

    #[must_use]
    pub fn len(&self) -> usize {
        self.underlying.len()
    }

    #[must_use]
    pub fn iter(&self) -> MutableBitSetIter<'_, T>
    where
        T: From<usize>,
    {
        self.into_iter()
    }
}

impl<T: Clone> Set<T> for MutableBitSet<T> {
    fn insert(&mut self, value: T) -> bool {
        let value = (self.to_u32)(value);
        self.underlying.insert(value as usize)
    }

    #[must_use]
    fn contains(&self, value: &T) -> bool {
        let value = (self.to_u32)(value.clone());
        self.underlying.contains(value as usize)
    }
}

pub struct MutableBitSetIter<'a, T> {
    iter:     bit_set::Iter<'a, u32>, // Adjust this to match `BitSet`'s iterator
    from_u32: fn(u32) -> T,
    phantom:  PhantomData<T>,
}

impl<T> Iterator for MutableBitSetIter<'_, T> {
    type Item = T;

    #[expect(clippy::cast_possible_truncation)]
    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(|value| (self.from_u32)(value as u32))
    }
}

impl<'a, T> IntoIterator for &'a MutableBitSet<T> {
    type IntoIter = MutableBitSetIter<'a, T>;
    type Item = T;

    fn into_iter(self) -> Self::IntoIter {
        MutableBitSetIter {
            iter:     self.underlying.into_iter(),
            from_u32: self.from_u32,
            phantom:  PhantomData,
        }
    }
}
