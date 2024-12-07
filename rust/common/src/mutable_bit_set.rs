use std::marker::PhantomData;

use bit_set::BitSet;

pub struct MutableBitSet<T> {
    underlying: BitSet<usize>,
    phantom:    PhantomData<T>,
}

impl<T> Default for MutableBitSet<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> MutableBitSet<T> {
    #[must_use]
    pub fn new() -> Self {
        Self {
            underlying: BitSet::default(),
            phantom:    PhantomData,
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

impl<T> MutableBitSet<T>
where
    usize: From<T>,
{
    pub fn insert(&mut self, value: T) -> bool {
        self.underlying.insert(value.into())
    }

    #[must_use]
    pub fn contains(&self, value: T) -> bool {
        self.underlying.contains(value.into())
    }
}

pub struct  MutableBitSetIter<'a, T> {
    iter: bit_set::Iter<'a, usize>, // Adjust this to match `BitSet`'s iterator
    phantom: PhantomData<T>,
}

impl<T> Iterator for MutableBitSetIter<'_, T>
where
    T: From<usize>,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(T::from)
    }
}

impl<'a, T> IntoIterator for &'a MutableBitSet<T>
where
    T: From<usize>,
{
    type Item = T;
    type IntoIter = MutableBitSetIter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        MutableBitSetIter {
            iter: self.underlying.into_iter(),
            phantom: PhantomData,
        }
    }
}

impl<T, const N: usize> From<[T; N]> for MutableBitSet<T>
where
    usize: From<T>,
{
    fn from(value: [T; N]) -> Self {
        MutableBitSet::from_iter(value)
    }
}

impl <T> FromIterator<T> for MutableBitSet<T>
where
    usize: From<T>,
{
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut set = MutableBitSet::new();
        for value in iter {
            set.insert(value);
        }
        set
    }
}