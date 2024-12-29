use std::cmp::Ordering;
use std::fmt::{Debug, Formatter};
use std::hash::Hash;

pub struct SetOfTwo<T> {
    a: T,
    b: T,
}

impl<T: Ord + Debug> Debug for SetOfTwo<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:?}, {:?})", self.min(), self.max())
    }
}

impl<T> SetOfTwo<T> {
    #[must_use]
    pub fn new(a: T, b: T) -> Option<Self>
    where
        T: PartialEq,
    {
        (a != b).then_some(Self { a, b })
    }

    #[must_use]
    pub fn to_tuple_unordered(self) -> (T, T) {
        (self.a, self.b)
    }
}

impl<T: Clone> Clone for SetOfTwo<T> {
    fn clone(&self) -> Self {
        Self {
            a: self.a.clone(),
            b: self.b.clone(),
        }
    }
}

impl<T> Hash for SetOfTwo<T>
where
    T: Hash,
{
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.a.hash(state);
        self.b.hash(state);
    }
}

impl<T> PartialEq<Self> for SetOfTwo<T>
where
    T: Eq,
{
    fn eq(&self, other: &Self) -> bool {
        (self.a == other.a && self.b == other.b) || (self.a == other.b && self.b == other.a)
    }
}

impl<T> Eq for SetOfTwo<T> where T: Eq {}

impl<T: Ord> SetOfTwo<T> {
    #[must_use]
    fn min(&self) -> &T {
        match self.a.cmp(&self.b) {
            Ordering::Less => &self.a,
            Ordering::Equal => panic!("Did not expect equal elements"),
            Ordering::Greater => &self.b,
        }
    }

    #[must_use]
    fn max(&self) -> &T {
        match self.a.cmp(&self.b) {
            Ordering::Less => &self.b,
            Ordering::Equal => panic!("Did not expect equal elements"),
            Ordering::Greater => &self.a,
        }
    }

    #[must_use]
    pub fn to_tuple_ordered(&self) -> (&T, &T) {
        (self.min(), self.max())
    }
}

impl<T: Ord> PartialOrd for SetOfTwo<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: Ord> Ord for SetOfTwo<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.min().cmp(other.min()) {
            Ordering::Equal => self.max().cmp(other.max()),
            ordering => ordering,
        }
    }
}

impl<T> IntoIterator for SetOfTwo<T> {
    type IntoIter = std::vec::IntoIter<T>;
    type Item = T;

    fn into_iter(self) -> Self::IntoIter {
        vec![self.a, self.b].into_iter()
    }
}
