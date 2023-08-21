use std::cmp::Ordering;
use std::fmt::{Debug, Formatter};
use std::hash::Hash;
use std::ops::Add;

pub struct Pairing<T> {
    smaller: T,
    larger:  T,
}

impl<T: Eq> Pairing<T> {
    pub fn contains(&self, value: &T) -> bool {
        self.smaller == *value || self.larger == *value
    }

    pub fn find_other_side(&self, value: &T) -> Option<&T> {
        if self.larger == *value {
            Some(&self.smaller)
        } else if self.smaller == *value {
            Some(&self.larger)
        } else {
            None
        }
    }
}

impl<T: Ord> Pairing<T> {
    pub fn new(a: T, b: T) -> Self {
        match a.cmp(&b) {
            std::cmp::Ordering::Less | std::cmp::Ordering::Equal => {
                Self {
                    smaller: a,
                    larger:  b,
                }
            },
            std::cmp::Ordering::Greater => {
                Self {
                    smaller: b,
                    larger:  a,
                }
            },
        }
    }
}

impl<T: Debug> Debug for Pairing<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{:?}, {:?}", self.smaller, self.larger))
    }
}

impl<T> From<Pairing<T>> for (T, T) {
    fn from(val: Pairing<T>) -> Self {
        (val.smaller, val.larger)
    }
}

impl<T: Copy> Copy for Pairing<T> {}

#[allow(clippy::expl_impl_clone_on_copy)]
impl<T: Clone> Clone for Pairing<T> {
    fn clone(&self) -> Self {
        Self {
            smaller: self.smaller.clone(),
            larger:  self.larger.clone(),
        }
    }
}

impl<T: Eq> Eq for Pairing<T> {}

impl<T: PartialEq> PartialEq for Pairing<T> {
    fn eq(&self, other: &Self) -> bool {
        self.smaller == other.smaller && self.larger == other.larger
    }
}

impl<T: Hash> Hash for Pairing<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.smaller.hash(state);
        self.larger.hash(state);
    }
}

impl<T: Copy + Add<T, Output = T>> Pairing<T> {
    pub fn sum(&self) -> T {
        self.smaller + self.larger
    }
}

impl<T: Ord> PartialOrd for Pairing<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: Ord> Ord for Pairing<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.smaller
            .cmp(&other.smaller)
            .then(self.larger.cmp(&other.larger))
    }
}
