use std::ops::{Index, IndexMut};

#[derive(Debug)]
pub struct Circular<T> {
    pub vec: Vec<T>,
}

impl<T> Circular<T> {
    #[must_use]
    fn wrap_index(&self, index: usize) -> usize {
        index % self.len()
    }

    #[must_use]
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        self.vec.len()
    }
}

impl<T> Circular<T> {
    fn swap(&mut self, a: usize, b: usize) {
        let aw = self.wrap_index(a);
        let bw = self.wrap_index(b);
        self.vec.swap(aw, bw);
    }

    pub fn reverse_slice(&mut self, from: usize, length: usize) {
        for i in 0 .. (length / 2) {
            self.swap(from + i, from + (length - i - 1));
        }
    }
}

impl<T> IndexMut<usize> for Circular<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        let wrapped_index = self.wrap_index(index);
        &mut self.vec[wrapped_index]
    }
}

impl<T> Index<usize> for Circular<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        let wrapped_index = self.wrap_index(index);
        &self.vec[wrapped_index]
    }
}

impl<T> FromIterator<T> for Circular<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut vec = Vec::new();

        for i in iter {
            vec.push(i);
        }

        Circular { vec }
    }
}
