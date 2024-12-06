#![expect(
    clippy::cast_possible_wrap,
    clippy::cast_possible_truncation,
    clippy::cast_sign_loss
)]

use std::fmt::{Debug, Formatter};
use std::iter::Sum;
use std::str::FromStr;

use pathfinding::matrix::Matrix;

use crate::coords2d::Coords2D;
use crate::parsing::parse_matrix;

pub type Coords = Coords2D<i32>;

pub trait Grid2D<T> {
    fn map_by_coords<F, U>(&self, f: F) -> MatrixGrid2D<U>
    where
        F: Fn(Coords) -> U;

    fn map_by_values<F, U>(&self, f: F) -> MatrixGrid2D<U>
    where
        F: Fn(&T) -> U;

    fn find_coords_by_value(&self, value: &T) -> Option<Coords>
    where
        T: PartialEq;

    fn count<F>(&self, f: F) -> usize
    where
        F: Fn(Coords, &T) -> bool;

    fn get(&self, coords: Coords) -> Option<&T>;

    fn set(&mut self, coords: Coords, value: T);

    fn get_or_else(&self, coords: Coords, default: T) -> T
    where
        T: Clone,
    {
        self.get(coords).cloned().unwrap_or(default)
    }

    fn sum(&self) -> T
    where
        T: Sum + Clone;

    fn iter<'a>(&'a self) -> impl Iterator<Item = (Coords, &'a T)>
    where
        T: 'a;
    fn coords(&self) -> impl Iterator<Item = Coords>;

    fn rows(&self) -> usize;
    fn columns(&self) -> usize;
}

pub struct MatrixGrid2D<T> {
    data: Matrix<T>,
}

impl From<(usize, usize)> for Coords {
    fn from(value: (usize, usize)) -> Self {
        let (r, c) = value;
        Coords::new(c as i32, r as i32)
    }
}

impl From<Coords> for (usize, usize) {
    fn from(coords: Coords) -> Self {
        (coords.y as usize, coords.x as usize)
    }
}

impl<T> Grid2D<T> for MatrixGrid2D<T> {
    fn map_by_coords<F, U>(&self, f: F) -> MatrixGrid2D<U>
    where
        F: Fn(Coords) -> U,
    {
        let new_data = self
            .data
            .keys()
            .map(|coords| f(Coords::from(coords)))
            .collect();
        MatrixGrid2D {
            data: Matrix::from_vec(self.data.rows, self.data.columns, new_data).unwrap(),
        }
    }

    fn map_by_values<F, U>(&self, f: F) -> MatrixGrid2D<U>
    where
        F: Fn(&T) -> U,
    {
        let new_data = self.data.values().map(f).collect();
        MatrixGrid2D {
            data: Matrix::from_vec(self.data.rows, self.data.columns, new_data).unwrap(),
        }
    }

    fn find_coords_by_value(&self, value: &T) -> Option<Coords>
    where
        T: PartialEq,
    {
        self.data.keys().find_map(|coords| {
            if self.data[coords] == *value {
                Some(Coords::from(coords))
            } else {
                None
            }
        })
    }

    fn count<F>(&self, f: F) -> usize
    where
        F: Fn(Coords, &T) -> bool,
    {
        self.data
            .keys()
            .filter(|coords| f(Coords::from(*coords), &self.data[*coords]))
            .count()
    }

    fn get(&self, coords: Coords) -> Option<&T> {
        self.data.get(coords.into())
    }

    fn set(&mut self, coords: Coords, value: T) {
        let coords: (usize, usize) = coords.into();
        self.data[coords] = value;
    }

    fn sum(&self) -> T
    where
        T: Sum + Clone,
    {
        self.data.values().cloned().sum()
    }

    fn coords(&self) -> impl Iterator<Item = Coords> {
        self.data.keys().map(Coords::from)
    }

    fn iter<'a>(&'a self) -> impl Iterator<Item = (Coords, &'a T)>
    where
        T: 'a,
    {
        self.data
            .items()
            .map(|(coords, value)| (Coords::from(coords), value))
    }

    fn rows(&self) -> usize {
        self.data.rows
    }

    fn columns(&self) -> usize {
        self.data.columns
    }
}

impl<T> FromStr for MatrixGrid2D<T>
where
    T: TryFrom<char> + Clone,
{
    type Err = T::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        parse_matrix(s, TryInto::try_into).map(|data| MatrixGrid2D { data })
    }
}

impl<T: Debug> Debug for MatrixGrid2D<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut result = String::new();
        for y in 0 .. self.rows() {
            for x in 0 .. self.columns() {
                let coords: Coords = (x, y).into();
                result.push_str(&format!(
                    "{:?}",
                    self.get(coords).expect("Failed to get value")
                ));
            }
            result.push('\n');
        }
        write!(f, "{result}")
    }
}
