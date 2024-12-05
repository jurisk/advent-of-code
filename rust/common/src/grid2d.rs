#![expect(
    clippy::cast_possible_wrap,
    clippy::cast_possible_truncation,
    clippy::cast_sign_loss
)]

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

    fn count<F>(&self, f: F) -> usize
    where
        F: Fn(Coords, &T) -> bool;

    fn get(&self, coords: Coords) -> Option<&T>;

    fn get_or_else(&self, coords: Coords, default: T) -> T
    where
        T: Clone,
    {
        self.get(coords).cloned().unwrap_or(default)
    }

    fn sum(&self) -> T
    where
        T: Sum + Clone;
}

pub struct MatrixGrid2D<T> {
    data: Matrix<T>,
}

impl From<(usize, usize)> for Coords {
    fn from(value: (usize, usize)) -> Self {
        let (x, y) = value;
        Coords::new(x as i32, y as i32)
    }
}

impl From<Coords> for (usize, usize) {
    fn from(coords: Coords) -> Self {
        (coords.x as usize, coords.y as usize)
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

    fn sum(&self) -> T
    where
        T: Sum + Clone,
    {
        self.data.values().cloned().sum()
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
