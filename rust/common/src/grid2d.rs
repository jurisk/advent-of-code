#![expect(
    clippy::cast_possible_wrap,
    clippy::cast_possible_truncation,
    clippy::cast_sign_loss
)]

use std::convert::{Infallible, identity};
use std::fmt::{Debug, Formatter};
use std::iter::Sum;
use std::str::FromStr;

use pathfinding::matrix::Matrix;

use crate::coords2d::Coords2D;
use crate::direction::Direction;
use crate::direction_with_diagonals::DirectionWithDiagonals;
use crate::parsing::parse_matrix;

pub type Coords = Coords2D<i32>;

pub trait Grid2D<T> {
    fn from_vec(data: Vec<Vec<T>>) -> Self;

    fn new(width: usize, height: usize, default: T) -> Self
    where
        T: Clone;

    // TODO: Rather sub-optimal that this isn't `impl Grid2D<U>` or similar
    fn map_by_coords<F, U>(&self, f: F) -> MatrixGrid2D<U>
    where
        F: Fn(Coords) -> U;

    // TODO: Rather sub-optimal that this isn't `impl Grid2D<U>` or similar
    fn map_by_values<F, U>(&self, f: F) -> MatrixGrid2D<U>
    where
        F: Fn(&T) -> U;

    // TODO: Rather sub-optimal that this isn't `impl Grid2D<U>` or similar
    fn flat_map_by_values<F, U: Default + Clone>(
        &self,
        width: usize,
        height: usize,
        f: F,
    ) -> MatrixGrid2D<U>
    where
        F: Fn(&T) -> MatrixGrid2D<U>;

    fn find_coords_by_value(&self, value: &T) -> Option<Coords>
    where
        T: PartialEq;

    fn filter_coords_by_value(&self, value: &T) -> Vec<Coords>
    where
        T: PartialEq;

    fn count<F>(&self, f: F) -> usize
    where
        F: Fn(Coords, &T) -> bool;

    fn valid_coords(&self, coords: Coords) -> bool;

    fn get(&self, coords: Coords) -> Option<&T>;

    fn get_or_default(&self, coords: Coords) -> T
    where
        T: Default + Clone,
    {
        self.get(coords).cloned().unwrap_or_default()
    }

    fn set(&mut self, coords: Coords, value: T);
    fn modify<F>(&mut self, coords: Coords, f: F)
    where
        F: FnOnce(&mut T);

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
    fn height(&self) -> usize {
        self.rows()
    }

    fn columns(&self) -> usize;
    fn width(&self) -> usize {
        self.columns()
    }

    fn is_empty(&self) -> bool {
        self.len() == 0
    }

    fn len(&self) -> usize {
        self.rows() * self.columns()
    }

    fn neighbours_for(&self, c: Coords, include_diagonal: bool) -> Vec<Coords> {
        if include_diagonal {
            DirectionWithDiagonals::all()
                .into_iter()
                .map(|d| c + d)
                .filter(|n| self.valid_coords(*n))
                .collect()
        } else {
            Direction::all()
                .into_iter()
                .map(|d| c + d)
                .filter(|n| self.valid_coords(*n))
                .collect()
        }
    }
}

#[derive(Clone)]
pub struct MatrixGrid2D<T> {
    data: Matrix<T>,
}

impl MatrixGrid2D<char> {
    #[must_use]
    pub fn parse_char_field(input: &str) -> Self {
        MatrixGrid2D::<char>::parse(input, identity)
    }
}

impl<T> MatrixGrid2D<T> {
    #[expect(clippy::missing_panics_doc)]
    pub fn parse(input: &str, f: impl Fn(char) -> T) -> Self {
        Self {
            data: parse_matrix(input, |ch| Ok::<T, Infallible>(f(ch))).unwrap(),
        }
    }
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
    #[must_use]
    fn from_vec(data: Vec<Vec<T>>) -> Self {
        let rows = data.len();
        let columns = data.first().map_or(0, Vec::len);
        let flattened = data.into_iter().flatten().collect();
        Self {
            data: Matrix::from_vec(rows, columns, flattened).expect("Failed to create matrix"),
        }
    }

    #[must_use]
    fn new(width: usize, height: usize, default: T) -> Self
    where
        T: Clone,
    {
        Self {
            data: Matrix::new(height, width, default),
        }
    }

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

    fn flat_map_by_values<F, U: Default + Clone>(
        &self,
        width: usize,
        height: usize,
        f: F,
    ) -> MatrixGrid2D<U>
    where
        F: Fn(&T) -> MatrixGrid2D<U>,
    {
        let mut result =
            MatrixGrid2D::new(self.columns() * width, self.rows() * height, U::default());
        for (coords, value) in self.iter() {
            let new_data = f(value);
            for c in new_data.coords() {
                let new_coords = Coords::new(
                    coords.x * width as i32 + c.x,
                    coords.y * height as i32 + c.y,
                );
                result.set(
                    new_coords,
                    new_data.get(c).expect("Failed to get value").clone(),
                );
            }
        }
        result
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

    fn filter_coords_by_value(&self, value: &T) -> Vec<Coords>
    where
        T: PartialEq,
    {
        self.data
            .keys()
            .filter_map(|coords| {
                if self.data[coords] == *value {
                    Some(Coords::from(coords))
                } else {
                    None
                }
            })
            .collect()
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

    fn modify<F>(&mut self, coords: Coords, f: F)
    where
        F: FnOnce(&mut T),
    {
        let coords: (usize, usize) = coords.into();
        f(&mut self.data[coords]);
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

    fn valid_coords(&self, coords: Coords) -> bool {
        let (r, c) = coords.into();
        self.data.within_bounds((r, c))
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

impl<T: Into<char> + Clone> Debug for MatrixGrid2D<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut result = String::new();
        for y in 0 .. self.rows() {
            for x in 0 .. self.columns() {
                let coords: Coords = Coords::new(x as i32, y as i32);

                let value = self.get(coords).expect("Failed to get value");
                let ch: char = value.clone().into();
                result.push(ch);
            }
            result.push('\n');
        }
        write!(f, "{result}")
    }
}
