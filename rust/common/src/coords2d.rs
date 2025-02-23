use std::fmt::Debug;
use std::iter::Step;
use std::ops::{Add, AddAssign, Neg, Sub};
use std::str::FromStr;

use num_traits::{One, Signed, Zero};

use crate::direction::Direction;
use crate::grid2d::Coords;
use crate::parsing::{Error, split_into_two_strings};

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub struct Coords2D<T> {
    pub x: T,
    pub y: T,
}

impl<T: Zero> Coords2D<T> {
    #[inline]
    #[must_use]
    pub fn origin() -> Coords2D<T> {
        Coords2D::new(T::zero(), T::zero())
    }
}

impl<T: Add<Output = T>> Add for Coords2D<T> {
    type Output = Coords2D<T>;

    #[inline]
    fn add(self, rhs: Coords2D<T>) -> Coords2D<T> {
        Coords2D {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl AddAssign for Coords2D<i32> {
    fn add_assign(&mut self, rhs: Self) {
        self.x += rhs.x;
        self.y += rhs.y;
    }
}

impl<T: Sub<Output = T>> Sub for Coords2D<T> {
    type Output = Coords2D<T>;

    fn sub(self, rhs: Self) -> Self::Output {
        Coords2D {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
        }
    }
}

impl<T> Coords2D<T> {
    #[inline]
    pub const fn new(x: T, y: T) -> Coords2D<T> {
        Coords2D { x, y }
    }
}

impl<T: Copy + Add<Output = T> + Zero + One + Neg<Output = T>> Coords2D<T> {
    #[must_use]
    pub fn move_in_direction(self, direction: Direction) -> Coords2D<T> {
        self + direction.diff()
    }
}

impl<T: Copy + One + Add<Output = T> + Sub<Output = T>> Coords2D<T> {
    #[must_use]
    pub fn adjacent4(self) -> Vec<Coords2D<T>> {
        vec![
            Coords2D {
                x: self.x,
                y: self.y + T::one(),
            },
            Coords2D {
                x: self.x,
                y: self.y - T::one(),
            },
            Coords2D {
                x: self.x + T::one(),
                y: self.y,
            },
            Coords2D {
                x: self.x - T::one(),
                y: self.y,
            },
        ]
    }

    #[must_use]
    pub fn adjacent8(self) -> Vec<Coords2D<T>> {
        let diagonals = vec![
            Coords2D {
                x: self.x + T::one(),
                y: self.y + T::one(),
            },
            Coords2D {
                x: self.x - T::one(),
                y: self.y - T::one(),
            },
            Coords2D {
                x: self.x + T::one(),
                y: self.y - T::one(),
            },
            Coords2D {
                x: self.x - T::one(),
                y: self.y + T::one(),
            },
        ];

        [self.adjacent4(), diagonals].concat()
    }
}

impl<T> Coords2D<T>
where
    T: Sub + Signed,
{
    pub fn manhattan_distance(self: Coords2D<T>, other: Coords2D<T>) -> T {
        (self.x - other.x).abs() + (self.y - other.y).abs()
    }

    pub fn manhattan_distance_to_origin(self: Coords2D<T>) -> T {
        self.x.abs() + self.y.abs()
    }

    pub fn all_coords_within_manhattan_distance(
        self: Coords2D<T>,
        distance_inclusive: T,
    ) -> Vec<Coords2D<T>>
    where
        T: Copy + Step,
    {
        let mut result = Vec::new();
        for x in -distance_inclusive ..= distance_inclusive {
            for y in -distance_inclusive ..= distance_inclusive {
                if x.abs() + y.abs() <= distance_inclusive {
                    result.push(self + Coords2D::new(x, y));
                }
            }
        }
        result
    }
}

impl<T: FromStr> FromStr for Coords2D<T>
where
    <T as FromStr>::Err: Debug,
{
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (x_str, y_str) = split_into_two_strings(s, ",")?;
        let x = x_str.parse().map_err(|err| format!("{err:?}"))?;
        let y = y_str.parse().map_err(|err| format!("{err:?}"))?;

        Ok(Coords2D { x, y })
    }
}

impl<T> Coords2D<T>
where
    T: Copy,
{
    #[inline]
    pub const fn as_tuple(&self) -> (T, T) {
        (self.y, self.x)
    }
}

// These conversions to/from usize are needed for the MutableBitSet
#[must_use]
#[expect(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
pub fn to_u32(coords: Coords, grid_width: usize) -> u32 {
    (coords.x as u32) + (coords.y as u32) * grid_width as u32
}

#[must_use]
#[expect(clippy::cast_possible_truncation, clippy::cast_possible_wrap)]
pub fn from_u32(value: u32, grid_width: usize) -> Coords {
    Coords {
        x: (value % grid_width as u32) as i32,
        y: (value / grid_width as u32) as i32,
    }
}
