use std::ops::Neg;

use num_traits::{One, Zero};

use crate::coords2d::Coords2D;
use crate::direction::Direction::{East, North, South, West};
use crate::parsing::Error;
use crate::rotation::Rotation;

#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq)]
#[repr(u8)]
pub enum Direction {
    North = b'N',
    East  = b'E',
    South = b'S',
    West  = b'W',
}

impl Direction {
    #[allow(clippy::match_same_arms)]
    #[must_use]
    pub fn rotate(self, rotation: Rotation) -> Direction {
        match (rotation, self) {
            (Rotation::NoRotation, _) => self,

            (Rotation::TurnAround, North) => South,
            (Rotation::TurnAround, East) => West,
            (Rotation::TurnAround, South) => North,
            (Rotation::TurnAround, West) => East,

            (Rotation::Left90, North) => West,
            (Rotation::Left90, East) => North,
            (Rotation::Left90, South) => East,
            (Rotation::Left90, West) => South,

            (Rotation::Right90, North) => East,
            (Rotation::Right90, East) => South,
            (Rotation::Right90, South) => West,
            (Rotation::Right90, West) => North,
        }
    }

    #[must_use]
    pub fn rotate_left(&self) -> Direction {
        self.rotate(Rotation::Left90)
    }

    #[must_use]
    pub fn rotate_right(&self) -> Direction {
        self.rotate(Rotation::Right90)
    }

    #[must_use]
    pub fn all() -> Vec<Direction> {
        vec![North, South, West, East]
    }

    #[must_use]
    pub fn diff<T>(self) -> Coords2D<T>
    where
        T: Zero + One + Neg<Output = T> + Copy,
    {
        match self {
            North => Coords2D::new(T::zero(), -T::one()),
            West => Coords2D::new(-T::one(), T::zero()),
            South => Coords2D::new(T::zero(), T::one()),
            East => Coords2D::new(T::one(), T::zero()),
        }
    }

    /// # Errors
    ///
    /// Will return `Err` if parsing fails.
    pub fn try_from_caret(ch: char) -> Result<Direction, Error> {
        match ch {
            '^' => Ok(North),
            '>' => Ok(East),
            'v' => Ok(South),
            '<' => Ok(West),
            _ => Err(format!("Unrecognized direction caret {ch}")),
        }
    }

    #[must_use]
    pub fn as_udlr_char(self) -> char {
        match self {
            North => 'U',
            East => 'R',
            South => 'D',
            West => 'L',
        }
    }
}
