use crate::coords2d::Coords2D;
use crate::rotation::Rotation;
use num_traits::{One, Zero};
use std::ops::Neg;

#[derive(Debug, Clone, Copy, Eq, Hash, PartialEq)]
#[repr(u8)]
pub enum Direction {
    North = b'N',
    East = b'E',
    South = b'S',
    West = b'W',
}

impl Direction {
    #[allow(clippy::match_same_arms)]
    #[must_use]
    pub fn rotate(&self, rotation: Rotation) -> Direction {
        match (rotation, self) {
            (Rotation::Left90, Direction::North) => Direction::West,
            (Rotation::Left90, Direction::East) => Direction::North,
            (Rotation::Left90, Direction::South) => Direction::East,
            (Rotation::Left90, Direction::West) => Direction::South,
            (Rotation::Right90, Direction::North) => Direction::East,
            (Rotation::Right90, Direction::East) => Direction::South,
            (Rotation::Right90, Direction::South) => Direction::West,
            (Rotation::Right90, Direction::West) => Direction::North,
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
        vec![
            Direction::North,
            Direction::South,
            Direction::West,
            Direction::East,
        ]
    }

    #[must_use]
    pub fn diff<T>(self) -> Coords2D<T>
    where
        T: Zero + One + Neg<Output = T> + Copy,
    {
        match self {
            Direction::North => Coords2D::new(T::zero(), -T::one()),
            Direction::South => Coords2D::new(T::zero(), T::one()),
            Direction::West => Coords2D::new(-T::one(), T::zero()),
            Direction::East => Coords2D::new(T::one(), T::zero()),
        }
    }
}
