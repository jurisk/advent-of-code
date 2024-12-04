use std::ops::{Add, Neg};

use num_traits::Num;

use crate::coords2d::Coords2D;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum DirectionWithDiagonals {
    N,
    NE,
    E,
    SE,
    S,
    SW,
    W,
    NW,
}

impl DirectionWithDiagonals {
    #[must_use]
    pub const fn all() -> [Self; 8] {
        [
            Self::N,
            Self::NE,
            Self::E,
            Self::SE,
            Self::S,
            Self::SW,
            Self::W,
            Self::NW,
        ]
    }
}

impl<N> From<DirectionWithDiagonals> for Coords2D<N>
where
    N: Num + Neg<Output = N> + Copy,
{
    fn from(value: DirectionWithDiagonals) -> Self {
        match value {
            DirectionWithDiagonals::N => Coords2D::new(N::zero(), N::one()),
            DirectionWithDiagonals::NE => Coords2D::new(N::one(), N::one()),
            DirectionWithDiagonals::E => Coords2D::new(N::one(), N::zero()),
            DirectionWithDiagonals::SE => Coords2D::new(N::one(), -N::one()),
            DirectionWithDiagonals::S => Coords2D::new(N::zero(), -N::one()),
            DirectionWithDiagonals::SW => Coords2D::new(-N::one(), -N::one()),
            DirectionWithDiagonals::W => Coords2D::new(-N::one(), N::zero()),
            DirectionWithDiagonals::NW => Coords2D::new(-N::one(), N::one()),
        }
    }
}

impl<N> Add<DirectionWithDiagonals> for Coords2D<N>
where
    N: Num + Neg<Output = N> + Copy,
{
    type Output = Coords2D<N>;

    fn add(self, rhs: DirectionWithDiagonals) -> Self::Output {
        self + Coords2D::from(rhs)
    }
}
