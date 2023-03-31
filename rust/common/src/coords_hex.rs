use std::fmt::Debug;
use std::ops::Add;

use num_traits::{One, Signed, Zero};

use crate::direction_hex::DirectionHex;

/// Cubic coordinates as per <https://www.redblobgames.com/grids/hexagons/#coordinates>
pub struct CoordsHex<T> {
    pub s: T,
    pub q: T,
    pub r: T,
}

impl<T: Add<Output = T>> Add for CoordsHex<T> {
    type Output = CoordsHex<T>;

    #[inline]
    fn add(self, rhs: CoordsHex<T>) -> CoordsHex<T> {
        CoordsHex {
            s: self.s + rhs.s,
            q: self.q + rhs.q,
            r: self.r + rhs.r,
        }
    }
}

impl<T: Add<Output = T> + Zero + Debug + Eq + Copy + Signed + One> CoordsHex<T> {
    /// # Panics
    ///
    /// Will panic if provided invalid cubic coordinates
    #[must_use]
    pub fn new(s: T, q: T, r: T) -> Self {
        assert_eq!(s + q + r, T::zero());
        CoordsHex { s, q, r }
    }

    #[inline]
    #[must_use]
    pub fn origin() -> CoordsHex<T> {
        CoordsHex::new(T::zero(), T::zero(), T::zero())
    }

    #[must_use]
    pub fn move_in_direction(self, direction: DirectionHex) -> CoordsHex<T> {
        self + direction.diff()
    }

    #[must_use]
    pub fn manhattan_distance_to_origin(&self) -> T {
        (self.s.abs() + self.q.abs() + self.r.abs()) / (T::one() + T::one())
    }
}
