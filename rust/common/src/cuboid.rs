use crate::coords3d::Coords3D;
use num_traits::One;
use std::cmp::{max, min};
use std::ops::{Add, Sub};

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub struct Cuboid<T> {
    pub min: Coords3D<T>,
    pub max: Coords3D<T>,
}

impl<T: Copy + Sub<Output = T> + Add<Output = T> + One + Ord> Cuboid<T> {
    #[must_use]
    pub fn size(&self) -> T {
        (self.max.x - self.min.x + T::one())
            * (self.max.y - self.min.y + T::one())
            * (self.max.z - self.min.z + T::one())
    }

    #[must_use]
    pub fn overlap(&self, other: &Cuboid<T>) -> Option<Cuboid<T>> {
        let x1 = max(self.min.x, other.min.x);
        let x2 = min(self.max.x, other.max.x);
        let y1 = max(self.min.y, other.min.y);
        let y2 = min(self.max.y, other.max.y);
        let z1 = max(self.min.z, other.min.z);
        let z2 = min(self.max.z, other.max.z);

        if x1 <= x2 && y1 <= y2 && z1 <= z2 {
            let min = Coords3D::new(x1, y1, z1);
            let max = Coords3D::new(x2, y2, z2);

            Some(Cuboid { min, max })
        } else {
            None
        }
    }
}
