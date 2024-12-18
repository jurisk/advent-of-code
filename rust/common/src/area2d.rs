use std::cmp::{max, min};
use std::iter::Step;

use crate::coords2d::Coords2D;

pub struct Area2D<T> {
    min: Coords2D<T>,
    max: Coords2D<T>,
}

impl<T: Copy + Ord + Step> Area2D<T> {
    pub fn new(a: Coords2D<T>, b: Coords2D<T>) -> Area2D<T> {
        let min_point = Coords2D::new(min(a.x, b.x), min(a.y, b.y));
        let max_point = Coords2D::new(max(a.x, b.x), max(a.y, b.y));
        Area2D {
            min: min_point,
            max: max_point,
        }
    }

    pub fn contains(&self, point: Coords2D<T>) -> bool {
        point.x >= self.min.x
            && point.x <= self.max.x
            && point.y >= self.min.y
            && point.y <= self.max.y
    }

    pub fn points(self) -> Vec<Coords2D<T>> {
        (self.min.x ..= self.max.x)
            .collect::<Vec<_>>()
            .into_iter()
            .flat_map(|x| {
                (self.min.y ..= self.max.y)
                    .collect::<Vec<_>>()
                    .into_iter()
                    .map(move |y| Coords2D::new(x, y))
            })
            .collect()
    }
}
