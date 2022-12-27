use std::ops::Add;

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub struct Coords2D<T> {
    pub x: T,
    pub y: T,
}

impl<T : Add<Output = T>> Add for Coords2D<T> {
    type Output = Coords2D<T>;

    fn add(self, rhs: Coords2D<T>) -> Coords2D<T> {
        Coords2D {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}
