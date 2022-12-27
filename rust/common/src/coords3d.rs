#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub struct Coords3D<T> {
    pub x: T,
    pub y: T,
    pub z: T,
}

impl<T> Coords3D<T> {
    #[must_use]
    #[inline]
    pub fn new(x: T, y: T, z: T) -> Coords3D<T> {
        Coords3D { x, y, z }
    }
}
