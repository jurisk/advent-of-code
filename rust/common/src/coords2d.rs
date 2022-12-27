use std::ops::Add;

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub struct Coords2D {
    pub x: i32,
    pub y: i32,
}

impl Add for Coords2D {
    type Output = Coords2D;

    fn add(self, rhs: Coords2D) -> Coords2D {
        Coords2D {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}
