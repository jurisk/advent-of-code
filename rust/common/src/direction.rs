use crate::coords2d::Coords2D;
use crate::rotation::Rotation;

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

    pub fn rotate_left(&self) -> Direction {
        self.rotate(Rotation::Left90)
    }

    pub fn rotate_right(&self) -> Direction {
        self.rotate(Rotation::Right90)
    }

    pub fn all() -> Vec<Direction> {
        vec![
            Direction::North,
            Direction::South,
            Direction::West,
            Direction::East,
        ]
    }

    pub fn diff(self) -> Coords2D {
        match self {
            Direction::North => Coords2D { x: 0, y: -1 },
            Direction::South => Coords2D { x: 0, y: 1 },
            Direction::West => Coords2D { x: -1, y: 0 },
            Direction::East => Coords2D { x: 1, y: 0 },
        }
    }
}
