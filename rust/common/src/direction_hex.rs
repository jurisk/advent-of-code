use std::fmt::Debug;
use std::ops::Neg;
use std::str::FromStr;

use num_traits::{One, Signed, Zero};

use crate::coords_hex::CoordsHex;
use crate::direction_hex::DirectionHex::{
    North, NorthEast, NorthWest, South, SouthEast, SouthWest,
};
use crate::parsing::Error;

pub enum DirectionHex {
    NorthWest,
    North,
    NorthEast,
    SouthWest,
    South,
    SouthEast,
}

impl DirectionHex {
    #[must_use]
    pub fn diff<T>(self) -> CoordsHex<T>
    where
        T: Zero + One + Neg<Output = T> + Copy + Eq + Signed + Debug,
    {
        let l = T::one();
        let o = T::zero();
        match self {
            NorthWest => CoordsHex::new(l, -l, o),
            North => CoordsHex::new(l, o, -l),
            NorthEast => CoordsHex::new(o, l, -l),
            SouthWest => CoordsHex::new(o, -l, l),
            South => CoordsHex::new(-l, o, l),
            SouthEast => CoordsHex::new(-l, l, o),
        }
    }
}

impl FromStr for DirectionHex {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "nw" => Ok(NorthWest),
            "n" => Ok(North),
            "ne" => Ok(NorthEast),
            "sw" => Ok(SouthWest),
            "s" => Ok(South),
            "se" => Ok(SouthEast),
            _ => Err(format!("Unrecognized hex direction {s}")),
        }
    }
}
