use num_derive::FromPrimitive;

#[repr(u8)]
#[derive(FromPrimitive, Debug)]
pub enum Rotation {
    Left90     = b'L',
    Right90    = b'R',
    NoRotation = b'|',
    TurnAround = b'^',
}
