#[repr(u8)]
#[derive(Debug)]
pub enum Rotation {
    Left90     = b'L',
    Right90    = b'R',
    NoRotation = b'|',
    TurnAround = b'^',
}
