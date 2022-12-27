use num_derive::FromPrimitive;

#[repr(u8)]
#[derive(FromPrimitive, Debug)]
pub enum Rotation {
    Left90 = 0,
    Right90 = 1,
}
