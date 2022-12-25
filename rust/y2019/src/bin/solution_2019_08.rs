use itertools::Itertools;
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

type Number = usize;

#[repr(u8)]
#[derive(PartialEq, Eq, Clone, FromPrimitive)]
enum PixelType {
    Black = 0,
    White = 1,
    Transparent = 2,
}

fn parse_layers(data: &str, width: Number, height: Number) -> Vec<Vec<u8>> {
    let layer_size = width * height;
    assert_eq!(data.len() % layer_size, 0, "Invalid sizes");

    let numbers: Vec<_> = data.chars().map(|x| (x as u8) - b'0').collect();
    let layers: Vec<_> = numbers
        .chunks_exact(layer_size)
        .map(<[u8]>::to_vec)
        .collect();
    layers
}

fn solve_1(data: &str, width: Number, height: Number) -> Number {
    let layers = parse_layers(data, width, height);
    let layer = layers
        .iter()
        .min_by_key(|v| v.iter().filter(|&&x| x == 0).count())
        .unwrap();
    layer.iter().filter(|&&x| x == 1).count() * layer.iter().filter(|&&x| x == 2).count()
}

fn solve_2(data: &str, width: Number, height: Number) -> String {
    let layers = parse_layers(data, width, height);
    let size = width * height;
    let resulting: Vec<PixelType> = (0..size)
        .map(|pixel_idx| {
            layers
                .iter()
                .map(|layer| FromPrimitive::from_u8(layer[pixel_idx]).unwrap())
                .find(|x| *x != PixelType::Transparent)
                .unwrap_or(PixelType::Black)
        })
        .collect();
    resulting
        .chunks_exact(width)
        .map(|x| {
            x.iter()
                .map(|pixel| match pixel {
                    PixelType::White => '█',
                    PixelType::Black => ' ',
                    PixelType::Transparent => '?',
                })
                .join("")
        })
        .join("\n")
}

fn main() {
    let data = include_str!("../../resources/08.txt").trim();
    let part_1 = solve_1(data, 25, 6);
    assert_eq!(part_1, 1965);
    println!("Part 1: {part_1}");

    let part_2 = solve_2(data, 25, 6);
    println!("Part 2:\n{part_2}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_day_8_part_1() {
        assert_eq!(solve_1("123456789012", 3, 2), 1);
    }
}
