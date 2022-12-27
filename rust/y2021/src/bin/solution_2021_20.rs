use advent_of_code_common::parsing::{
    parse_matrix, split_into_two_segments_separated_by_double_newline, Error,
};
use num_enum::{IntoPrimitive, TryFromPrimitive};
use pathfinding::matrix::Matrix;
use std::str::FromStr;

#[repr(u8)]
#[derive(Debug, Eq, PartialEq, Clone, TryFromPrimitive, IntoPrimitive, Copy)]
enum Pixel {
    Light = b'#',
    Dark = b'.',
}

impl Pixel {
    fn as_char(self) -> char {
        let as_u8: u8 = self.into();
        as_u8 as char
    }
}

fn parse_pixel(ch: char) -> Result<Pixel, Error> {
    Pixel::try_from(ch as u8).map_err(|err| format!("{err:?}"))
}

#[derive(Clone)]
struct ImageEnhancementAlgorithm {
    mapping: [Pixel; 512],
}

struct Image {
    pixels: Matrix<Pixel>,
    surrounding_pixels: Pixel,
}

impl Image {
    fn light_pixels(&self) -> Option<usize> {
        match self.surrounding_pixels {
            Pixel::Light => None,
            Pixel::Dark => Some(self.pixels.values().filter(|&&p| p == Pixel::Light).count()),
        }
    }

    #[allow(clippy::cast_sign_loss)]
    fn bit_at(&self, r: i32, c: i32) -> usize {
        let pixel = if r < 0 || c < 0 {
            &self.surrounding_pixels
        } else {
            self.pixels
                .get((r as usize, c as usize))
                .unwrap_or(&self.surrounding_pixels)
        };

        match pixel {
            Pixel::Light => 1,
            Pixel::Dark => 0,
        }
    }

    fn enhance_pixel(&self, r: i32, c: i32, algorithm: &ImageEnhancementAlgorithm) -> Pixel {
        let idx = self.bit_at(r - 1, c - 1) << 8
            | self.bit_at(r - 1, c) << 7
            | self.bit_at(r - 1, c + 1) << 6
            | self.bit_at(r, c - 1) << 5
            | self.bit_at(r, c) << 4
            | self.bit_at(r, c + 1) << 3
            | self.bit_at(r + 1, c - 1) << 2
            | self.bit_at(r + 1, c) << 1
            | self.bit_at(r + 1, c + 1);

        algorithm.mapping[idx]
    }

    fn enhance(&self, algorithm: &ImageEnhancementAlgorithm) -> Image {
        let mut pixels: Matrix<Pixel> =
            Matrix::new(self.pixels.rows + 2, self.pixels.columns + 2, Pixel::Dark);
        for r in 0..pixels.rows {
            for c in 0..pixels.columns {
                pixels[(r, c)] = self.enhance_pixel(
                    i32::try_from(r).unwrap() - 1,
                    i32::try_from(c).unwrap() - 1,
                    algorithm,
                );
            }
        }

        let surrounding_pixels = match self.surrounding_pixels {
            Pixel::Light => algorithm.mapping[0b1_1111_1111],
            Pixel::Dark => algorithm.mapping[0b0_0000_0000],
        };

        Image {
            pixels,
            surrounding_pixels,
        }
    }
}

struct Data {
    image_enhancement_algorithm: ImageEnhancementAlgorithm,
    image: Image,
}

impl Data {
    fn enhance(&self) -> Data {
        Data {
            image_enhancement_algorithm: self.image_enhancement_algorithm.clone(),
            image: self.image.enhance(&self.image_enhancement_algorithm),
        }
    }

    fn debug_print(&self) {
        println!(
            "With surrounding at: {}",
            self.image.surrounding_pixels.as_char()
        );
        for r in 0..self.image.pixels.rows {
            let s: String = (0..self.image.pixels.columns)
                .map(|c| self.image.pixels[(r, c)].as_char())
                .collect();
            println!("{s}");
        }
        println!();
    }
}

impl FromStr for Data {
    type Err = Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let (a, b) = split_into_two_segments_separated_by_double_newline(input)?;
        let mapping_vec_result: Result<Vec<_>, _> =
            a.chars().map(|ch| Pixel::try_from(ch as u8)).collect();
        let mapping_vec = mapping_vec_result.map_err(|err| format!("{err:?}"))?;
        let mapping: [Pixel; 512] = mapping_vec.try_into().map_err(|err| format!("{err:?}"))?;
        let image_enhancement_algorithm = ImageEnhancementAlgorithm { mapping };
        let pixels = parse_matrix(&b, parse_pixel)?;
        let surrounding_pixels = Pixel::Dark;
        let image = Image {
            pixels,
            surrounding_pixels,
        };

        Ok(Data {
            image_enhancement_algorithm,
            image,
        })
    }
}

fn solve(input: &str, times: usize) -> Result<usize, Error> {
    let mut data: Data = input.parse()?;
    for _ in 0..times {
        data = data.enhance();
    }
    data.debug_print();
    data.image
        .light_pixels()
        .ok_or_else(|| "Infinite".to_string())
}

const DATA: &str = include_str!("../../resources/20.txt");

fn main() {
    let result_1 = solve(DATA, 2);
    println!("Part 1: {result_1:?}");

    let result_2 = solve(DATA, 50);
    println!("Part 2: {result_2:?}");
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = include_str!("../../resources/20-test.txt");

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve(TEST_DATA, 2), Ok(35));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve(DATA, 2), Ok(5503));
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve(TEST_DATA, 50), Ok(3351));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve(DATA, 50), Ok(19156));
    }
}
