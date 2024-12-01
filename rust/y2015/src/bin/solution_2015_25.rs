use advent_of_code_common::coords2d::Coords2D;
use advent_of_code_common::math::{mod_pow, sum_of_standard_arithmetic_progression};

type N = u64;

fn index(coords: &Coords2D<N>) -> N {
    sum_of_standard_arithmetic_progression(coords.x + coords.y - 2) + coords.x
}

fn code(index: N) -> N {
    const A1: u64 = 20_151_125;
    const BASE: u64 = 252_533;
    const M: u64 = 33_554_393;
    A1 * mod_pow(BASE, index - 1, M) % M
}

fn solve(coords: &Coords2D<N>) -> N {
    code(index(coords))
}

const ROW: N = 2981;
const COLUMN: N = 3075;

fn main() {
    let data: Coords2D<N> = Coords2D::new(COLUMN, ROW);
    let result = solve(&data);
    println!("{result}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_code() {
        assert_eq!(code(1), 20_151_125);
        assert_eq!(code(2), 31_916_031);
        assert_eq!(code(3), 18_749_137);
    }

    #[test]
    fn test_index() {
        assert_eq!(index(&Coords2D::new(1, 1)), 1);
        assert_eq!(index(&Coords2D::new(4, 1)), 10);
        assert_eq!(index(&Coords2D::new(2, 5)), 17);
        assert_eq!(index(&Coords2D::new(4, 3)), 19);
    }

    #[test]
    fn test_solve() {
        assert_eq!(solve(&Coords2D::new(1, 1)), 20_151_125);
        assert_eq!(solve(&Coords2D::new(4, 1)), 30_943_339);
        assert_eq!(solve(&Coords2D::new(2, 5)), 17_552_253);
        assert_eq!(solve(&Coords2D::new(4, 3)), 7_981_243);
    }

    #[test]
    fn test_solve_real() {
        let data: Coords2D<N> = Coords2D::new(COLUMN, ROW);
        assert_eq!(solve(&data), 9_132_360);
    }
}
