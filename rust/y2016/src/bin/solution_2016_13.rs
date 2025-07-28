use advent_of_code_common::coords2d::Coords2D;
use advent_of_code_common::parsing::Error;
use itertools::Itertools;
use pathfinding::prelude::{bfs, bfs_reach};

type N = i32;

type Coords = Coords2D<N>;

struct Maze {
    favourite_number: N,
}

impl Maze {
    fn is_open(&self, c: Coords) -> bool {
        if c.x < 0 || c.y < 0 {
            false
        } else {
            let x = c.x * c.x + 3 * c.x + 2 * c.x * c.y + c.y + c.y * c.y + self.favourite_number;
            x.count_ones().is_multiple_of(2)
        }
    }

    fn neighbours(&self, c: Coords) -> Vec<Coords> {
        c.adjacent4()
            .iter()
            .filter(|x| self.is_open(**x))
            .copied()
            .collect()
    }
}

fn part_1(favourite_number: N, target: Coords) -> Result<usize, Error> {
    let maze = Maze { favourite_number };
    let source = Coords::new(1, 1);

    bfs(&source, |x| maze.neighbours(*x), |x| *x == target)
        .map(|x| x.len() - 1)
        .ok_or_else(|| "not found".to_string())
}

#[derive(Clone, Eq, PartialEq, Hash)]
struct State {
    location: Coords,
    moves:    usize,
}

fn part_2(favourite_number: N, limit_inclusive: usize) -> usize {
    let maze = Maze { favourite_number };
    let start = State {
        location: Coords::new(1, 1),
        moves:    0,
    };

    // Could do it more efficiently as it revisits seen coordinates, but this works.
    bfs_reach(start, |state| {
        if state.moves >= limit_inclusive {
            vec![]
        } else {
            maze.neighbours(state.location)
                .iter()
                .map(|n| {
                    State {
                        location: *n,
                        moves:    state.moves + 1,
                    }
                })
                .collect()
        }
    })
    .map(|x| x.location)
    .unique()
    .count()
}

fn main() -> Result<(), Error> {
    let result_1 = part_1(1352, Coords::new(31, 39))?;
    println!("Part 1: {result_1:?}");

    let result_2 = part_2(1352, 50);
    println!("Part 2: {result_2:?}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve_1_test() {
        assert_eq!(part_1(10, Coords::new(7, 4)), Ok(11));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(1352, Coords::new(31, 39)), Ok(90));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(1352, 50), 135);
    }
}
