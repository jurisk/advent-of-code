use std::collections::HashSet;

use advent_of_code_common::coords2d::Coords2D;
use advent_of_code_common::cryptography::md5_for_string;
use advent_of_code_common::direction::Direction;
use pathfinding::prelude::{bfs, bfs_reach};

type N = i32;

type Coords = Coords2D<N>;

struct Maze {
    prefix: String,
    finish: Coords,
}

#[derive(Clone, Eq, PartialEq, Hash)]
struct State {
    location: Coords,
    moves:    Vec<Direction>,
}

impl State {
    fn moves_as_string(&self) -> String {
        self.moves.iter().map(|d| d.as_udlr_char()).collect()
    }
}

fn valid(ch: Option<char>) -> bool {
    if let Some(c) = ch {
        matches!(c, 'b' | 'c' | 'd' | 'e' | 'f')
    } else {
        false
    }
}

impl Maze {
    fn new(prefix: &str) -> Self {
        Self {
            prefix: prefix.to_string(),
            finish: Coords::new(3, 3),
        }
    }

    fn start_state() -> State {
        State {
            location: Coords::new(0, 0),
            moves:    vec![],
        }
    }

    fn is_target_state(&self, state: &State) -> bool {
        self.finish == state.location
    }

    fn within_bounds(&self, c: Coords) -> bool {
        c.x >= 0 && c.y >= 0 && c.x <= self.finish.x && c.y <= self.finish.y
    }

    fn neighbours(&self, state: &State) -> Vec<State> {
        if self.is_target_state(state) {
            vec![]
        } else {
            let history_string = format!("{}{}", self.prefix, state.moves_as_string());
            let hash = md5_for_string(&history_string);
            let mut hash_chars = hash.chars();

            let mut candidate_directions: HashSet<Direction> = HashSet::new();
            if valid(hash_chars.next()) {
                candidate_directions.insert(Direction::North);
            }

            if valid(hash_chars.next()) {
                candidate_directions.insert(Direction::South);
            }

            if valid(hash_chars.next()) {
                candidate_directions.insert(Direction::West);
            }

            if valid(hash_chars.next()) {
                candidate_directions.insert(Direction::East);
            }

            candidate_directions
                .iter()
                .filter_map(|d| {
                    let n = state.location.move_in_direction(*d);
                    if self.within_bounds(n) {
                        let moves = vec![state.moves.clone(), vec![*d]].concat();
                        let result = State { location: n, moves };
                        Some(result)
                    } else {
                        None
                    }
                })
                .collect()
        }
    }
}

fn part_1(prefix: &str) -> Option<String> {
    let maze = Maze::new(prefix);
    let results = bfs(
        &Maze::start_state(),
        |x| maze.neighbours(x),
        |x| maze.is_target_state(x),
    );
    results.map(|path| path.last().unwrap().moves_as_string())
}

fn part_2(prefix: &str) -> Option<usize> {
    let maze = Maze::new(prefix);
    let results = bfs_reach(Maze::start_state(), |x| maze.neighbours(x)).collect::<Vec<_>>();
    results
        .iter()
        .filter(|x| maze.is_target_state(x))
        .map(|x| x.moves.len())
        .max()
}

const DATA: &str = "yjjvjgan";

fn main() {
    let result_1 = part_1(DATA);
    println!("Part 1: {result_1:?}");

    let result_2 = part_2(DATA);
    println!("Part 2: {result_2:?}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve_1_test_1() {
        assert_eq!(part_1("hijkl"), None);
    }

    #[test]
    fn test_solve_1_test_2() {
        assert_eq!(part_1("ihgpwlah"), Some("DDRRRD".to_string()));
    }

    #[test]
    fn test_solve_1_test_3() {
        assert_eq!(part_1("kglvqrro"), Some("DDUDRLRRUDRD".to_string()));
    }

    #[test]
    fn test_solve_1_test_4() {
        assert_eq!(
            part_1("ulqzkmiv"),
            Some("DRURDRUDDLLDLUURRDULRLDUUDDDRR".to_string())
        );
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Some("RLDRUDRDDR".to_string()));
    }

    #[test]
    fn test_solve_2_test_1() {
        assert_eq!(part_2("ihgpwlah"), Some(370));
    }

    #[test]
    fn test_solve_2_test_2() {
        assert_eq!(part_2("kglvqrro"), Some(492));
    }

    #[test]
    fn test_solve_2_test_3() {
        assert_eq!(part_2("ulqzkmiv"), Some(830));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Some(498));
    }
}
