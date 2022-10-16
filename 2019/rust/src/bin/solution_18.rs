extern crate core;

use pathfinding::prelude::bfs;
use std::collections::HashSet;
use std::hash::Hash;

const DATA: &str = include_str!("../../resources/18.txt");

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
struct CoordsXY {
    x: i32,
    y: i32,
}

impl CoordsXY {
    fn new(x: i32, y: i32) -> CoordsXY {
        CoordsXY { x, y }
    }

    fn neighbours(self) -> Vec<CoordsXY> {
        vec![
            CoordsXY {
                x: self.x,
                y: self.y + 1,
            },
            CoordsXY {
                x: self.x,
                y: self.y - 1,
            },
            CoordsXY {
                x: self.x + 1,
                y: self.y,
            },
            CoordsXY {
                x: self.x - 1,
                y: self.y,
            },
        ]
    }
}

type DoorKeyPair = char;

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
enum Square {
    Wall,
    Empty,
    Key(DoorKeyPair),
    Door(DoorKeyPair),
}

impl Square {
    fn create(ch: char) -> Square {
        match ch {
            '#' => Square::Wall,
            '.' | '@' => Square::Empty,
            d if d.is_ascii_uppercase() => Square::Door(d),
            k if k.is_ascii_lowercase() => Square::Key(k.to_ascii_uppercase()),
            _ => panic!("Did not expect char {}", ch),
        }
    }
}

struct Maze {
    field: Vec<Vec<Square>>,
    key_count: usize,
    entrance: CoordsXY,
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
struct State {
    position: CoordsXY,
    keys_obtained: Vec<DoorKeyPair>,
}

impl Maze {
    fn parse(data: &str) -> Maze {
        let chars: Vec<Vec<char>> = data
            .split('\n')
            .filter(|s| !s.is_empty())
            .map(|s| s.chars().collect())
            .collect();

        let field: Vec<Vec<Square>> = chars
            .iter()
            .map(|r| r.iter().map(|ch| Square::create(*ch)).collect())
            .collect();

        let entrances: Vec<CoordsXY> = (0..chars.len())
            .flat_map(|y| {
                let r = &chars[y];
                (0..r.len()).filter_map(move |x| {
                    if r[x] == '@' {
                        Some(CoordsXY::new(x as i32, y as i32))
                    } else {
                        None
                    }
                })
            })
            .collect();

        assert_eq!(entrances.len(), 1);

        let keys: HashSet<DoorKeyPair> = field
            .iter()
            .flat_map(|r| {
                r.iter().filter_map(|sq| match sq {
                    Square::Key(ch) => Some(*ch),
                    _ => None,
                })
            })
            .collect();

        Maze {
            field,
            key_count: keys.len(),
            entrance: entrances[0],
        }
    }

    fn start_state(&self) -> State {
        State {
            position: self.entrance,
            keys_obtained: vec![],
        }
    }

    fn is_finished(&self, state: &State) -> bool {
        state.keys_obtained.len() == self.key_count
    }

    fn at(&self, coords: CoordsXY) -> Square {
        self.field[coords.y as usize][coords.x as usize]
    }

    fn can_go_from_to(&self, state: &State, position: CoordsXY) -> Option<State> {
        match self.at(position) {
            Square::Empty => Some(State {
                position,
                keys_obtained: state.keys_obtained.clone(),
            }),
            Square::Key(k) if state.keys_obtained.contains(&k) => Some(State {
                position,
                keys_obtained: state.keys_obtained.clone(),
            }),
            Square::Key(k) => Some(State {
                position,
                keys_obtained: vec![state.keys_obtained.clone(), vec![k]].concat(),
            }),
            Square::Door(d) if state.keys_obtained.contains(&d) => Some(State {
                position,
                keys_obtained: state.keys_obtained.clone(),
            }),
            Square::Door(_) | Square::Wall => None,
        }
    }

    fn successors(&self, state: &State) -> Vec<State> {
        state
            .position
            .neighbours()
            .iter()
            .filter_map(|n| self.can_go_from_to(state, *n))
            .collect()
    }
}

fn solve_1(data: &str) -> Option<usize> {
    let maze = Maze::parse(data);
    bfs(
        &maze.start_state(),
        |s| maze.successors(s),
        |s| maze.is_finished(s),
    )
    .map(|s| s.len() - 1)
}

fn main() {
    let result = solve_1(DATA);
    println!("Part 1: {:?}", result);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve_1_1() {
        let data = "#########
#b.A.@.a#
#########";

        assert_eq!(solve_1(data), Some(8));
    }

    #[test]
    fn test_solve_1_2() {
        let data = "########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################";

        assert_eq!(solve_1(data), Some(86));
    }

    #[test]
    fn test_solve_1_3() {
        let data = "########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################";

        assert_eq!(solve_1(data), Some(132));
    }

    #[test]
    fn test_solve_1_4() {
        let data = "#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################";

        assert_eq!(solve_1(data), Some(136));
    }

    #[test]
    fn test_solve_1_5() {
        let data = "########################
#@..............ac.GI.b#
###d#e#f################
###A#B#C################
###g#h#i################
########################";

        assert_eq!(solve_1(data), Some(81));
    }

    #[test]
    fn test_solve_1_6() {
        assert_eq!(solve_1(DATA), Some(123_456));
    }
}
