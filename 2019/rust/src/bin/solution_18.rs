extern crate core;

use pathfinding::prelude::{bfs, dijkstra};
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

#[derive(PartialEq, Eq, Debug, Clone, Copy, Hash)]
struct DoorKeyPairSet {
    data: u32,
}

impl DoorKeyPairSet {
    fn bit_repr(p: DoorKeyPair) -> u32 {
        let how_many = p as u32 - 'A' as u32;
        1_u32 << how_many
    }

    fn add(self, p: DoorKeyPair) -> DoorKeyPairSet {
        DoorKeyPairSet {
            data: self.data | DoorKeyPairSet::bit_repr(p),
        }
    }

    fn contains(self, p: DoorKeyPair) -> bool {
        (DoorKeyPairSet::bit_repr(p) & self.data) != 0
    }
}

impl DoorKeyPairSet {
    fn new() -> DoorKeyPairSet {
        DoorKeyPairSet { data: 0 }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
struct State {
    positions: Vec<CoordsXY>,
    keys_obtained: DoorKeyPairSet,
}

struct Maze {
    field: Vec<Vec<Square>>,
    entrances: Vec<CoordsXY>,
    all_keys: DoorKeyPairSet,
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

        let keys: HashSet<DoorKeyPair> = field
            .iter()
            .flat_map(|r| {
                r.iter().filter_map(|sq| match sq {
                    Square::Key(ch) => Some(*ch),
                    _ => None,
                })
            })
            .collect();

        let mut all_keys = DoorKeyPairSet::new();
        for key in keys {
            all_keys = all_keys.add(key);
        }

        Maze {
            field,
            entrances,
            all_keys,
        }
    }

    fn start_state(&self) -> State {
        State {
            positions: self.entrances.clone(),
            keys_obtained: DoorKeyPairSet::new(),
        }
    }

    fn is_finished(&self, state: &State) -> bool {
        state.keys_obtained == self.all_keys
    }

    fn at(&self, coords: CoordsXY) -> Square {
        self.field[coords.y as usize][coords.x as usize]
    }

    // TODO: belongs elsewhere, perhaps in State
    fn update_positions(
        positions: &[CoordsXY],
        position_idx: usize,
        new_position: CoordsXY,
    ) -> Vec<CoordsXY> {
        let mut positions = positions.to_vec();
        positions[position_idx] = new_position;
        positions
    }

    fn can_pass(&self, position: CoordsXY, keys_held: DoorKeyPairSet) -> bool {
        match self.at(position) {
            Square::Key(_) | Square::Empty => true,
            Square::Door(d) if keys_held.contains(d) => true,
            Square::Door(_) | Square::Wall => false,
        }
    }

    fn simple_successors(&self, position: CoordsXY, keys_held: DoorKeyPairSet) -> Vec<CoordsXY> {
        position
            .neighbours()
            .iter()
            .filter(|n| self.can_pass(**n, keys_held))
            .copied()
            .collect::<Vec<_>>()
    }

    fn cost_from_to(
        &self,
        from: CoordsXY,
        to: CoordsXY,
        keys_held: DoorKeyPairSet,
    ) -> Option<usize> {
        bfs(
            &from,
            |c| self.simple_successors(*c, keys_held),
            |c| *c == to,
        )
        .map(|path| path.len() - 1)
    }

    fn all_coords(&self) -> Vec<CoordsXY> {
        (0..self.field.len())
            .flat_map(|y| {
                let r = &self.field[y];
                (0..r.len()).map(move |x| CoordsXY {
                    x: x as i32,
                    y: y as i32,
                })
            })
            .collect()
    }

    fn positions_for_keys_not_held_yet(
        &self,
        keys_held: DoorKeyPairSet,
    ) -> Vec<(CoordsXY, DoorKeyPair)> {
        self.all_coords()
            .iter()
            .filter_map(|c| match self.at(*c) {
                Square::Empty | Square::Door(_) | Square::Wall => None,
                Square::Key(k) if keys_held.contains(k) => None,
                Square::Key(k) => Some((*c, k)),
            })
            .collect()
    }

    fn reachable_keys(
        &self,
        from_position: CoordsXY,
        keys_held: DoorKeyPairSet,
    ) -> Vec<(CoordsXY, DoorKeyPair, usize)> {
        self.positions_for_keys_not_held_yet(keys_held)
            .iter()
            .filter_map(|(to_position, key)| {
                let cost = self.cost_from_to(from_position, *to_position, keys_held);
                cost.map(|cost| (*to_position, *key, cost))
            })
            .collect()
    }

    fn successors_from_position(&self, state: &State, position_idx: usize) -> Vec<(State, usize)> {
        let from_position = state.positions[position_idx];
        let reachable_keys_with_distances: Vec<(CoordsXY, DoorKeyPair, usize)> =
            self.reachable_keys(from_position, state.keys_obtained);
        reachable_keys_with_distances
            .iter()
            .map(|(coords, key, cost)| {
                let new_state = State {
                    positions: Maze::update_positions(&state.positions, position_idx, *coords),
                    keys_obtained: state.keys_obtained.add(*key),
                };
                (new_state, *cost)
            })
            .collect()
    }

    fn successors(&self, state: &State) -> Vec<(State, usize)> {
        (0..state.positions.len())
            .flat_map(|position_idx| self.successors_from_position(state, position_idx))
            .collect()
    }
}

fn solve(data: &str) -> Option<usize> {
    let maze = Maze::parse(data);
    dijkstra(
        &maze.start_state(),
        |s| maze.successors(s),
        |s| maze.is_finished(s),
    )
    .map(|(_, cost)| cost)
}

fn hack_line(data: &str, what: &str, idx_at: usize) -> String {
    let mut result: String = String::new();
    result.push_str(&data[0..idx_at]);
    result.push_str(what);
    result.push_str(&data[(idx_at + what.len())..]);
    result
}

fn hack_to_convert_part_1_to_part_2(data: &str) -> String {
    let mut lines: Vec<&str> = data.split('\n').collect();
    let l_39 = &hack_line(lines[39], "@#@", 39);
    lines[39] = l_39;
    let l_40 = &hack_line(lines[40], "###", 39);
    lines[40] = l_40;
    let l_41 = &hack_line(lines[41], "@#@", 39);
    lines[41] = l_41;
    let result = lines.join("\n");
    assert_eq!(data.len(), result.len());
    result
}

fn main() {
    let result = solve(DATA);
    println!("Part 1: {:?}", result);

    let result = solve(&hack_to_convert_part_1_to_part_2(DATA));
    println!("Part 2: {:?}", result);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve_1_1() {
        let data = "#########
#b.A.@.a#
#########";

        assert_eq!(solve(data), Some(8));
    }

    #[test]
    fn test_solve_1_2() {
        let data = "########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################";

        assert_eq!(solve(data), Some(86));
    }

    #[test]
    fn test_solve_1_3() {
        let data = "########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################";

        assert_eq!(solve(data), Some(132));
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

        assert_eq!(solve(data), Some(136));
    }

    #[test]
    fn test_solve_1_5() {
        let data = "########################
#@..............ac.GI.b#
###d#e#f################
###A#B#C################
###g#h#i################
########################";

        assert_eq!(solve(data), Some(81));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve(DATA), Some(5808));
    }

    #[test]
    fn test_solve_2_1() {
        let data = "#######
#a.#Cd#
##@#@##
#######
##@#@##
#cB#Ab#
#######";
        assert_eq!(solve(data), Some(8));
    }

    #[test]
    fn test_solve_2_2() {
        let data = "###############
#d.ABC.#.....a#
######@#@######
###############
######@#@######
#b.....#.....c#
###############";
        assert_eq!(solve(data), Some(24));
    }

    #[test]
    fn test_solve_2_3() {
        let data = "#############
#DcBa.#.GhKl#
#.###@#@#I###
#e#d#####j#k#
###C#@#@###J#
#fEbA.#.FgHi#
#############";
        assert_eq!(solve(data), Some(32));
    }

    #[test]
    fn test_solve_2_4() {
        let data = "#############
#g#f.D#..h#l#
#F###e#E###.#
#dCba@#@BcIJ#
#############
#nK.L@#@G...#
#M###N#H###.#
#o#m..#i#jk.#
#############";
        assert_eq!(solve(data), Some(72));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(
            solve(&hack_to_convert_part_1_to_part_2(DATA)),
            Some(123_456)
        );
    }
}
