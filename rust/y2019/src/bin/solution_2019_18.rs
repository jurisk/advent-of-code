extern crate core;

use std::collections::{HashMap, HashSet};
use std::hash::Hash;

use advent_of_code_common::coords2d::Coords2D;
use pathfinding::prelude::{bfs, dijkstra};

const DATA: &str = include_str!("../../resources/18.txt");

type Coords = Coords2D<i32>;

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
            _ => panic!("Did not expect char {ch}"),
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
    positions:     Vec<Coords>,
    keys_obtained: DoorKeyPairSet,
}

struct Maze {
    field:     Vec<Vec<Square>>,
    entrances: Vec<Coords>,
}

fn update_element_at<T: Clone>(positions: &[T], position_idx: usize, new_position: T) -> Vec<T> {
    let mut positions = positions.to_vec();
    positions[position_idx] = new_position;
    positions
}

impl Maze {
    fn parse(data: &str) -> Maze {
        let chars: Vec<Vec<char>> = data
            .lines()
            .filter(|s| !s.is_empty())
            .map(|s| s.chars().collect())
            .collect();

        let field: Vec<Vec<Square>> = chars
            .iter()
            .map(|r| r.iter().map(|ch| Square::create(*ch)).collect())
            .collect();

        let entrances: Vec<Coords> = (0 .. chars.len())
            .flat_map(|y| {
                let r = &chars[y];
                (0 .. r.len()).filter_map(move |x| {
                    if r[x] == '@' {
                        Some(Coords::new(
                            i32::try_from(x).unwrap(),
                            i32::try_from(y).unwrap(),
                        ))
                    } else {
                        None
                    }
                })
            })
            .collect();

        Maze { field, entrances }
    }

    #[allow(clippy::cast_sign_loss)]
    fn at(&self, coords: Coords) -> Square {
        self.field[coords.y as usize][coords.x as usize]
    }

    fn neighbours_only_empty_squares(&self, c: Coords, goal: Coords) -> Vec<Coords> {
        c.adjacent4()
            .iter()
            .filter(|n| {
                if goal == **n {
                    true
                } else {
                    let sq = self.at(**n);
                    match sq {
                        Square::Empty => true,
                        Square::Key(_) | Square::Door(_) | Square::Wall => false,
                    }
                }
            })
            .copied()
            .collect()
    }

    fn distance_only_using_empty_squares(&self, from: Coords, to: Coords) -> Option<usize> {
        bfs(
            &from,
            |sq| self.neighbours_only_empty_squares(*sq, to),
            |sq| *sq == to,
        )
        .map(|v| v.len() - 1)
    }

    fn all_coords(&self) -> Vec<Coords> {
        (0 .. self.field.len())
            .flat_map(|y| {
                let r = &self.field[y];
                (0 .. r.len()).map(move |x| {
                    Coords {
                        x: i32::try_from(x).unwrap(),
                        y: i32::try_from(y).unwrap(),
                    }
                })
            })
            .collect()
    }
}

struct Graph {
    entrances: Vec<Coords>,
    vertices:  HashMap<Coords, Square>,
    distances: HashMap<(Coords, Coords), usize>,
    all_keys:  DoorKeyPairSet,
}

impl Graph {
    fn create(maze: &Maze) -> Graph {
        let entrances = maze.entrances.clone();

        let keys: HashSet<DoorKeyPair> = maze
            .field
            .iter()
            .flat_map(|r| {
                r.iter().filter_map(|sq| {
                    match sq {
                        Square::Key(ch) => Some(*ch),
                        _ => None,
                    }
                })
            })
            .collect();

        let mut all_keys = DoorKeyPairSet::new();
        for key in keys {
            all_keys = all_keys.add(key);
        }

        let vertices: HashMap<Coords, Square> = maze
            .all_coords()
            .iter()
            .map(|c| (c, maze.at(*c)))
            .filter(|(c, sq)| {
                match sq {
                    Square::Wall => false,
                    Square::Empty => entrances.contains(c),
                    Square::Key(_) | Square::Door(_) => true,
                }
            })
            .map(|(c, sq)| (*c, sq))
            .collect();

        println!("vertices length {:?}", vertices.len());

        let mut distances: HashMap<(Coords, Coords), usize> = HashMap::new();
        for from in vertices.keys() {
            for to in vertices.keys() {
                if *from != *to {
                    if let Some(cost) = maze.distance_only_using_empty_squares(*from, *to) {
                        // we insert in both directions as it is the same distance
                        distances.insert((*from, *to), cost);
                        distances.insert((*to, *from), cost);
                    }
                }
            }
        }

        Graph {
            entrances,
            vertices,
            distances,
            all_keys,
        }
    }

    fn at(&self, coords: Coords) -> Square {
        self.vertices[&coords]
    }

    fn start_state(&self) -> State {
        State {
            positions:     self.entrances.clone(),
            keys_obtained: DoorKeyPairSet::new(),
        }
    }

    fn successors_from_position(&self, state: &State, position_idx: usize) -> Vec<(State, usize)> {
        let from_position = state.positions[position_idx];
        let candidates: Vec<(Coords, usize)> = self
            .distances
            .iter()
            .filter(|((from, _), _)| *from == from_position)
            .map(|((_, to), cost)| (*to, *cost))
            .collect();

        candidates
            .iter()
            .filter_map(|(to, cost)| {
                let positions = update_element_at(&state.positions, position_idx, *to);
                let new_keys = match self.at(*to) {
                    Square::Empty => Some(state.keys_obtained),
                    Square::Key(k) if state.keys_obtained.contains(k) => Some(state.keys_obtained),
                    Square::Key(k) => Some(state.keys_obtained.add(k)),
                    Square::Door(d) if state.keys_obtained.contains(d) => Some(state.keys_obtained),
                    Square::Door(_) | Square::Wall => None,
                };

                new_keys.map(|keys_obtained| {
                    (
                        State {
                            positions,
                            keys_obtained,
                        },
                        *cost,
                    )
                })
            })
            .collect()
    }

    fn successors(&self, state: &State) -> Vec<(State, usize)> {
        (0 .. state.positions.len())
            .flat_map(|position_idx| self.successors_from_position(state, position_idx))
            .collect()
    }

    fn is_finished(&self, state: &State) -> bool {
        state.keys_obtained == self.all_keys
    }
}

fn solve(data: &str) -> Option<usize> {
    let maze = Maze::parse(data);
    let graph = Graph::create(&maze);

    dijkstra(
        &graph.start_state(),
        |s| graph.successors(s),
        |s| graph.is_finished(s),
    )
    .map(|(_, cost)| cost)
}

fn hack_line(data: &str, what: &str, idx_at: usize) -> String {
    let mut result: String = String::new();
    result.push_str(&data[0 .. idx_at]);
    result.push_str(what);
    result.push_str(&data[(idx_at + what.len()) ..]);
    result
}

fn hack_to_convert_part_1_to_part_2(data: &str) -> String {
    let mut lines: Vec<&str> = data.lines().collect();
    let l_39 = &hack_line(lines[39], "@#@", 39);
    lines[39] = l_39;
    let l_40 = &hack_line(lines[40], "###", 39);
    lines[40] = l_40;
    let l_41 = &hack_line(lines[41], "@#@", 39);
    lines[41] = l_41;

    lines.join("\n")
}

fn main() {
    let result = solve(DATA);
    println!("Part 1: {result:?}");

    let result = solve(&hack_to_convert_part_1_to_part_2(DATA));
    println!("Part 2: {result:?}");
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
    #[ignore = "Slow"]
    fn test_solve_2_real() {
        assert_eq!(solve(&hack_to_convert_part_1_to_part_2(DATA)), Some(1992));
    }
}
