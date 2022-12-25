use bimap::BiMap;
use itertools::Itertools;
use pathfinding::prelude::bfs;
use std::collections::HashMap;

#[derive(Eq, PartialEq, Hash, Copy, Clone, Debug)]
struct CoordsXY {
    x: i32,
    y: i32,
}

impl CoordsXY {
    fn all_directions(self) -> Vec<CoordsXY> {
        vec![
            CoordsXY {
                x: self.x + 1,
                y: self.y,
            },
            CoordsXY {
                x: self.x - 1,
                y: self.y,
            },
            CoordsXY {
                x: self.x,
                y: self.y + 1,
            },
            CoordsXY {
                x: self.x,
                y: self.y - 1,
            },
        ]
    }
}

impl CoordsXY {
    fn with_level(self, level: u32) -> CoordsXYL {
        CoordsXYL {
            x: self.x,
            y: self.y,
            level,
        }
    }
}

impl CoordsXY {
    fn outermost(self) -> CoordsXYL {
        CoordsXYL {
            x: self.x,
            y: self.y,
            level: 0,
        }
    }
}

#[derive(Eq, PartialEq, Hash, Copy, Clone, Debug)]
struct CoordsXYL {
    x: i32,
    y: i32,
    level: u32,
}

#[derive(Debug)]
struct Maze {
    open_passages: Vec<Vec<bool>>,
    start: CoordsXY,
    end: CoordsXY,
    portals: BiMap<CoordsXY, CoordsXY>, // left side is outer - right side is inner
}

const PASSAGE: char = '.';

type Name = [char; 2];

impl Maze {
    fn build_temp_portals(chars: &Vec<Vec<char>>) -> Vec<(Name, CoordsXY)> {
        let mut temp_portals: Vec<(Name, CoordsXY)> = Vec::new();

        #[allow(clippy::needless_range_loop)]
        for y in 0..chars.len() {
            let r = &chars[y];
            for x in 0..r.len() - 2 {
                let row = &chars[y];
                if row[x].is_ascii_uppercase()
                    && row[x + 1].is_ascii_uppercase()
                    && row[x + 2] == PASSAGE
                {
                    let name: [char; 2] = [row[x], row[x + 1]];
                    let coords = CoordsXY {
                        x: (x + 2 - 2) as i32,
                        y: (y - 2) as i32,
                    };
                    temp_portals.push((name, coords));
                }

                if row[x] == PASSAGE
                    && row[x + 1].is_ascii_uppercase()
                    && row[x + 2].is_ascii_uppercase()
                {
                    let name: [char; 2] = [row[x + 1], row[x + 2]];
                    let coords = CoordsXY {
                        x: (x - 2) as i32,
                        y: (y - 2) as i32,
                    };
                    temp_portals.push((name, coords));
                }
            }
        }

        for y in 0..chars.len() - 2 {
            let r_plus_0 = &chars[y];
            let r_plus_1 = &chars[y + 1];
            let r_plus_2 = &chars[y + 2];
            for x in 2..r_plus_0.len() - 2 {
                if r_plus_0[x].is_ascii_uppercase()
                    && r_plus_1[x].is_ascii_uppercase()
                    && r_plus_2[x] == PASSAGE
                {
                    let name: [char; 2] = [r_plus_0[x], r_plus_1[x]];
                    let coords = CoordsXY {
                        x: (x - 2) as i32,
                        y: y as i32,
                    };
                    temp_portals.push((name, coords));
                }

                if r_plus_0[x] == PASSAGE
                    && r_plus_1[x].is_ascii_uppercase()
                    && r_plus_2[x].is_ascii_uppercase()
                {
                    let name: [char; 2] = [r_plus_1[x], r_plus_2[x]];
                    let coords = CoordsXY {
                        x: (x - 2) as i32,
                        y: (y - 2) as i32,
                    };
                    temp_portals.push((name, coords));
                }
            }
        }

        temp_portals
    }

    fn new(data: &str) -> Maze {
        let rows: Vec<&str> = data.lines().collect();

        let width = rows[2].len() - 2; // we assume the first row of the maze never has a portal on the right side

        let chars: Vec<Vec<char>> = rows
            .iter()
            .filter(|x| !x.is_empty())
            .map(|x| x.chars().pad_using(width + 4, |_| ' ').collect())
            .collect();

        let height = chars.len() - 4; // we assume there are always portals on the top & the bottom

        println!("width = {width}, height = {height}");

        let open_passages: Vec<Vec<bool>> = (0..height)
            .map(|y| (0..width).map(|x| chars[y + 2][x + 2] == PASSAGE).collect())
            .collect();

        assert_eq!(open_passages.len(), height);
        for row in &open_passages {
            assert_eq!(row.len(), width);
        }

        let temp_portals = Maze::build_temp_portals(&chars);

        let mut portals = BiMap::new();
        let mut temporary_map: HashMap<Name, CoordsXY> = HashMap::new();
        for (name, coords) in temp_portals {
            match temporary_map.remove(&name) {
                None => {
                    println!("Found {name:?} at {coords:?}");
                    temporary_map.insert(name, coords);
                },
                Some(existing) => {
                    if (coords.x == 0 || coords.x == (width - 1) as i32)
                        || (coords.y == 0 || coords.y == (height - 1) as i32)
                    {
                        println!("Matched {name:?} as outer {coords:?} and inner {existing:?}");
                        portals.insert(coords, existing);
                    } else {
                        println!("Matched {name:?} as outer {existing:?} and inner {coords:?}");
                        portals.insert(existing, coords);
                    }
                },
            }
        }

        assert_eq!(temporary_map.len(), 2); // only the start & finish coordinates left

        Maze {
            open_passages,
            start: temporary_map[&['A', 'A']],
            end: temporary_map[&['Z', 'Z']],
            portals,
        }
    }

    fn passage_at(&self, c: CoordsXY) -> bool {
        self.open_passages
            .get(c.y as usize)
            .map(|r| r.get(c.x as usize))
            == Some(Some(&true))
    }

    fn part_1_rules_neighbours(&self, from: CoordsXY) -> Vec<CoordsXY> {
        let a_binding = self.portals.get_by_left(&from).copied();
        let a: Vec<CoordsXY> = a_binding.iter().copied().collect();
        let b_binding = self.portals.get_by_right(&from).copied();
        let b: Vec<CoordsXY> = b_binding.iter().copied().collect();
        let neighbours = from.all_directions();
        let c: Vec<CoordsXY> = neighbours
            .iter()
            .filter(|c| self.passage_at(**c))
            .copied()
            .collect();
        let results: Vec<Vec<CoordsXY>> = vec![a, b, c];
        results.concat()
    }

    fn part_2_rules_neighbours(&self, from: CoordsXYL) -> Vec<CoordsXYL> {
        let from_xy: CoordsXY = CoordsXY {
            x: from.x,
            y: from.y,
        };

        // Remember - in self.portals left is outer, right is inner
        let a: Vec<CoordsXYL> = if from.level == 0 {
            vec![]
        } else {
            let a_binding = self.portals.get_by_left(&from_xy).copied();
            a_binding
                .iter()
                .map(|c| c.with_level(from.level - 1))
                .collect()
        };

        let b_binding = self.portals.get_by_right(&from_xy).copied();
        let b: Vec<CoordsXYL> = b_binding
            .iter()
            .map(|c| c.with_level(from.level + 1))
            .collect();

        let c: Vec<CoordsXYL> = from_xy
            .all_directions()
            .iter()
            .filter(|c| self.passage_at(**c))
            .map(|c| c.with_level(from.level))
            .collect();

        vec![a, b, c].concat()
    }
}

fn solve_1(data: &str, expected: Option<i32>) {
    let maze = Maze::new(data);
    let result = bfs(
        &maze.start,
        |n| maze.part_1_rules_neighbours(*n),
        |n| *n == maze.end,
    );
    assert_eq!(result.map(|r| (r.len() - 1) as i32), expected);
    println!("{expected:?}");
}

fn solve_2(data: &str, expected: Option<i32>) {
    let maze = Maze::new(data);
    let result = bfs(
        &maze.start.outermost(),
        |n| maze.part_2_rules_neighbours(*n),
        |n| *n == maze.end.outermost(),
    );
    assert_eq!(result.map(|r| (r.len() - 1) as i32), expected);
    println!("{expected:?}");
}

const DATA_3: &str = include_str!("../../resources/20.txt");

fn main() {
    solve_1(DATA_3, Some(606));
    solve_2(DATA_3, Some(123_456));
}

#[cfg(test)]
mod tests {
    use super::*;

    const DATA_1: &str = include_str!("../../resources/20-test-1.txt");
    const DATA_2: &str = include_str!("../../resources/20-test-2.txt");
    const DATA_4: &str = include_str!("../../resources/20-test-3.txt");

    #[test]
    fn test_solve_1_1() {
        solve_1(DATA_1, Some(23));
    }

    #[test]
    fn test_solve_1_2() {
        solve_1(DATA_2, Some(58));
    }

    #[test]
    fn test_solve_1_3() {
        solve_1(DATA_3, Some(606));
    }

    #[test]
    fn test_solve_2_1() {
        solve_2(DATA_1, Some(26));
    }

    #[test]
    fn test_solve_2_3() {
        solve_2(DATA_4, Some(396));
    }

    #[test]
    fn test_solve_2_4() {
        solve_2(DATA_3, Some(7_186));
    }
}
