use bimap::BiMap;
use itertools::Itertools;
use pathfinding::prelude::dijkstra;
use std::collections::HashMap;

#[derive(Eq, PartialEq, Hash, Copy, Clone, Debug)]
struct CoordsXY {
    x: i32,
    y: i32,
}

#[derive(Debug)]
struct Maze {
    open_passages: Vec<Vec<bool>>,
    start: CoordsXY,
    end: CoordsXY,
    portals: BiMap<CoordsXY, CoordsXY>,
}

const PASSAGE: char = '.';

impl Maze {
    fn new(data: &str) -> Maze {
        type Name = [char; 2];

        let rows: Vec<&str> = data.split('\n').collect();

        let width = rows[2].len() - 2; // we assume the first row of the maze never has a portal on the right side
        let height = rows.len() - 4; // we assume there are always portals on the top & the bottom

        let chars: Vec<Vec<char>> = rows
            .iter()
            .filter(|x| !x.is_empty())
            .map(|x| x.chars().pad_using(width + 4, |_| ' ').collect())
            .collect();

        println!("{}", data);

        println!("width = {}, height = {}", width, height);

        let open_passages: Vec<Vec<bool>> = (0..height)
            .map(|y| (0..width).map(|x| chars[y + 2][x + 2] == PASSAGE).collect())
            .collect();

        assert_eq!(open_passages.len(), height);
        for row in &open_passages {
            assert_eq!(row.len(), width);
        }

        let mut temp_portals: Vec<(Name, CoordsXY)> = Vec::new();
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

        let mut portals = BiMap::new();
        let mut temporary_map: HashMap<Name, CoordsXY> = HashMap::new();
        for (name, coords) in temp_portals {
            match temporary_map.remove(&name) {
                None => {
                    println!("Found {:?} at {:?}", name, coords);
                    temporary_map.insert(name, coords);
                }
                Some(existing) => {
                    println!("Matched {:?} and {:?}", coords, existing);
                    portals.insert(coords, existing);
                }
            }
        }

        println!("{:?}", temporary_map);
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

    fn neighbours_iter(&self, from: &CoordsXY) -> Vec<(CoordsXY, i32)> {
        let a_binding = self.portals.get_by_left(from).copied();
        let a: Vec<&CoordsXY> = a_binding.iter().collect();
        let b_binding = self.portals.get_by_right(from).copied();
        let b: Vec<&CoordsXY> = b_binding.iter().collect();
        let neigbours = vec![
            CoordsXY {
                x: from.x + 1,
                y: from.y,
            },
            CoordsXY {
                x: from.x - 1,
                y: from.y,
            },
            CoordsXY {
                x: from.x,
                y: from.y + 1,
            },
            CoordsXY {
                x: from.x,
                y: from.y - 1,
            },
        ];
        let c: Vec<&CoordsXY> = neigbours.iter().filter(|c| self.passage_at(**c)).collect();

        let results: Vec<Vec<&CoordsXY>> = vec![a, b, c];

        results.concat().iter().map(|n| (**n, 1)).collect()
    }
}

fn solve_1(data: &str, expected: i32) {
    let maze = Maze::new(data);
    println!("{:?}", maze);
    let (route, cost) =
        dijkstra(&maze.start, |n| maze.neighbours_iter(n), |n| *n == maze.end).unwrap();
    println!("{:?}", route);
    assert_eq!(cost, expected);
}

fn solve_2(data: &str, expected: i32) {
    todo!();
}

const DATA_3: &str = include_str!("../../resources/20.txt");

fn main() {
    solve_1(DATA_3, 123_456);
    solve_2(DATA_3, 123_456);
}

#[cfg(test)]
mod tests {
    use super::*;

    const DATA_1: &str = include_str!("../../resources/20-test-1.txt");
    const DATA_2: &str = include_str!("../../resources/20-test-2.txt");

    #[test]
    fn test_solve_1_1() {
        solve_1(DATA_1, 23);
    }

    #[test]
    fn test_solve_1_2() {
        solve_1(DATA_2, 58);
    }

    #[test]
    fn test_solve_1_3() {
        solve_1(DATA_3, 606);
    }

    #[test]
    fn test_solve_2() {
        solve_2(DATA_3, 123_456);
    }
}
