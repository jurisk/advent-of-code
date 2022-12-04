use advent_of_code_2019::intcode::{Entry, Process};
use itertools::Itertools;
use std::collections::{HashMap, HashSet, VecDeque};

#[derive(Clone, Eq, PartialEq, Debug)]
enum Direction {
    North = 1,
    South = 2,
    West = 3,
    East = 4,
}

impl Direction {
    fn to_intcode(&self) -> Entry {
        match self {
            Direction::North => 1,
            Direction::South => 2,
            Direction::West => 3,
            Direction::East => 4,
        }
    }

    fn rotate_left(&self) -> Direction {
        match self {
            Direction::North => Direction::West,
            Direction::South => Direction::East,
            Direction::West => Direction::South,
            Direction::East => Direction::North,
        }
    }

    fn rotate_right(&self) -> Direction {
        match self {
            Direction::North => Direction::East,
            Direction::South => Direction::West,
            Direction::West => Direction::North,
            Direction::East => Direction::South,
        }
    }

    fn all() -> Vec<Direction> {
        vec![
            Direction::North,
            Direction::South,
            Direction::West,
            Direction::East,
        ]
    }
}

type Number = i32;

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
struct Coords {
    x: Number,
    y: Number,
}

impl Coords {
    fn new(x: i32, y: i32) -> Self {
        Coords { x, y }
    }

    fn movement(&self, direction: &Direction) -> Coords {
        Coords {
            x: self.x + i32::from(*direction == Direction::East)
                - i32::from(*direction == Direction::West),
            y: self.y - i32::from(*direction == Direction::North)
                + i32::from(*direction == Direction::South),
        }
    }

    fn neighbours(&self) -> Vec<Coords> {
        Direction::all()
            .iter()
            .map(|direction| self.movement(direction))
            .collect()
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
enum Tile {
    Wall = 0,
    Empty = 1,
    Goal = 2,
}

impl Tile {
    fn from_intcode(value: Entry) -> Tile {
        match value {
            0 => Tile::Wall,
            1 => Tile::Empty,
            2 => Tile::Goal,
            _ => panic!("Unrecognized tile {value}"),
        }
    }
}

struct Robot {
    known_tiles: HashMap<Coords, Tile>,
    //     queue: VecDeque<Coords>,
    current_position: Coords,
    current_direction: Direction,
}

impl Robot {
    fn new() -> Robot {
        let mut result = Robot {
            known_tiles: HashMap::new(),
            //            queue: VecDeque::new(),
            current_position: Coords::new(0, 0),
            current_direction: Direction::North,
        };

        result.known_tiles.insert(Coords::new(0, 0), Tile::Empty);

        result
    }

    fn has_finished_exploring(&self) -> bool {
        self.current_position == Coords::new(0, 0)
            && self.current_direction == Direction::North
            && self.known_tiles.len() > 1
    }

    fn pop_next_direction(&self) -> Direction {
        self.current_direction.clone()
    }

    fn map_as_string(&self) -> String {
        let coords: Vec<&Coords> = self.known_tiles.keys().collect();
        let x_s: Vec<Number> = coords.iter().map(|c| c.x).collect();
        let y_s: Vec<Number> = coords.iter().map(|c| c.y).collect();

        let min_x = *x_s.iter().min().unwrap();
        let min_y = *y_s.iter().min().unwrap();
        let max_x = *x_s.iter().max().unwrap();
        let max_y = *y_s.iter().max().unwrap();
        (min_y..=max_y)
            .map(|y| {
                (min_x..=max_x)
                    .map(|x| {
                        let coords = Coords::new(x, y);
                        if coords == self.current_position {
                            format!("{:?}", self.current_direction)
                                .chars()
                                .collect::<Vec<_>>()[0]
                        } else {
                            let square = self.known_tiles.get(&coords);
                            match square {
                                Some(Tile::Empty) => '░',
                                Some(Tile::Wall) => '█',
                                Some(Tile::Goal) => '!',
                                None => ' ',
                            }
                        }
                    })
                    .join("")
                    + "\n"
            })
            .join("")
    }

    fn move_result(&mut self, tile: Tile) {
        let target_position = self.current_position.movement(&self.current_direction);
        println!("Found {tile:?} at {target_position:?}");
        if tile == Tile::Wall {
            self.current_direction = self.current_direction.rotate_left();
            println!(
                "Rotated to face {:?}, currently at {:?}",
                self.current_direction, self.current_position
            );
        } else {
            self.current_position = target_position.clone();
            self.current_direction = self.current_direction.rotate_right();
            println!(
                "Moved to {:?}, known is\n{}",
                self.current_position,
                self.map_as_string()
            );
        }

        self.known_tiles.insert(target_position, tile);
    }
}

fn bfs(tiles: &HashMap<Coords, Tile>, start: Coords, end: Coords) -> Vec<Coords> {
    // maintain a queue of paths
    let mut queue: VecDeque<Vec<Coords>> = VecDeque::new();
    let mut visited: HashSet<Coords> = HashSet::new();

    // push the first path into the queue
    queue.push_back(vec![start]);
    while !queue.is_empty() {
        // get the first path from the queue
        let path = queue.pop_front().unwrap();

        // get the last node from the path
        let last_node = path.last().unwrap();

        // path found
        if *last_node == end {
            return path;
        }

        if !visited.contains(last_node) {
            // enumerate all adjacent nodes, construct a new path and push it into the queue
            last_node
                .neighbours()
                .iter()
                .filter(|x| *tiles.get(x).unwrap() != Tile::Wall)
                .for_each(|adjacent| {
                    let mut new_path = path.clone();
                    new_path.push(adjacent.clone());
                    queue.push_back(new_path);
                });

            visited.insert(last_node.clone());
        }
    }

    panic!("Path not found in {tiles:?}");
}

fn goal_tile(tiles: &HashMap<Coords, Tile>) -> Coords {
    tiles
        .iter()
        .find(|(_, v)| **v == Tile::Goal)
        .unwrap()
        .0
        .clone()
}

fn find_best_route(tiles: &HashMap<Coords, Tile>) -> Vec<Coords> {
    bfs(tiles, Coords::new(0, 0), goal_tile(tiles))
}

fn find_tiles() -> HashMap<Coords, Tile> {
    let mut process = Process::from_string(include_str!("../../resources/15.txt"));
    let mut robot: Robot = Robot::new();

    while !robot.has_finished_exploring() {
        process.provide_input(robot.pop_next_direction().to_intcode());
        let halted = process.run_to_halt_or_output();

        if halted {
            break;
        }

        let obtained_tile = Tile::from_intcode(process.next_output_unsafe());
        robot.move_result(obtained_tile);
    }

    robot.known_tiles
}

fn solve_1(tiles: &HashMap<Coords, Tile>) {
    let route = find_best_route(tiles);
    println!("Route: {route:?}");
    let result = route.len() - 1;
    assert_eq!(result, 298);
    println!("Part 1: {result}");
}

fn bfs_depth(tiles: &HashMap<Coords, Tile>, from: Coords) -> usize {
    // maintain a queue of paths
    let mut queue: VecDeque<Vec<Coords>> = VecDeque::new();
    let mut visited: HashSet<Coords> = HashSet::new();
    let mut longest_found: usize = 0;

    // push the first path into the queue
    queue.push_back(vec![from]);
    while !queue.is_empty() {
        // get the first path from the queue
        let path = queue.pop_front().unwrap();

        // get the last node from the path
        let last_node = path.last().unwrap();

        if path.len() > longest_found {
            longest_found = path.len();
        }

        if !visited.contains(last_node) {
            // enumerate all adjacent nodes, construct a new path and push it into the queue
            last_node
                .neighbours()
                .iter()
                .filter(|x| *tiles.get(x).unwrap() != Tile::Wall)
                .for_each(|adjacent| {
                    let mut new_path = path.clone();
                    new_path.push(adjacent.clone());
                    queue.push_back(new_path);
                });

            visited.insert(last_node.clone());
        }
    }

    longest_found
}

fn solve_2(tiles: &HashMap<Coords, Tile>) {
    let result = bfs_depth(tiles, goal_tile(tiles)) - 2;
    assert_eq!(result, 346);
    println!("Part 2: {result}");
}

fn main() {
    let tiles = find_tiles();
    solve_1(&tiles);
    solve_2(&tiles);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve_1() {
        solve_1(&find_tiles());
    }

    #[test]
    fn test_solve_2() {
        solve_2(&find_tiles());
    }
}
