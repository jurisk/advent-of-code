use std::str::FromStr;

use advent_of_code_common::coords2d::Coords2D;
use advent_of_code_common::parsing::Error;
use advent_of_code_common::utils::single_result;
use itertools::Itertools;
use pathfinding::matrix::Matrix;
use pathfinding::prelude::astar;
use regex::Regex;

#[derive(Debug)]
struct NodeInformation {
    location: Coords2D<usize>,
    capacity: u16,
    used:     u16,
}

impl FromStr for NodeInformation {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let re = Regex::new(r"/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+(\d+)%")
            .map_err(|_| "Error while compiling regex".to_string())?;

        let caps = re.captures(s).ok_or("No captures for regex".to_string())?;

        let get_group = |group: usize| -> Result<String, Error> {
            caps.get(group)
                .map(|m| m.as_str().to_owned())
                .ok_or(format!("Could not get capture group {group}"))
        };

        let parse_int = |s: &str| -> Result<u16, Error> {
            s.parse::<u16>()
                .map_err(|_| "Failed to parse integer".to_string())
        };

        let x = parse_int(&get_group(1)?)?;
        let y = parse_int(&get_group(2)?)?;
        let size = parse_int(&get_group(3)?)?;
        let used = parse_int(&get_group(4)?)?;

        Ok(NodeInformation {
            location: Coords2D {
                x: x as usize,
                y: y as usize,
            },
            capacity: size,
            used,
        })
    }
}

type ParsedInputData = Vec<NodeInformation>;

fn parse(input: &str) -> Result<ParsedInputData, Error> {
    let mut lines = input.lines();
    let l1 = lines.next().ok_or("Missing Line 1")?;
    assert_eq!(l1, "root@ebhq-gridcenter# df -h");
    let l2 = lines.next().ok_or("Missing Line 2")?;
    assert_eq!(l2, "Filesystem              Size  Used  Avail  Use%");

    let mut result = Vec::new();
    for line in lines {
        let node = NodeInformation::from_str(line)?;
        result.push(node);
    }

    Ok(result)
}

fn solve_1(data: &ParsedInputData) -> usize {
    let (capacities, state) = create(data);
    state.viable_moves_ignoring_proximity(&capacities).len()
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
struct CompleteState {
    goal_at: (usize, usize),
    usages:  Matrix<u16>,
}

impl CompleteState {
    fn pretty_print(&self, capacities: &Matrix<u16>) -> String {
        (0 .. self.usages.rows)
            .map(|row| {
                (0 .. self.usages.columns)
                    .map(|column| {
                        let key = (row, column);
                        let capacity = capacities[key];
                        let usage = self.usages[key];
                        format!("{usage:>3}/{capacity:>3}")
                    })
                    .join(" | ")
            })
            .join("\n")
    }

    fn is_target(&self) -> bool {
        self.goal_at == (0, 0)
    }

    fn viable_move(
        &self,
        capacities: &Matrix<u16>,
        from: (usize, usize),
        to: (usize, usize),
    ) -> bool {
        let from_used = self.usages[from];
        let to_used = self.usages[to];
        let to_capacity = capacities[to];

        from_used != 0 && // Node A is not empty (its Used is not zero)
            from != to && // Nodes A and B are not the same node
            from_used <= to_capacity - to_used // The data on node A (its Used) would fit on node B (its Avail)
    }

    fn viable_moves_ignoring_proximity(
        &self,
        capacities: &Matrix<u16>,
    ) -> Vec<((usize, usize), (usize, usize))> {
        let mut result = vec![];

        for from in self.usages.keys() {
            for to in self.usages.keys() {
                if self.viable_move(capacities, from, to) {
                    result.push((from, to));
                }
            }
        }

        result
    }

    fn viable_neighbouring_moves(
        &self,
        capacities: &Matrix<u16>,
    ) -> Vec<((usize, usize), (usize, usize))> {
        let mut result = vec![];

        for from in self.usages.keys() {
            for to in self.usages.neighbours(from, false) {
                if self.viable_move(capacities, from, to) {
                    result.push((from, to));
                }
            }
        }

        result
    }

    fn successors(&self, capacities: &Matrix<u16>) -> Vec<(CompleteState, usize)> {
        self.viable_neighbouring_moves(capacities)
            .into_iter()
            .map(|(from, to)| {
                let goal_at = if self.goal_at == from {
                    to
                } else {
                    self.goal_at
                };

                let mut usages = self.usages.clone();
                let moving = usages[from];
                usages[from] = 0;
                usages[to] += moving;
                debug_assert_eq!(
                    self.usages[from] + self.usages[to],
                    usages[from] + usages[to]
                );

                (Self { goal_at, usages }, 1)
            })
            .collect()
    }

    fn heuristic(&self) -> usize {
        let (y, x) = self.goal_at;
        x + y
    }
}

fn create(data: &ParsedInputData) -> (Matrix<u16>, CompleteState) {
    let max_x = data.iter().map(|n| n.location.x).max().unwrap();
    let max_y = data.iter().map(|n| n.location.y).max().unwrap();

    let matrix_from_nodes = |f: fn(&NodeInformation) -> u16| {
        let mut m: Matrix<u16> = Matrix::new(max_y + 1, max_x + 1, 0);
        for node in data {
            m[node.location.as_tuple()] = f(node);
        }
        m
    };

    let capacities: Matrix<u16> = matrix_from_nodes(|node| node.capacity);
    let usages: Matrix<u16> = matrix_from_nodes(|node| node.used);

    let state = CompleteState {
        goal_at: (0, max_x),
        usages,
    };

    (capacities, state)
}

// This takes too long for larger levels
#[allow(unused)]
fn solve_2_using_complete_state(data: &ParsedInputData) -> Option<usize> {
    let (capacities, complete_state) = create(data);

    println!(
        "Complete starting state:\n{}",
        complete_state.pretty_print(&capacities)
    );

    astar(
        &complete_state,
        |state| state.successors(&capacities),
        CompleteState::heuristic,
        CompleteState::is_target,
    )
    .map(|(_, result)| result)
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
struct SimplifiedState {
    goal_at:  (usize, usize),
    empty_at: (usize, usize),
}

impl SimplifiedState {
    fn is_target(&self) -> bool {
        self.goal_at == (0, 0)
    }

    fn heuristic(&self) -> usize {
        let (y, x) = self.goal_at;
        x + y
    }

    fn successors(
        &self,
        capacities: &Matrix<u16>,
        large_threshold: u16,
    ) -> Vec<(SimplifiedState, usize)> {
        capacities
            .neighbours(self.empty_at, false)
            .filter(|c| capacities[*c] < large_threshold)
            .map(|c| {
                let goal_at = if c == self.goal_at {
                    self.empty_at
                } else {
                    self.goal_at
                };
                let empty_at = c;
                (Self { goal_at, empty_at }, 1)
            })
            .collect()
    }

    fn pretty_print(&self, capacities: &Matrix<u16>, large_threshold: u16) -> String {
        (0 .. capacities.rows)
            .map(|row| {
                (0 .. capacities.columns)
                    .map(|column| {
                        let key = (row, column);
                        let repr = if capacities[key] >= large_threshold {
                            "#"
                        } else if key == self.empty_at {
                            "_"
                        } else if key == self.goal_at {
                            "G"
                        } else {
                            "."
                        };
                        format!(" {repr} ")
                    })
                    .join("")
            })
            .join("\n")
    }
}

fn solve_2_using_simplified_state(data: &ParsedInputData, large_threshold: u16) -> Option<usize> {
    let (capacities, complete_state) = create(data);

    let empty_coords: Vec<_> = complete_state
        .usages
        .keys()
        .filter(|k| complete_state.usages[*k] == 0)
        .collect();
    let empty_at = *single_result(&empty_coords).unwrap();

    let start_state = SimplifiedState {
        goal_at: complete_state.goal_at,
        empty_at,
    };

    println!(
        "Starting state:\n{}\n",
        start_state.pretty_print(&capacities, large_threshold)
    );

    astar(
        &start_state,
        |state| state.successors(&capacities, large_threshold),
        SimplifiedState::heuristic,
        SimplifiedState::is_target,
    )
    .map(|(_, result)| result)
}

fn part_1(input: &str) -> Result<usize, Error> {
    parse(input).map(|input| solve_1(&input))
}

// `large_threshold` parameter is currently a heuristic, found through viewing the printed output
// of `CompleteState`. It can probably be calculated, but it was not needed.
fn part_2(input: &str, large_threshold: u16) -> Result<Option<usize>, Error> {
    parse(input).map(|input| solve_2_using_simplified_state(&input, large_threshold))
}

const DATA: &str = include_str!("../../resources/22.txt");

fn main() -> Result<(), Error> {
    let result_1 = part_1(DATA)?;
    println!("Part 1: {result_1}");

    let result_2 = part_2(DATA, 450)?;
    println!("Part 2: {result_2:?}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST: &str = include_str!("../../resources/22-test.txt");

    #[test]
    #[ignore]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(901));
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(part_2(TEST, 20), Ok(Some(7)));
    }

    #[test]
    #[ignore]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA, 450), Ok(Some(238)));
    }
}
