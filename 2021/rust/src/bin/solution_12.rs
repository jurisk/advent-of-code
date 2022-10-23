use advent_of_code::parsing::{parse_lines_to_nonempty, split_into_two_strings, Error};
use itertools::Itertools;
use nonempty::{nonempty, NonEmpty};
use pathfinding::prelude::bfs_reach;
use std::str::FromStr;

const DATA: &str = include_str!("../../resources/12.txt");

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
enum Vertex {
    Start,
    End,
    Big { name: String },
    Small { name: String },
}

impl FromStr for Vertex {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let result = if s == "start" {
            Vertex::Start
        } else if s == "end" {
            Vertex::End
        } else if s.to_lowercase() == s {
            Vertex::Small {
                name: s.to_string(),
            }
        } else {
            Vertex::Big {
                name: s.to_string(),
            }
        };
        Ok(result)
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
struct Edge {
    a: Vertex,
    b: Vertex,
}

impl Edge {
    fn has(&self, what: &Vertex) -> Option<&Vertex> {
        if &self.a == what {
            Some(&self.b)
        } else if &self.b == what {
            Some(&self.a)
        } else {
            None
        }
    }
}

impl FromStr for Edge {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (a_str, b_str) = split_into_two_strings(s, "-")?;
        let a = a_str.parse()?;
        let b = b_str.parse()?;
        Ok(Edge { a, b })
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
struct Path {
    vertices: NonEmpty<Vertex>,
}

type SmallVertexValidContinuationF = fn(&Path, &Vertex) -> bool;

impl Path {
    fn start() -> Path {
        Path {
            vertices: nonempty![Vertex::Start],
        }
    }

    fn last(&self) -> &Vertex {
        self.vertices.last()
    }

    // visit small caves at most once, and can visit big caves any number of times
    fn valid_continuation(
        &self,
        v: &Vertex,
        continuation_f: SmallVertexValidContinuationF,
    ) -> bool {
        if self.last() == &Vertex::End {
            false
        } else {
            match v {
                Vertex::Start => false,
                Vertex::Big { .. } => true,
                _ => continuation_f(self, v),
            }
        }
    }

    // Should visit small caves at most once, and can visit big caves any number of times
    fn small_vertex_is_valid_continuation_1(&self, v: &Vertex) -> bool {
        !self.vertices.contains(v)
    }

    fn is_small_cave_already_visited_twice(&self) -> bool {
        let all_small: Vec<_> = self
            .vertices
            .iter()
            .filter(|p| matches!(p, Vertex::Small { .. }))
            .collect();
        let unique_small_count = all_small.iter().unique().count();
        all_small.len() != unique_small_count
    }

    // Specifically, big caves can be visited any number of times, a single small cave can be
    // visited at most twice, and the remaining small caves can be visited at most once.
    fn small_vertex_is_valid_continuation_2(&self, v: &Vertex) -> bool {
        self.small_vertex_is_valid_continuation_1(v) || !self.is_small_cave_already_visited_twice()
    }

    fn with_continuation(&self, v: &Vertex) -> Path {
        let mut vertices = self.vertices.clone();
        vertices.push(v.clone());
        Path { vertices }
    }
}

struct Graph {
    edges: NonEmpty<Edge>,
}

impl Graph {
    fn neighbours(&self, vertex: &Vertex) -> Vec<&Vertex> {
        self.edges.iter().filter_map(|e| e.has(vertex)).collect()
    }

    fn successors(&self, path: &Path, continuation_f: SmallVertexValidContinuationF) -> Vec<Path> {
        let neighbours_for_last = self.neighbours(path.last());
        let valid_continuations: Vec<&&Vertex> = neighbours_for_last
            .iter()
            .filter(|v| path.valid_continuation(v, continuation_f))
            .collect();
        valid_continuations
            .iter()
            .map(|v| path.with_continuation(v))
            .collect()
    }
}

impl FromStr for Graph {
    type Err = Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let edges: NonEmpty<Edge> = parse_lines_to_nonempty(input)?;
        Ok(Graph { edges })
    }
}

fn solve(input: &str, continuation_f: SmallVertexValidContinuationF) -> Result<usize, Error> {
    let graph: Graph = input.parse()?;
    let result = bfs_reach(Path::start(), |path| graph.successors(path, continuation_f))
        .filter(|p| p.last() == &Vertex::End)
        .count();
    Ok(result)
}

fn solve_1(input: &str) -> Result<usize, Error> {
    solve(input, Path::small_vertex_is_valid_continuation_1)
}

fn solve_2(input: &str) -> Result<usize, Error> {
    solve(input, Path::small_vertex_is_valid_continuation_2)
}

fn main() {
    let result_1 = solve_1(DATA);
    println!("Part 1: {:?}", result_1);

    let result_2 = solve_2(DATA);
    println!("Part 2: {:?}", result_2);
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA_1: &str = include_str!("../../resources/12-test-1.txt");
    const TEST_DATA_2: &str = include_str!("../../resources/12-test-2.txt");
    const TEST_DATA_3: &str = include_str!("../../resources/12-test-3.txt");

    #[test]
    fn test_solve_1_test_1() {
        assert_eq!(solve_1(TEST_DATA_1), Ok(10));
    }

    #[test]
    fn test_solve_1_test_2() {
        assert_eq!(solve_1(TEST_DATA_2), Ok(19));
    }

    #[test]
    fn test_solve_1_test_3() {
        assert_eq!(solve_1(TEST_DATA_3), Ok(226));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(DATA), Ok(3576));
    }

    #[test]
    fn test_solve_2_test_1() {
        assert_eq!(solve_2(TEST_DATA_1), Ok(36));
    }

    #[test]
    fn test_solve_2_test_2() {
        assert_eq!(solve_2(TEST_DATA_2), Ok(103));
    }

    #[test]
    fn test_solve_2_test_3() {
        assert_eq!(solve_2(TEST_DATA_3), Ok(3509));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(DATA), Ok(84271));
    }
}
