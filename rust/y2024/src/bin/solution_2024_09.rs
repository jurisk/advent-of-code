use std::fmt::{Debug, Formatter};

const DATA: &str = include_str!("../../resources/09.txt");

#[derive(Copy, Clone)]
struct FileId(usize);

impl Debug for FileId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let FileId(file_id) = self;
        if *file_id < 10 {
            write!(f, "{file_id}")
        } else {
            write!(f, "({file_id}) ")
        }
    }
}

#[derive(Copy, Clone)]
enum Node {
    Free(usize),
    Full(usize, FileId),
}

impl Debug for Node {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Free(count) => {
                for _ in 0 .. *count {
                    write!(f, ".")?;
                }
                Ok(())
            },
            Node::Full(count, file_id) => {
                for _ in 0 .. *count {
                    write!(f, "{file_id:?}")?;
                }
                Ok(())
            },
        }
    }
}

type R = usize;

// Note - Having a more efficient tree-based structure, which has easier insertion of slices would be more efficient.
#[derive(Clone)]
struct Disk {
    nodes: Vec<Node>,
}

impl Disk {
    pub fn solve_1(&mut self) {
        let mut head = 0;
        let mut tail = self.nodes.len() - 1;

        while head < tail {
            match (self.nodes[head], self.nodes[tail]) {
                (_, Node::Free(_)) => {
                    tail -= 1;
                },
                (Node::Full(..), _) => {
                    head += 1;
                },
                (Node::Free(free_count), Node::Full(full_count, file_id)) => {
                    if free_count == full_count {
                        self.nodes[head] = Node::Full(full_count, file_id);
                        self.nodes[tail] = Node::Free(free_count);
                        head += 1;
                        tail -= 1;
                    } else if free_count < full_count {
                        let new_nodes = [self.nodes[0 .. head].to_vec(),
                            vec![Node::Full(free_count, file_id)],
                            self.nodes[head + 1 .. tail].to_vec(),
                            vec![Node::Full(full_count - free_count, file_id)],
                            self.nodes[tail + 1 ..].to_vec()]
                        .concat();
                        self.nodes = new_nodes;
                    } else if free_count > full_count {
                        let new_nodes = [self.nodes[0 .. head].to_vec(),
                            vec![
                                Node::Full(full_count, file_id),
                                Node::Free(free_count - full_count),
                            ],
                            self.nodes[head + 1 .. tail].to_vec(),
                            self.nodes[tail + 1 ..].to_vec()]
                        .concat();
                        self.nodes = new_nodes;
                    }
                },
            }
        }
    }

    fn find_free(&self, count: usize) -> Option<(usize, usize)> {
        self.nodes
            .iter()
            .enumerate()
            .filter_map(|(idx, n)| {
                match n {
                    Node::Free(free_count) if *free_count >= count => Some((idx, *free_count)),
                    _ => None,
                }
            })
            .next()
    }

    fn solve_2(&mut self) {
        let mut pointer = self.nodes.len();
        while pointer > 0 {
            pointer -= 1;
            match &self.nodes[pointer] {
                Node::Free(_) => {},
                Node::Full(count, file_id) => {
                    if let Some((found_idx, free)) = self.find_free(*count) {
                        if found_idx < pointer {
                            if *count == free {
                                self.nodes[found_idx] = Node::Full(*count, *file_id);
                                self.nodes[pointer] = Node::Free(free);
                            } else if *count < free {
                                let new_nodes = [self.nodes[0 .. found_idx].to_vec(),
                                    vec![Node::Full(*count, *file_id), Node::Free(free - *count)],
                                    self.nodes[found_idx + 1 .. pointer].to_vec(),
                                    vec![Node::Free(*count)],
                                    self.nodes[pointer + 1 ..].to_vec()]
                                .concat();
                                self.nodes = new_nodes;
                            } else {
                                unreachable!("This should not happen");
                            }
                        }
                    }
                },
            }
        }
    }

    #[must_use]
    pub fn check_sum(&self) -> R {
        let mut result = 0;
        let mut before = 0;
        for n in &self.nodes {
            match n {
                Node::Free(count) => {
                    before += count;
                },
                Node::Full(count, FileId(file_id)) => {
                    for i in 0 .. *count {
                        result += (before + i) * file_id;
                    }
                    before += count;
                },
            }
        }
        result
    }
}

impl Debug for Disk {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for node in &self.nodes {
            write!(f, "{node:?}")?;
        }
        Ok(())
    }
}

fn parse(input: &str) -> Disk {
    let nodes: Vec<Node> = input
        .chars()
        .enumerate()
        .map(|(idx, ch)| {
            let count = (ch as u8 - b'0') as usize;
            if idx % 2 == 0 {
                Node::Full(count, FileId(idx / 2))
            } else {
                Node::Free(count)
            }
        })
        .collect();
    Disk { nodes }
}

fn solve_1(disk: Disk) -> R {
    let mut disk = disk;
    disk.solve_1();
    disk.check_sum()
}

fn solve_2(disk: Disk) -> R {
    let mut disk = disk;
    disk.solve_2();
    disk.check_sum()
}

fn main() {
    let data = parse(DATA);

    let result_1 = solve_1(data.clone());
    println!("Part 1: {result_1}");

    let result_2 = solve_2(data);
    println!("Part 2: {result_2}");
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = include_str!("../../resources/09-test-00.txt");

    fn test_data() -> Disk {
        parse(TEST_DATA)
    }

    fn real_data() -> Disk {
        parse(DATA)
    }

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(test_data()), 1928);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(real_data()), 6_471_961_544_878);
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve_2(test_data()), 2858);
    }

    #[test]
    #[ignore]
    fn test_solve_2_real() {
        assert_eq!(solve_2(real_data()), 6_511_178_035_564);
    }
}
