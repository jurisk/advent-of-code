use std::collections::{BTreeMap, BTreeSet};

use itertools::Itertools;
use pathfinding::prelude::astar;

use crate::Element::{Curium, Dilithium, Elerium, Plutonium, Ruthenium, Strontium, Thulium};
use crate::Object::{Generator, Microchip};

#[derive(Eq, PartialEq, Hash, Copy, Clone, Ord, PartialOrd)]
#[repr(u8)]
#[allow(dead_code)]
enum Element {
    Curium    = b'C',
    Dilithium = b'D',
    Elerium   = b'E',
    Hydrogen  = b'H',
    Lithium   = b'L',
    Plutonium = b'P',
    Ruthenium = b'R',
    Strontium = b'S',
    Thulium   = b'T',
}

#[derive(Eq, PartialEq, Hash, Copy, Clone, Ord, PartialOrd)]
enum Object {
    Microchip(Element),
    Generator(Element),
}

#[derive(Eq, PartialEq, Hash, Copy, Clone, Ord, PartialOrd)]
#[repr(u8)]
enum Floor {
    F1 = 1,
    F2 = 2,
    F3 = 3,
    F4 = 4,
}

impl Floor {
    fn terminal(self) -> bool {
        self == Floor::F4
    }

    fn all() -> Vec<Floor> {
        vec![Floor::F1, Floor::F2, Floor::F3, Floor::F4]
    }

    fn non_terminal_floors() -> Vec<Floor> {
        Floor::all().into_iter().filter(|x| !x.terminal()).collect()
    }

    fn neighbouring_floors(self) -> Vec<Floor> {
        match self {
            Floor::F1 => vec![Floor::F2],
            Floor::F2 => vec![Floor::F1, Floor::F3],
            Floor::F3 => vec![Floor::F2, Floor::F4],
            Floor::F4 => vec![Floor::F3],
        }
    }
}

#[derive(Eq, PartialEq, Clone, Hash)]
struct State {
    elevator_at: Floor,
    floors:      BTreeMap<Floor, BTreeSet<Object>>,
}

impl State {
    fn objects_are_safe(objects: &BTreeSet<Object>) -> bool {
        let mut microchips = BTreeSet::new();
        let mut generators = BTreeSet::new();

        for o in objects {
            match o {
                Microchip(x) => {
                    microchips.insert(x);
                },
                Generator(x) => {
                    generators.insert(x);
                },
            }
        }

        // If a chip is ever left in the same area as another RTG, and it's not
        // connected to its own RTG, the chip will be fried
        if generators.is_empty() {
            true
        } else {
            microchips
                .into_iter()
                .all(|microchip| generators.contains(microchip))
        }
    }

    fn objects_at_floor(&self, floor: Floor) -> BTreeSet<Object> {
        self.floors.get(&floor).unwrap_or(&BTreeSet::new()).clone()
    }

    fn is_target(&self) -> bool {
        self.elevator_at.terminal()
            && Floor::non_terminal_floors()
                .into_iter()
                .all(|x| self.objects_at_floor(x).is_empty())
    }

    fn is_safe(&self) -> bool {
        self.floors
            .iter()
            .all(|(_floor, objects)| State::objects_are_safe(objects))
    }

    fn move_objects(&self, objects: &Vec<Object>, target_floor: Floor) -> State {
        let mut new_floors = self.floors.clone();
        let mut new_at_current_floor = self.objects_at_floor(self.elevator_at);
        let mut new_at_target_floor = self.objects_at_floor(target_floor);
        for o in objects {
            new_at_current_floor.remove(o);
            new_at_target_floor.insert(*o);
        }
        new_floors.insert(self.elevator_at, new_at_current_floor);
        new_floors.insert(target_floor, new_at_target_floor);
        State {
            elevator_at: target_floor,
            floors:      new_floors,
        }
    }

    fn candidate_successors(&self) -> Vec<State> {
        let mut results = Vec::new();
        let candidate_objects = self.objects_at_floor(self.elevator_at);
        for target_floor in self.elevator_at.neighbouring_floors() {
            for object in &candidate_objects {
                results.push(self.move_objects(&vec![*object], target_floor));
            }

            for combination in candidate_objects.iter().combinations(2) {
                let v: Vec<Object> = combination.iter().map(|x| **x).collect();
                results.push(self.move_objects(&v, target_floor));
            }
        }
        results
    }

    fn successors(&self) -> Vec<State> {
        self.candidate_successors()
            .into_iter()
            .filter(State::is_safe)
            .collect()
    }

    fn heuristic(&self) -> usize {
        // A pretty useless heuristic as it is not speeding things up
        self.floors
            .iter()
            .map(|(floor, objects)| {
                let coef = match floor {
                    Floor::F1 => 3,
                    Floor::F2 => 2,
                    Floor::F3 => 1,
                    Floor::F4 => 0,
                };

                objects.len() * coef
            })
            .sum::<usize>()
            / 2
    }
}

fn real_data_1() -> State {
    State {
        elevator_at: Floor::F1,
        floors:      BTreeMap::from([
            (
                Floor::F1,
                BTreeSet::from([
                    Generator(Strontium),
                    Microchip(Strontium),
                    Generator(Plutonium),
                    Microchip(Plutonium),
                ]),
            ),
            (
                Floor::F2,
                BTreeSet::from([
                    Generator(Thulium),
                    Generator(Ruthenium),
                    Microchip(Ruthenium),
                    Generator(Curium),
                    Microchip(Curium),
                ]),
            ),
            (Floor::F3, BTreeSet::from([Microchip(Thulium)])),
            (Floor::F4, BTreeSet::new()),
        ]),
    }
}

fn real_data_2() -> State {
    State {
        elevator_at: Floor::F1,
        floors:      BTreeMap::from([
            (
                Floor::F1,
                BTreeSet::from([
                    Generator(Strontium),
                    Microchip(Strontium),
                    Generator(Plutonium),
                    Microchip(Plutonium),
                    Generator(Elerium),
                    Microchip(Elerium),
                    Generator(Dilithium),
                    Microchip(Dilithium),
                ]),
            ),
            (
                Floor::F2,
                BTreeSet::from([
                    Generator(Thulium),
                    Generator(Ruthenium),
                    Microchip(Ruthenium),
                    Generator(Curium),
                    Microchip(Curium),
                ]),
            ),
            (Floor::F3, BTreeSet::from([Microchip(Thulium)])),
            (Floor::F4, BTreeSet::new()),
        ]),
    }
}

fn solve(start: &State) -> Option<usize> {
    let result = astar(
        start,
        |x| {
            State::successors(x)
                .into_iter()
                .map(|s| (s, 1))
                .collect::<Vec<_>>()
        },
        State::heuristic,
        State::is_target,
    );
    result.map(|(_path, len)| len)
}

fn main() {
    let result_1 = solve(&real_data_1());
    println!("Part 1: {result_1:?}");

    let result_2 = solve(&real_data_2());
    println!("Part 2: {result_2:?}");
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Element::{Hydrogen, Lithium};

    fn test_data() -> State {
        State {
            elevator_at: Floor::F1,
            floors:      BTreeMap::from([
                (
                    Floor::F1,
                    BTreeSet::from([Microchip(Hydrogen), Microchip(Lithium)]),
                ),
                (Floor::F2, BTreeSet::from([Generator(Hydrogen)])),
                (Floor::F3, BTreeSet::from([Generator(Lithium)])),
                (Floor::F4, BTreeSet::new()),
            ]),
        }
    }

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve(&test_data()), Some(11));
    }

    #[test]
    #[ignore]
    fn test_solve_1_real() {
        assert_eq!(solve(&real_data_1()), Some(37));
    }

    #[test]
    #[ignore]
    fn test_solve_2_real() {
        assert_eq!(solve(&real_data_2()), Some(61));
    }
}
