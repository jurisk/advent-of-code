use std::collections::{BTreeMap, BTreeSet};

use itertools::Itertools;
use pathfinding::prelude::dijkstra;
use strum::{EnumIter, IntoEnumIterator};

use crate::Element::{Curium, Dilithium, Elerium, Plutonium, Ruthenium, Strontium, Thulium};
use crate::Object::{Generator, Microchip};

#[derive(Eq, PartialEq, Hash, Copy, Clone, Ord, PartialOrd, EnumIter, Debug)]
#[repr(u8)]
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

#[derive(Eq, PartialEq, Hash, Copy, Clone, Ord, PartialOrd, Debug)]
enum Object {
    Microchip(Element),
    Generator(Element),
}

#[derive(Eq, PartialEq, Hash, Copy, Clone, Ord, PartialOrd, EnumIter, Debug)]
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

    fn neighbouring_floors(self) -> Vec<Floor> {
        match self {
            Floor::F1 => vec![Floor::F2],
            Floor::F2 => vec![Floor::F1, Floor::F3],
            Floor::F3 => vec![Floor::F2, Floor::F4],
            Floor::F4 => vec![Floor::F3],
        }
    }
}

#[derive(Eq, PartialEq, Clone, Hash, Ord, PartialOrd, Debug)]
struct Definition {
    elevator_at: Floor,
    floors:      BTreeMap<Floor, BTreeSet<Object>>,
}

impl Definition {
    fn find_object(&self, object: Object) -> Option<Floor> {
        self.floors
            .iter()
            .find(|(_floor, objects)| objects.contains(&object))
            .map(|(floor, _objects)| *floor)
    }

    fn object_count(&self) -> usize {
        self.floors.values().map(BTreeSet::len).sum()
    }

    fn to_state(&self) -> State {
        let mut pairs: Vec<StatePair> = Vec::new();

        let elements: BTreeSet<Element> = self
            .floors
            .values()
            .flat_map(|x| {
                x.iter().map(|y| {
                    match y {
                        Microchip(z) | Generator(z) => *z,
                    }
                })
            })
            .collect();

        for element in elements {
            let generator_at = self.find_object(Generator(element)).unwrap();
            let microchip_at = self.find_object(Microchip(element)).unwrap();
            pairs.push(StatePair {
                generator_at,
                microchip_at,
            });
        }

        let result = State::create(self.elevator_at, pairs);

        assert_eq!(self.object_count(), result.object_count());
        result
    }

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

    fn is_safe(&self) -> bool {
        self.floors
            .iter()
            .all(|(_floor, objects)| Definition::objects_are_safe(objects))
    }

    #[must_use]
    fn add_object(&self, floor: Floor, object: Object) -> Definition {
        let mut new_floors = self.floors.clone();
        let mut new_set = self.objects_at_floor(floor);
        new_set.insert(object);
        new_floors.insert(floor, new_set);
        Definition {
            elevator_at: self.elevator_at,
            floors:      new_floors,
        }
    }

    #[must_use]
    fn remove_object(&self, floor: Floor, object: Object) -> Definition {
        let mut new_floors = self.floors.clone();
        let mut new_set = self.objects_at_floor(floor);
        new_set.remove(&object);
        new_floors.insert(floor, new_set);
        Definition {
            elevator_at: self.elevator_at,
            floors:      new_floors,
        }
    }

    #[must_use]
    fn move_objects(&self, objects: &Vec<Object>, target_floor: Floor) -> Definition {
        let mut result = Definition {
            elevator_at: target_floor,
            floors:      self.floors.clone(),
        };

        for object in objects {
            result = result.remove_object(self.elevator_at, *object);
            result = result.add_object(target_floor, *object);
        }

        assert_eq!(self.object_count(), result.object_count());
        result
    }

    fn candidate_successors(&self) -> BTreeSet<Definition> {
        let mut results = BTreeSet::new();
        let candidate_objects = self.objects_at_floor(self.elevator_at);
        for target_floor in self.elevator_at.neighbouring_floors() {
            for object in &candidate_objects {
                results.insert(self.move_objects(&vec![*object], target_floor));
            }

            for combination in candidate_objects.iter().combinations(2) {
                let v: Vec<Object> = combination.iter().map(|x| **x).collect();
                results.insert(self.move_objects(&v, target_floor));
            }
        }
        results
    }

    fn successors(&self) -> BTreeSet<Definition> {
        self.candidate_successors()
            .into_iter()
            .filter(Definition::is_safe)
            .collect()
    }
}

#[derive(Eq, PartialEq, Clone, Hash, Ord, PartialOrd, Debug)]
struct StatePair {
    generator_at: Floor,
    microchip_at: Floor,
}

#[derive(Eq, PartialEq, Clone, Hash, Ord, PartialOrd, Debug)]
struct State {
    elevator_at: Floor,
    pairs:       Vec<StatePair>,
}

impl State {
    fn create(elevator_at: Floor, pairs: Vec<StatePair>) -> Self {
        State {
            elevator_at,
            pairs: pairs.into_iter().sorted().collect(),
        }
    }

    fn object_count(&self) -> usize {
        self.pairs.len() * 2
    }

    fn to_definition(&self) -> Definition {
        let elements: Vec<Element> = Element::iter().collect();
        let floors: BTreeMap<Floor, BTreeSet<Object>> = BTreeMap::new();

        let mut result = Definition {
            elevator_at: self.elevator_at,
            floors,
        };

        for (idx, pair) in self.pairs.iter().enumerate() {
            let element = elements[idx];
            result = result.add_object(pair.generator_at, Generator(element));
            result = result.add_object(pair.microchip_at, Microchip(element));
        }

        assert_eq!(self.object_count(), result.object_count());

        result
    }

    fn successors(&self) -> BTreeSet<State> {
        let definition = self.to_definition();
        definition
            .successors()
            .iter()
            .map(Definition::to_state)
            .collect()
    }

    fn is_target(&self) -> bool {
        self.elevator_at.terminal()
            && self
                .pairs
                .iter()
                .all(|pair| pair.generator_at.terminal() && pair.microchip_at.terminal())
    }
}

fn real_data_1() -> Definition {
    Definition {
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

fn real_data_2() -> Definition {
    Definition {
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

fn solve(start: &Definition) -> Option<usize> {
    let result = dijkstra(
        &start.to_state(),
        |x| {
            State::successors(x)
                .into_iter()
                .map(|s| (s, 1))
                .collect::<Vec<_>>()
        },
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

    fn test_data() -> Definition {
        Definition {
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
    fn test_solve_1_real() {
        assert_eq!(solve(&real_data_1()), Some(37));
    }

    #[test]
    #[ignore = "Slow"]
    fn test_solve_2_real() {
        assert_eq!(solve(&real_data_2()), Some(61));
    }
}
