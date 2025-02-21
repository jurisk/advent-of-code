use std::cmp::Ordering;
use std::collections::{BTreeSet, HashMap};
use std::fmt::{Debug, Formatter};
use std::str::FromStr;

use advent_of_code_common::parsing::{
    Error, parse_lines_to_vec_passing_parser, split_into_two_segments_separated_by_double_newline,
};
use advent_of_code_common::set_of_two::SetOfTwo;
use pathfinding::prelude::{dfs, topological_sort};

const DATA: &str = include_str!("../../resources/24.txt");

type N = u64;

const INPUT_BITS: u8 = 45;

#[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
enum WireId {
    X(u8),
    Y(u8),
    Z(u8),
    Named(usize),
}

#[derive(Default, Clone)]
struct WireNames {
    names:           Vec<String>,
    name_to_wire_id: HashMap<String, usize>,
}

impl WireNames {
    #[must_use]
    fn name_for(&self, id: WireId) -> String {
        match id {
            WireId::X(n) => format!("x{n}"),
            WireId::Y(n) => format!("y{n}"),
            WireId::Z(n) => format!("z{n}"),
            WireId::Named(n) => self.names[n].clone(),
        }
    }

    #[must_use]
    fn names_size(&self) -> usize {
        self.names.len()
    }

    #[must_use]
    fn get(&self, name: &str) -> Option<WireId> {
        if let Some(stripped) = name.strip_prefix("x") {
            Some(WireId::X(stripped.parse().expect("Invalid x")))
        } else if let Some(stripped) = name.strip_prefix("y") {
            Some(WireId::Y(stripped.parse().expect("Invalid y")))
        } else if let Some(stripped) = name.strip_prefix("z") {
            Some(WireId::Z(stripped.parse().expect("Invalid z")))
        } else {
            self.name_to_wire_id.get(name).map(|&id| WireId::Named(id))
        }
    }

    #[must_use]
    fn get_or_insert(&mut self, name: &str) -> WireId {
        match self.get(name) {
            None => {
                let id = self.names.len();
                self.names.push(name.to_string());
                self.name_to_wire_id.insert(name.to_string(), id);
                WireId::Named(id)
            },
            Some(found) => found,
        }
    }
}

#[derive(Clone)]
struct Values {
    x:     N,
    y:     N,
    z:     N,
    named: Vec<bool>,
}

fn mask(n: u8) -> N {
    1 << n
}

impl Debug for Values {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Values {{ x: {:b}, y: {:b}, z: {:b} }}",
            self.x, self.y, self.z,
        )
    }
}

impl Values {
    #[must_use]
    fn for_names(names: &WireNames) -> Self {
        Values {
            x:     0,
            y:     0,
            z:     0,
            named: vec![false; names.names_size()],
        }
    }

    fn random_bits(bits: u8) -> N {
        let mut value = 0;
        for idx in 0 .. bits {
            value |= N::from(rand::random::<bool>()) << idx;
        }
        value
    }

    #[must_use]
    fn random_xy(names: &WireNames) -> Self {
        let mut values = Self::for_names(names);
        values.x = Self::random_bits(INPUT_BITS);
        values.y = Self::random_bits(INPUT_BITS);
        values
    }

    #[must_use]
    fn get(&self, id: WireId) -> bool {
        let get_bit = |n: u8, field: N| field & mask(n) != 0;
        match id {
            WireId::X(n) => get_bit(n, self.x),
            WireId::Y(n) => get_bit(n, self.y),
            WireId::Z(n) => get_bit(n, self.z),
            WireId::Named(n) => self.named[n],
        }
    }

    fn set(&mut self, id: WireId, value: bool) {
        let set_bit = |n: u8, field: &mut N| {
            if value {
                *field |= mask(n);
            } else {
                *field &= !mask(n);
            }
        };

        match id {
            WireId::X(n) => set_bit(n, &mut self.x),
            WireId::Y(n) => set_bit(n, &mut self.y),
            WireId::Z(n) => set_bit(n, &mut self.z),
            WireId::Named(n) => {
                self.named[n] = value;
            },
        }
    }
}

impl Values {
    fn ensure_space(&mut self, id: WireId) {
        if let WireId::Named(n) = id {
            if n >= self.named.len() {
                self.named.resize(n + 1, false);
            }
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
enum Operation {
    And,
    Or,
    Xor,
}

impl Operation {
    #[must_use]
    pub fn apply(self, a: bool, b: bool) -> bool {
        match self {
            Operation::And => a && b,
            Operation::Or => a || b,
            Operation::Xor => a ^ b,
        }
    }
}

impl FromStr for Operation {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "AND" => Ok(Operation::And),
            "OR" => Ok(Operation::Or),
            "XOR" => Ok(Operation::Xor),
            _ => Err(format!("Invalid operation {s}")),
        }
    }
}

type Swap = SetOfTwo<WireId>;
type Swaps = BTreeSet<Swap>;

#[derive(Clone, Hash, Eq, PartialEq)]
struct Connection {
    a:   WireId,
    b:   WireId,
    op:  Operation,
    out: WireId,
}

impl Connection {
    #[must_use]
    fn value(&self, values: &Values) -> bool {
        let a = values.get(self.a);
        let b = values.get(self.b);
        self.op.apply(a, b)
    }

    fn try_to_apply_output_swap(&mut self, swap: &Swap) {
        let (a, b) = swap.clone().to_tuple_unordered();
        match (self.out == a, self.out == b) {
            (true, false) => self.out = b,
            (false, true) => self.out = a,
            _ => {},
        }
    }
}

#[derive(Clone)]
struct Connections {
    topologically_ordered: Vec<Connection>,
}

impl Connections {
    fn apply_swaps<'a>(&self, swaps: impl IntoIterator<Item = &'a Swap>) -> Option<Self> {
        let mut connections = self.topologically_ordered.clone();
        for swap in swaps {
            for connection in &mut connections {
                connection.try_to_apply_output_swap(swap);
            }
        }
        Self::from_unsorted(&connections)
    }

    fn propagate(&self, values: &mut Values) {
        for connection in &self.topologically_ordered {
            values.set(connection.out, connection.value(values));
        }
    }

    fn bit_correct(&self, bit: u8, names: &WireNames) -> bool {
        for (x, y) in [(false, false), (false, true), (true, false), (true, true)] {
            let mut values = Values::for_names(names);
            values.set(WireId::X(bit), x);
            values.set(WireId::Y(bit), y);
            self.propagate(&mut values);
            if values.get(WireId::Z(bit)) != (x ^ y) {
                return false;
            }
            if values.get(WireId::Z(bit + 1)) != (x & y) {
                return false;
            }
        }
        true
    }

    fn bits_with_errors(&self, names: &WireNames) -> u8 {
        for bit in 0 .. INPUT_BITS {
            if !self.bit_correct(bit, names) {
                return INPUT_BITS - bit;
            }
        }
        0
    }

    fn sample_adding_errors(&self, names: &WireNames, samples: usize) -> usize {
        let mut errors = 0;
        for _ in 0 .. samples {
            let mut values = Values::random_xy(names);
            self.propagate(&mut values);
            if values.z != values.x + values.y {
                errors += 1;
            }
        }
        errors
    }

    #[must_use]
    fn from_unsorted(unsorted: &[Connection]) -> Option<Self> {
        match topological_sort(unsorted, |c| {
            unsorted
                .iter()
                .filter(|&c2| c2.out == c.a || c2.out == c.b)
                .cloned()
                .collect::<Vec<_>>()
        }) {
            Ok(mut topologically_ordered) => {
                topologically_ordered.reverse();
                Some(Connections {
                    topologically_ordered,
                })
            },
            Err(_) => None,
        }
    }

    fn output_wires(&self) -> impl Iterator<Item = WireId> + '_ {
        self.topologically_ordered.iter().map(|c| c.out)
    }
}

type Input = (WireNames, Values, Connections);

fn parse(input: &str) -> Result<Input, Error> {
    let (values_string, connections_string) =
        split_into_two_segments_separated_by_double_newline(input)?;
    let values_lines = parse_lines_to_vec_passing_parser(&values_string, |value_string| {
        let (name, value) = value_string
            .split_once(": ")
            .ok_or(format!("Invalid value line {value_string}"))?;
        let b: bool = match value {
            "0" => false,
            "1" => true,
            _ => return Err(format!("Invalid value {value}")),
        };
        Ok((name.to_string(), b))
    })?;
    let connections_lines = parse_lines_to_vec_passing_parser(&connections_string, |s| {
        let (a, b) = s
            .split_once(" -> ")
            .ok_or(format!("Invalid connection line {s}"))?;
        let v = a.split_ascii_whitespace().collect::<Vec<_>>();
        assert_eq!(v.len(), 3);
        Ok((
            v[0].to_string(),
            v[1].to_string(),
            v[2].to_string(),
            b.to_string(),
        ))
    })?;

    let mut names = WireNames::default();
    let mut connections = vec![];
    for (a, op, b, out) in connections_lines {
        let a = names.get_or_insert(&a);
        let b = names.get_or_insert(&b);
        let op = op.parse()?;
        let out = names.get_or_insert(&out);
        let connection = Connection { a, b, op, out };
        connections.push(connection);
    }

    let mut values = Values::for_names(&names);
    for (name, value) in values_lines {
        let id = names.get_or_insert(&name);
        values.ensure_space(id);
        values.set(id, value);
    }

    let connections = Connections::from_unsorted(&connections).expect("Invalid connections");
    Ok((names, values, connections))
}

fn solve_1(data: Input) -> N {
    let (_names, mut values, connections) = data;
    connections.propagate(&mut values);
    values.z
}

const SAMPLES: usize = 1024;
fn success(swaps: &Swaps, connections: &Connections, names: &WireNames) -> bool {
    if swaps.len() == REQUIRED_SWAPS {
        let swapped = connections.apply_swaps(swaps.iter());
        match swapped {
            None => false,
            Some(swapped) => {
                swapped.bits_with_errors(names) == 0
                    && swapped.sample_adding_errors(names, SAMPLES) == 0
            },
        }
    } else {
        false
    }
}

const REQUIRED_SWAPS: usize = 4;

fn successors(swaps: &Swaps, connections: &Connections, names: &WireNames) -> Vec<Swaps> {
    if swaps.len() >= REQUIRED_SWAPS {
        return vec![];
    }

    let mut best = u8::MAX;
    let mut bests: Vec<Swaps> = vec![];
    for a in connections.output_wires() {
        for b in connections.output_wires() {
            if a < b {
                if let Some(swap) = Swap::new(a, b) {
                    if !swaps.contains(&swap) {
                        let mut new_swaps = swaps.clone();
                        new_swaps.insert(swap);
                        if let Some(swapped) = connections.apply_swaps(&new_swaps.clone()) {
                            let result = swapped.bits_with_errors(names);
                            match result.cmp(&best) {
                                Ordering::Less => {
                                    best = result;
                                    bests = vec![new_swaps];
                                },
                                Ordering::Equal => {
                                    bests.push(new_swaps);
                                },
                                Ordering::Greater => {},
                            }
                        }
                    }
                }
            }
        }
    }
    println!("Best: {best} for {}", debug_swaps_vec(&bests, names));
    bests
}

fn debug_swap(swap: &Swap, names: &WireNames) -> String {
    let (a, b) = swap.to_tuple_ordered();
    format!("{} <-> {}", names.name_for(*a), names.name_for(*b))
}

fn debug_swaps(swaps: &Swaps, names: &WireNames) -> String {
    format!(
        "[{}]",
        swaps
            .iter()
            .map(|s| debug_swap(s, names))
            .collect::<Vec<_>>()
            .join(", ")
    )
}

fn debug_swaps_vec(swaps: &[Swaps], names: &WireNames) -> String {
    swaps
        .iter()
        .map(|s| debug_swaps(s, names))
        .collect::<Vec<_>>()
        .join(", ")
}

fn solve_2(data: Input) -> String {
    let (names, _values, connections) = data;
    let start: Swaps = Swaps::new();
    let result = dfs(
        start,
        |current| successors(current, &connections, &names),
        |current| success(current, &connections, &names),
    )
    .expect("Expected a solution");
    let result = result
        .into_iter()
        .next_back()
        .expect("Expected a last element");
    let mut names: Vec<_> = result
        .into_iter()
        .flat_map(IntoIterator::into_iter)
        .map(|id| names.name_for(id))
        .collect();
    names.sort();
    names.join(",")
}

fn main() -> Result<(), Error> {
    let data = parse(DATA)?;

    let result_1 = solve_1(data.clone());
    println!("Part 1: {result_1}");

    let result_2 = solve_2(data);
    println!("Part 2: {result_2}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = include_str!("../../resources/24-test-00.txt");

    fn test_data() -> Input {
        parse(TEST_DATA).unwrap()
    }

    fn real_data() -> Input {
        parse(DATA).unwrap()
    }

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(test_data()), 2024);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(real_data()), 58_740_594_706_150);
    }

    #[test]
    fn value_stores_numbers() {
        let mut values = Values::for_names(&WireNames::default());
        values.set(WireId::X(0), true);
        values.set(WireId::Y(1), true);
        values.set(WireId::Z(2), true);
        assert_eq!(values.x, 1);
        assert_eq!(values.y, 2);
        assert_eq!(values.z, 4);
    }

    #[test]
    fn test_valid_swaps_yield_valid_adder() {
        let (names, _values, connections) = real_data();

        println!("{} errors initially", connections.bits_with_errors(&names));

        let swaps = Swaps::from([
            Swap::new(names.get("hbk").unwrap(), WireId::Z(14)).unwrap(),
            Swap::new(names.get("kvn").unwrap(), WireId::Z(18)).unwrap(),
            Swap::new(names.get("dbb").unwrap(), WireId::Z(23)).unwrap(),
            Swap::new(names.get("cvh").unwrap(), names.get("tfn").unwrap()).unwrap(),
        ]);

        for swap in &swaps {
            let fixed_one = connections.apply_swaps(&[swap.clone()]).unwrap();
            let errors = fixed_one.bits_with_errors(&names);
            println!("{} errors after {}", errors, debug_swap(swap, &names));
        }

        let fixed = connections.apply_swaps(&swaps).unwrap();

        let mut values = Values::random_xy(&names);
        let expected = values.x + values.y;
        fixed.propagate(&mut values);
        let obtained = values.z;
        assert_eq!(expected, obtained);

        let errors = fixed.bits_with_errors(&names);
        assert_eq!(errors, 0);
    }

    #[test]
    #[ignore]
    fn test_solve_2_real() {
        assert_eq!(solve_2(real_data()), "cvh,dbb,hbk,kvn,tfn,z14,z18,z23");
    }
}
