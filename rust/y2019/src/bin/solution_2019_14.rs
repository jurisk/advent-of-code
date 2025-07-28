#![feature(int_roundings)]

use std::collections::{HashMap, VecDeque};
use std::fmt::{Debug, Formatter};

use itertools::Itertools;

type Number = u64;

const MEGA_ORE: Number = 1_000_000_000_000;

#[derive(PartialEq, Eq, Hash, Clone)]
struct Ingredient {
    name: String,
}

impl Ingredient {
    fn new(s: &str) -> Ingredient {
        Ingredient {
            name: s.to_string(),
        }
    }
}

impl Debug for Ingredient {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Ingredient {
    fn parse_with_quantity(s: &str) -> (Ingredient, Number) {
        let elements: Vec<_> = s.split(' ').collect();
        assert_eq!(elements.len(), 2);
        let ingredient = Ingredient {
            name: elements[1].to_string(),
        };
        let number = elements[0].parse::<Number>().unwrap();
        (ingredient, number)
    }
}

#[derive(PartialEq, Eq, Clone)]
struct Conversion {
    pub from:          HashMap<Ingredient, Number>,
    pub to_ingredient: Ingredient,
    pub to_quantity:   Number,
}

impl Debug for Conversion {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?} => [{:?}: {:?}]",
            self.from, self.to_ingredient, self.to_quantity
        )
    }
}

impl Conversion {
    fn parse(s: &str) -> Conversion {
        let left_and_right: Vec<_> = s.split(" => ").collect();
        assert_eq!(left_and_right.len(), 2);
        let from: HashMap<Ingredient, Number> = left_and_right[0]
            .split(", ")
            .map(Ingredient::parse_with_quantity)
            .collect();
        let (to_ingredient, to_quantity) = Ingredient::parse_with_quantity(left_and_right[1]);
        Conversion {
            from,
            to_ingredient,
            to_quantity,
        }
    }
}

fn ore_for_n_fuel(conversions: &HashMap<Ingredient, Conversion>, fuel_required: Number) -> Number {
    let fuel = Ingredient::new("FUEL");
    let ore = Ingredient::new("ORE");

    let mut ore_needed: Number = 0;
    let mut orders: VecDeque<(Ingredient, Number)> = VecDeque::new();
    let mut leftovers: HashMap<Ingredient, Number> = HashMap::new();
    for k in conversions.keys() {
        leftovers.insert(k.clone(), 0);
    }
    orders.push_back((fuel, fuel_required));

    while !orders.is_empty() {
        let (order_ingredient, order_amount) = orders.pop_front().unwrap();
        if order_ingredient == ore {
            ore_needed += order_amount;
        } else if order_amount <= *leftovers.get(&order_ingredient).unwrap() {
            *leftovers.get_mut(&order_ingredient).unwrap() -= order_amount;
        } else {
            let amount_needed = order_amount - leftovers.get(&order_ingredient).unwrap_or(&0);
            let recipe = conversions.get(&order_ingredient).unwrap();
            let batches_to_make = amount_needed.div_ceil(recipe.to_quantity);
            recipe.from.iter().for_each(|(k, v)| {
                orders.push_back((k.clone(), v * batches_to_make));
            });
            let leftover_amount = batches_to_make * recipe.to_quantity - amount_needed;
            leftovers.insert(order_ingredient, leftover_amount);
        }
    }

    println!("{ore_needed} ore is needed to produce {fuel_required} fuel");

    ore_needed
}

fn parse_data(data: &str) -> HashMap<Ingredient, Conversion> {
    let conversions_vec: Vec<_> = data
        .lines()
        .filter(|x| !x.is_empty())
        .map(Conversion::parse)
        .collect();
    let conversions: HashMap<Ingredient, Conversion> = conversions_vec
        .iter()
        .into_group_map_by(|x| x.to_ingredient.clone())
        .iter()
        .map(|(k, v)| {
            assert_eq!(v.len(), 1);
            (k.clone(), v[0].clone())
        })
        .collect();
    conversions
}

fn ore_for_1_fuel_str(data: &str) -> Number {
    let conversions = parse_data(data);
    ore_for_n_fuel(&conversions, 1)
}

#[allow(clippy::redundant_else)]
fn fuel_from_mega_ore(conversions: &HashMap<Ingredient, Conversion>) -> Number {
    let one_fuel_cost = ore_for_n_fuel(conversions, 1);
    println!("One fuel cost {one_fuel_cost}");
    let mut start: Number = MEGA_ORE.div_floor(one_fuel_cost);
    assert!(ore_for_n_fuel(conversions, start) < MEGA_ORE);
    let mut end: Number = (MEGA_ORE * 2).div_floor(one_fuel_cost);
    assert!(ore_for_n_fuel(conversions, end) > MEGA_ORE);

    // binary search
    loop {
        if end - start == 1 {
            return start;
        } else {
            let mid: Number = u64::midpoint(start, end);
            let required_ore = ore_for_n_fuel(conversions, mid);
            if required_ore > MEGA_ORE {
                end = mid;
            } else {
                start = mid;
            }
        }
    }
}

fn fuel_from_mega_ore_str(data: &str) -> Number {
    let conversions = parse_data(data);
    fuel_from_mega_ore(&conversions)
}

fn solve_1() {
    let result = ore_for_1_fuel_str(include_str!("../../resources/14.txt"));
    println!("Part 1: {result}");
    assert_eq!(result, 654_909);
}

fn solve_2() {
    let result = fuel_from_mega_ore_str(include_str!("../../resources/14.txt"));
    println!("Part 2: {result}");
    assert_eq!(result, 2_876_992);
}

fn main() {
    solve_1();
    solve_2();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_1_1() {
        assert_eq!(
            ore_for_1_fuel_str(
                "\
10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL"
            ),
            31,
        );
    }

    #[test]
    fn test_1_2() {
        assert_eq!(
            ore_for_1_fuel_str(
                "\
9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL"
            ),
            165,
        );
    }

    #[test]
    fn test_1_3() {
        let data = "157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT";

        assert_eq!(ore_for_1_fuel_str(data), 13312);
        assert_eq!(fuel_from_mega_ore_str(data), 82_892_753);
    }

    #[test]
    fn test_1_4() {
        let data = "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
17 NVRVD, 3 JNWZP => 8 VPVL
53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
22 VJHF, 37 MNCFX => 5 FWMGM
139 ORE => 4 NVRVD
144 ORE => 7 JNWZP
5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
145 ORE => 6 MNCFX
1 NVRVD => 8 CXFTF
1 VJHF, 6 MNCFX => 4 RFSQX
176 ORE => 6 VJHF";

        assert_eq!(ore_for_1_fuel_str(data), 180_697);
        assert_eq!(fuel_from_mega_ore_str(data), 5_586_022);
    }

    #[test]
    fn test_1_5() {
        let data = "171 ORE => 8 CNZTR
7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
114 ORE => 4 BHXH
14 VRPVC => 6 BMBT
6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
5 BMBT => 4 WPTQ
189 ORE => 9 KTJDG
1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
12 VRPVC, 27 CNZTR => 2 XDBXC
15 KTJDG, 12 BHXH => 5 XCVML
3 BHXH, 2 VRPVC => 7 MZWV
121 ORE => 7 VRPVC
7 XCVML => 6 RJRHP
5 BHXH, 4 VRPVC => 5 LTCX";

        assert_eq!(ore_for_1_fuel_str(data), 2_210_736);
        assert_eq!(fuel_from_mega_ore_str(data), 460_664);
    }

    #[test]
    fn test_solve_1() {
        solve_1();
    }

    #[test]
    fn test_solve_2() {
        solve_2();
    }
}
