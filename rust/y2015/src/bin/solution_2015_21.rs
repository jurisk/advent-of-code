use std::cmp::max;
use std::ops::Add;

use itertools::Itertools;

#[derive(Clone, Copy)]
struct Item {
    cost:   u16,
    damage: u16,
    armour: u16,
}

impl Add for Item {
    type Output = Item;

    fn add(self, rhs: Self) -> Self::Output {
        Item {
            cost:   self.cost + rhs.cost,
            damage: self.damage + rhs.damage,
            armour: self.armour + rhs.armour,
        }
    }
}

struct Stats {
    hit_points: u16,
    damage:     u16,
    armour:     u16,
    cost:       u16,
}

// You must buy exactly one weapon; no dual-wielding.
const WEAPONS: [Item; 5] = [
    Item {
        cost:   8,
        damage: 4,
        armour: 0,
    },
    Item {
        cost:   10,
        damage: 5,
        armour: 0,
    },
    Item {
        cost:   25,
        damage: 6,
        armour: 0,
    },
    Item {
        cost:   40,
        damage: 7,
        armour: 0,
    },
    Item {
        cost:   74,
        damage: 8,
        armour: 0,
    },
];

// Armor is optional, but you can't use more than one.
const ARMOUR: [Item; 5] = [
    Item {
        cost:   13,
        damage: 0,
        armour: 1,
    },
    Item {
        cost:   31,
        damage: 0,
        armour: 2,
    },
    Item {
        cost:   53,
        damage: 0,
        armour: 3,
    },
    Item {
        cost:   75,
        damage: 0,
        armour: 4,
    },
    Item {
        cost:   102,
        damage: 0,
        armour: 5,
    },
];

// You can buy 0-2 rings (at most one for each hand).
const RINGS: [Item; 6] = [
    Item {
        cost:   25,
        damage: 1,
        armour: 0,
    },
    Item {
        cost:   50,
        damage: 2,
        armour: 0,
    },
    Item {
        cost:   100,
        damage: 3,
        armour: 0,
    },
    Item {
        cost:   20,
        damage: 0,
        armour: 1,
    },
    Item {
        cost:   40,
        damage: 0,
        armour: 2,
    },
    Item {
        cost:   80,
        damage: 0,
        armour: 3,
    },
];

const BOSS: Stats = Stats {
    hit_points: 109,
    damage:     8,
    armour:     2,
    cost:       0,
};

fn wins(player: &Stats, boss: &Stats) -> bool {
    // Faster implementations exist, as we don't actually have to simulate every
    // round - but this is simpler
    let mut player_hit_points = player.hit_points;
    let mut boss_hit_points = boss.hit_points;

    let player_effective_damage = max(player.damage.saturating_sub(boss.armour), 1);
    let boss_effective_damage = max(boss.damage.saturating_sub(player.armour), 1);

    loop {
        boss_hit_points = boss_hit_points.saturating_sub(player_effective_damage);
        if boss_hit_points == 0 {
            return true;
        }

        player_hit_points = player_hit_points.saturating_sub(boss_effective_damage);
        if player_hit_points == 0 {
            return false;
        }
    }
}

fn calculate_stats(weapons: &[Item], armour: &[Item], rings: &[Item]) -> Stats {
    let all: Vec<Item> = [weapons, armour, rings].concat();
    let joined = all.into_iter().reduce(|a, b| a + b).unwrap();
    Stats {
        hit_points: 100,
        damage:     joined.damage,
        armour:     joined.armour,
        cost:       joined.cost,
    }
}

fn item_combinations<const N: usize>(items: [Item; N], valid_counts: &[usize]) -> Vec<Vec<Item>> {
    valid_counts
        .iter()
        .flat_map(|&count| {
            items
                .into_iter()
                .combinations(count)
                .collect::<Vec<Vec<Item>>>()
        })
        .collect()
}

fn all_stats() -> Vec<Stats> {
    let weapons_options: Vec<Vec<Item>> = item_combinations(WEAPONS, &[1]);
    let armour_options: Vec<Vec<Item>> = item_combinations(ARMOUR, &[0, 1]);
    let rings_options: Vec<Vec<Item>> = item_combinations(RINGS, &[0, 1, 2]);

    weapons_options
        .into_iter()
        .cartesian_product(armour_options)
        .cartesian_product(rings_options)
        .map(|((w, a), r)| calculate_stats(&w, &a, &r))
        .collect()
}

fn solve(boss: &Stats, cost_multiplier: i32, expected_to_win: bool) -> u16 {
    all_stats()
        .into_iter()
        .sorted_by_key(|setup| i32::from(setup.cost) * cost_multiplier)
        .find(|setup| wins(setup, boss) == expected_to_win)
        .unwrap()
        .cost
}

fn solve_1(boss: &Stats) -> u16 {
    solve(boss, 1, true)
}

fn solve_2(boss: &Stats) -> u16 {
    solve(boss, -1, false)
}

fn main() {
    let result_1 = solve_1(&BOSS);
    println!("Part 1: {result_1}");

    let result_2 = solve_2(&BOSS);
    println!("Part 2: {result_2}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_wins() {
        let player = Stats {
            hit_points: 8,
            damage:     5,
            armour:     5,
            cost:       0,
        };

        let boss = Stats {
            hit_points: 12,
            damage:     7,
            armour:     2,
            cost:       0,
        };

        assert!(wins(&player, &boss))
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(&BOSS), 111);
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(&BOSS), 188);
    }
}
