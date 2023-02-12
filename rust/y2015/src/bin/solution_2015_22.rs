use std::cmp::max;

use pathfinding::prelude::dijkstra;
use strum::{EnumIter, IntoEnumIterator};

use crate::Spell::{Drain, MagicMissile, Poison, Recharge, Shield};

type N = u16;

#[derive(EnumIter, Eq, PartialEq, Copy, Clone)]
enum Spell {
    MagicMissile,
    Drain,
    Shield,
    Poison,
    Recharge,
}

struct GameRules {
    hit_points_player_loses_at_start_of_each_player_turn: N,
    boss_damage: N,
}

impl Spell {
    fn cost(self) -> N {
        match self {
            MagicMissile => 53,
            Drain => 73,
            Shield => 113,
            Poison => 173,
            Recharge => 229,
        }
    }

    fn duration(self) -> Option<N> {
        match self {
            MagicMissile | Drain => None,
            Shield | Poison => Some(6),
            Recharge => Some(5),
        }
    }

    fn do_damage(self) -> N {
        match self {
            MagicMissile => 4,
            Drain => 2,
            Poison => 3,
            _ => 0,
        }
    }

    fn heal_for(self) -> N {
        match self {
            Drain => 2,
            _ => 0,
        }
    }

    fn add_armour(self) -> N {
        match self {
            Shield => 7,
            _ => 0,
        }
    }

    fn add_mana(self) -> N {
        match self {
            Recharge => 101,
            _ => 0,
        }
    }

    fn is_immediate(self) -> bool {
        self.duration().is_none()
    }
}

struct PlayerStats {
    hit_points: N,
    mana:       N,
}

struct BossStats {
    hit_points: N,
    damage:     N,
}

#[derive(Eq, PartialEq, Hash, Clone, Copy, Debug)]
struct State {
    player_hit_points:        N,
    player_mana:              N,
    boss_hit_points:          N,
    shield_spell_remaining:   Option<N>,
    poison_spell_remaining:   Option<N>,
    recharge_spell_remaining: Option<N>,
}

impl State {
    fn successors(&self, rules: &GameRules) -> Vec<(State, N)> {
        Spell::iter()
            .filter_map(|spell| self.both_turns_for_player_casting_with_cost(spell, rules))
            .collect()
    }

    fn from_stats(you: &PlayerStats, boss: &BossStats) -> State {
        State {
            player_hit_points:        you.hit_points,
            player_mana:              you.mana,
            boss_hit_points:          boss.hit_points,
            shield_spell_remaining:   None,
            poison_spell_remaining:   None,
            recharge_spell_remaining: None,
        }
    }

    fn apply_effects(self) -> State {
        let poison_damage = if self.poison_spell_remaining.is_some() {
            Poison.do_damage()
        } else {
            0
        };

        let mana_increase = if self.recharge_spell_remaining.is_some() {
            Recharge.add_mana()
        } else {
            0
        };

        let decrement =
            |x: Option<N>| -> Option<N> { x.and_then(|y| if y > 1 { Some(y - 1) } else { None }) };

        State {
            player_mana: self.player_mana + mana_increase,
            boss_hit_points: self.boss_hit_points.saturating_sub(poison_damage),
            shield_spell_remaining: decrement(self.shield_spell_remaining),
            poison_spell_remaining: decrement(self.poison_spell_remaining),
            recharge_spell_remaining: decrement(self.recharge_spell_remaining),
            ..self
        }
    }

    fn apply_spell(self, spell: Spell) -> State {
        let immediate_damage = if spell.is_immediate() {
            spell.do_damage()
        } else {
            0
        };

        let immediate_healing = if spell.is_immediate() {
            spell.heal_for()
        } else {
            0
        };

        let new_shield_spell_remaining = if spell == Shield {
            Shield.duration()
        } else {
            self.shield_spell_remaining
        };
        let new_poison_spell_remaining = if spell == Poison {
            Poison.duration()
        } else {
            self.poison_spell_remaining
        };
        let new_recharge_spell_remaining = if spell == Recharge {
            Recharge.duration()
        } else {
            self.recharge_spell_remaining
        };

        State {
            player_hit_points:        self.player_hit_points + immediate_healing,
            player_mana:              self.player_mana - spell.cost(),
            boss_hit_points:          self.boss_hit_points.saturating_sub(immediate_damage),
            shield_spell_remaining:   new_shield_spell_remaining,
            poison_spell_remaining:   new_poison_spell_remaining,
            recharge_spell_remaining: new_recharge_spell_remaining,
        }
    }

    fn boss_attacks(self, rules: &GameRules) -> State {
        let effective_armour = if self.shield_spell_remaining.is_some() {
            Shield.add_armour()
        } else {
            0
        };

        let damage = max(rules.boss_damage - effective_armour, 1);
        let new_player_hit_points = self.player_hit_points.saturating_sub(damage);

        State {
            player_hit_points: new_player_hit_points,
            ..self
        }
    }

    fn player_loses_hit_points(self, amount: N) -> Self {
        State {
            player_hit_points: self.player_hit_points.saturating_sub(amount),
            ..self
        }
    }

    fn both_turns_for_player_casting_with_cost(
        self,
        spell: Spell,
        rules: &GameRules,
    ) -> Option<(State, N)> {
        self.both_turns_for_player_casting(spell, rules)
            .map(|result| (result, spell.cost()))
    }

    fn is_duplicate(&self, spell: Spell) -> bool {
        let sh = spell == Shield && self.shield_spell_remaining.is_some();
        let po = spell == Poison && self.poison_spell_remaining.is_some();
        let re = spell == Recharge && self.recharge_spell_remaining.is_some();
        // You cannot cast a spell that would start an effect which is already active.
        sh || po || re
    }

    fn both_turns_for_player_casting(self, spell: Spell, rules: &GameRules) -> Option<State> {
        let state = self
            .player_loses_hit_points(rules.hit_points_player_loses_at_start_of_each_player_turn);
        if state.player_hit_points == 0 {
            None
        } else {
            let state = state.apply_effects();

            // can afford and is it a valid spell?
            if state.player_mana >= spell.cost() && !state.is_duplicate(spell) {
                if state.boss_hit_points == 0 {
                    Some(state)
                } else {
                    let state = state.apply_spell(spell);
                    if state.boss_hit_points == 0 {
                        Some(state)
                    } else {
                        let state = state.apply_effects();
                        if state.boss_hit_points == 0 {
                            Some(state)
                        } else {
                            Some(state.boss_attacks(rules))
                        }
                    }
                }
            } else {
                None
            }
        }
    }
}

fn solve(you: &PlayerStats, boss: &BossStats, rules: &GameRules) -> N {
    let start = State::from_stats(you, boss);

    // What is the least amount of mana you can spend and still win the fight?
    let (_path, cost) = dijkstra(
        &start,
        |state| state.successors(rules),
        |state| state.boss_hit_points == 0,
    )
    .unwrap_or_else(|| panic!("Not found!"));
    cost
}

const BOSS: BossStats = BossStats {
    hit_points: 71,
    damage:     10,
};

const YOU: PlayerStats = PlayerStats {
    hit_points: 50,
    mana:       500,
};

fn solve_1(you: &PlayerStats, boss: &BossStats) -> N {
    let rules: GameRules = GameRules {
        hit_points_player_loses_at_start_of_each_player_turn: 0,
        boss_damage: boss.damage,
    };

    solve(you, boss, &rules)
}

fn solve_2(you: &PlayerStats, boss: &BossStats) -> N {
    let rules: GameRules = GameRules {
        hit_points_player_loses_at_start_of_each_player_turn: 1,
        boss_damage: boss.damage,
    };

    solve(you, boss, &rules)
}

fn main() {
    let result_1 = solve_1(&YOU, &BOSS);
    println!("Part 1: {result_1}");

    let result_2 = solve_2(&YOU, &BOSS);
    println!("Part 2: {result_2}");
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Spell::MagicMissile;

    #[test]
    fn test_solve_1_test_1() {
        let you = PlayerStats {
            hit_points: 10,
            mana:       250,
        };
        let boss = BossStats {
            hit_points: 13,
            damage:     8,
        };

        let rules = GameRules {
            hit_points_player_loses_at_start_of_each_player_turn: 0,
            boss_damage: boss.damage,
        };

        let a = State::from_stats(&you, &boss);
        let b = a.both_turns_for_player_casting(Poison, &rules).unwrap();
        assert_eq!(b.player_hit_points, 2);
        assert_eq!(b.player_mana, 77);
        assert_eq!(b.boss_hit_points, 10);
        let c = b
            .both_turns_for_player_casting(MagicMissile, &rules)
            .unwrap();
        assert_eq!(c.player_hit_points, 2);
        assert_eq!(c.player_mana, 24);
        assert_eq!(c.boss_hit_points, 0);

        assert_eq!(solve_1(&you, &boss), 250 - 24);
    }

    #[test]
    fn test_solve_1_test_2() {
        let you = PlayerStats {
            hit_points: 10,
            mana:       250,
        };
        let boss = BossStats {
            hit_points: 14,
            damage:     8,
        };

        assert_eq!(solve_1(&you, &boss), 641);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(&YOU, &BOSS), 1824);
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(&YOU, &BOSS), 1937);
    }
}
