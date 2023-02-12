use std::cmp::{max, min};
use std::collections::HashMap;
use std::fmt::Debug;

use itertools::Itertools;
use memoize::memoize;

const DATA: (u32, u32) = (3, 7);

trait Die {
    fn new() -> Self;
    fn roll(&self) -> (u32, Self);
}

#[derive(Clone, Debug)]
struct DeterministicDie {
    next: u32,
}

impl Die for DeterministicDie {
    fn new() -> Self {
        DeterministicDie { next: 1 }
    }

    fn roll(&self) -> (u32, Self) {
        (
            self.next,
            if self.next == 100 {
                DeterministicDie::new()
            } else {
                DeterministicDie {
                    next: self.next + 1,
                }
            },
        )
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Copy)]
enum Player {
    One = 1,
    Two = 2,
}

impl Player {
    fn other(self) -> Player {
        match self {
            Player::One => Player::Two,
            Player::Two => Player::One,
        }
    }
}

#[derive(Clone, Debug)]
struct Game {
    die_rolled_times:  u32,
    player_1_points:   u32,
    player_2_points:   u32,
    player_1_position: u32,
    player_2_position: u32,
    next_to_move:      Player,
}

const QUANTUM_WIN_AT: u32 = 21;

impl Game {
    fn new(starting_positions: (u32, u32)) -> Game {
        let (p1, p2) = starting_positions;
        Game {
            die_rolled_times:  0,
            player_1_points:   0,
            player_2_points:   0,
            player_1_position: p1,
            player_2_position: p2,
            next_to_move:      Player::One,
        }
    }

    fn wrap_position(position: u32) -> u32 {
        if position > 10 {
            Game::wrap_position(position - 10)
        } else {
            position
        }
    }

    fn lagging_player_points(&self) -> u32 {
        min(self.player_1_points, self.player_2_points)
    }

    fn leading_player_points(&self) -> u32 {
        max(self.player_1_points, self.player_2_points)
    }

    fn three_quantum_dice_roll_frequencies() -> HashMap<u32, usize> {
        let mut result: Vec<u32> = Vec::new();
        for a in 1 ..= 3 {
            for b in 1 ..= 3 {
                for c in 1 ..= 3 {
                    result.push(a + b + c);
                }
            }
        }

        result.into_iter().counts()
    }

    fn play_until_points<D: Die>(&self, points: u32, die: D) -> Game {
        let mut state: Game = self.clone();
        let mut die: D = die;

        while state.leading_player_points() < points {
            let (r1, da) = die.roll();
            let (r2, db) = da.roll();
            let (r3, dc) = db.roll();
            die = dc;
            let moves = r1 + r2 + r3;

            state = state.next_moves_as(moves);
        }
        state
    }

    fn next_moves_as(&self, moves: u32) -> Game {
        let die_rolled_times = self.die_rolled_times + 3;
        let next_to_move = self.next_to_move.other();

        let (player_1_points, player_2_points, player_1_position, player_2_position) =
            match self.next_to_move {
                Player::One => {
                    let player_1_position = Game::wrap_position(self.player_1_position + moves);
                    let player_1_points = self.player_1_points + player_1_position;
                    (
                        player_1_points,
                        self.player_2_points,
                        player_1_position,
                        self.player_2_position,
                    )
                },
                Player::Two => {
                    let player_2_position = Game::wrap_position(self.player_2_position + moves);
                    let player_2_points = self.player_2_points + player_2_position;
                    (
                        self.player_1_points,
                        player_2_points,
                        self.player_1_position,
                        player_2_position,
                    )
                },
            };

        Game {
            die_rolled_times,
            player_1_points,
            player_2_points,
            player_1_position,
            player_2_position,
            next_to_move,
        }
    }

    fn win_counts(&self) -> (usize, usize) {
        if self.player_1_points >= QUANTUM_WIN_AT {
            (1, 0)
        } else if self.player_2_points >= QUANTUM_WIN_AT {
            (0, 1)
        } else {
            quantum_win_counts(
                self.player_1_points,
                self.player_2_points,
                self.player_1_position,
                self.player_2_position,
                self.next_to_move,
            )
        }
    }
}

fn solve_1(starting_positions: (u32, u32)) -> u32 {
    let game = Game::new(starting_positions);
    let die = DeterministicDie::new();
    let result = game.play_until_points(1000, die);
    result.lagging_player_points() * result.die_rolled_times
}

#[memoize]
fn quantum_win_counts(
    player_1_points: u32,
    player_2_points: u32,
    player_1_position: u32,
    player_2_position: u32,
    next_to_move: Player,
) -> (usize, usize) {
    let mut p1 = 0;
    let mut p2 = 0;

    let game = Game {
        die_rolled_times: 0, // we do not care about this field
        player_1_points,
        player_2_points,
        player_1_position,
        player_2_position,
        next_to_move,
    };

    for (roll, count) in Game::three_quantum_dice_roll_frequencies() {
        let (dp1, dp2) = game.next_moves_as(roll).win_counts();
        p1 += dp1 * count;
        p2 += dp2 * count;
    }

    (p1, p2)
}

fn solve_2(starting_positions: (u32, u32)) -> usize {
    let game = Game::new(starting_positions);
    let (a, b) = game.win_counts();
    max(a, b)
}

fn main() {
    let result_1 = solve_1(DATA);
    println!("Part 1: {result_1:?}");

    let result_2 = solve_2(DATA);
    println!("Part 2: {result_2:?}");
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: (u32, u32) = (4, 8);

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(TEST_DATA), 739_785);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(DATA), 1_006_866);
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve_2(TEST_DATA), 444_356_092_776_315);
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(DATA), 273_042_027_784_929);
    }
}
