extern crate core;

use std::ops::{Add, Rem};
use crate::Command::{Cut, DealIntoNewStack, DealWithIncrement};
use itertools::Itertools;
use num_bigint::BigUint;
use num_integer::{Integer};
use num_traits::{ToPrimitive, Zero};

const DATA: &str = include_str!("../../resources/22.txt");

/**
    if len = 10 and n = 3:
        pos 0 => 0, because 0 * n
        pos 21 => 7, because 7 * n
        pos 12 => 4, because 4 * n
        pos 3 => 1, because 1 * n
        pos 24 => 8, because 8 * n
        pos 15 => 5, because 5 * n
        pos 6 => 2, because 2 * n
        pos 27 => 9, because 9 * n
        pos 18 => 6, because 6 * n
        pos 9 => 3, because 3 * n
 */
fn magic(len: usize, n: usize, pos: usize) -> usize {
    let mut attempt: BigUint = BigUint::from(pos);
    let len = BigUint::from(len);
    let n = BigUint::from(n);
    let pos = BigUint::from(pos);

    loop {
        let attempt_rem_len = attempt.clone().rem(&len);
        let (attempt_div_n, attempt_rem_n) = attempt.clone().div_rem(&n);
        // if attempt % len == pos && attempt % n == 0 {
        if attempt_rem_len == pos && attempt_rem_n.is_zero() {
            // return attempt / n;
            return attempt_div_n.to_usize().unwrap();
        }

        //    attempt += len;
        attempt = attempt.add(&len);
    }
}

type Card = usize;

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
struct Deck<T> {
    cards: Vec<T>,
}

impl<T: Copy + Clone + Eq> Deck<T> {
    fn new<F>(size: usize, create: F) -> Deck<T>
    where
        F: Fn(usize) -> T,
    {
        Deck {
            cards: (0..size).map(create).collect(),
        }
    }

    fn apply_many(&self, commands: &[Command]) -> Deck<T> {
        commands
            .iter()
            .fold(self.clone(), |d, c| d.apply_command(&c))
    }

    fn apply_command(&self, command: &Command) -> Deck<T> {
        match command {
            DealIntoNewStack => {
                let mut cards = self.cards.clone();
                cards.reverse();
                Deck { cards }
            }
            Cut(n) if *n > 0 => {
                let m = *n as usize;
                Deck {
                    cards: vec![&self.cards[m..], &self.cards[..m]].concat(),
                }
            }
            Cut(n) => {
                let m = (self.cards.len() as isize + *n) as usize;
                Deck {
                    cards: vec![&self.cards[m..], &self.cards[..m]].concat(),
                }
            }
            DealWithIncrement(n) => {
                // This can be improved but it works
                let len = self.cards.len();
                let factory = Deck::new(len, |idx| idx).cards;
                let index: Vec<_> = (0..len).map(|idx| factory[(idx * n) % len]).collect();
                let mapping: Vec<_> = (0..len)
                    .map(|n| index.iter().position(|x| *x == n).unwrap())
                    .collect();
                Deck {
                    cards: mapping.iter().map(|x| self.cards[*x as usize]).collect(),
                }
            }
        }
    }

    fn find(&self, card: T) -> Option<usize> {
        self.cards
            .iter()
            .find_position(|c| **c == card)
            .map(|(idx, _)| idx)
    }
}

enum Command {
    DealWithIncrement(usize),
    Cut(isize),
    DealIntoNewStack,
}

impl Command {
    fn parse(s: &str) -> Command {
        const DEAL_INTO_NEW_STACK_COMMAND: &str = "deal into new stack";
        const DEAL_WITH_INCREMENT_PREFIX: &str = "deal with increment ";
        const CUT_PREFIX: &str = "cut ";

        if s == DEAL_INTO_NEW_STACK_COMMAND {
            DealIntoNewStack
        } else if s.starts_with(DEAL_WITH_INCREMENT_PREFIX) {
            DealWithIncrement(s[DEAL_WITH_INCREMENT_PREFIX.len()..].parse().unwrap())
        } else if s.starts_with(CUT_PREFIX) {
            Cut(s[CUT_PREFIX.len()..].parse().unwrap())
        } else {
            panic!("Unexpected {}", s);
        }
    }

    fn parse_many(data: &str) -> Vec<Command> {
        data.split('\n')
            .filter(|s| !s.is_empty())
            .map(Command::parse)
            .collect()
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
struct FocusedDeck {
    size: usize,
}

impl FocusedDeck {
    fn new(size: usize) -> FocusedDeck {
        FocusedDeck { size }
    }

    fn apply_many(&self, commands: &[Command], focus_on_position: usize) -> Card {
        if commands.is_empty() {
            return focus_on_position;
        }

        let command = &commands.last().unwrap();
        let tail = &commands[..commands.len() - 1];

        match command {
            DealWithIncrement(n) => {
                self.apply_many(
                    tail,
                    magic(self.size, *n, focus_on_position),
                )
            },
            Cut(n) => {
                let len = self.size as isize;

                self.apply_many(
                    tail,
                    ((focus_on_position as isize + len + *n) % len) as usize,
                )
            },
            DealIntoNewStack => {
                self.apply_many(tail, self.size - focus_on_position - 1)
            },
        }
    }
}

fn solve_1(data: &str) -> Option<usize> {
    let commands = Command::parse_many(data);
    let deck: Deck<Card> = Deck::new(10007, |idx| idx);
    let result = deck.apply_many(&commands);
    result.find(2019)
}

fn solve_2(data: &str) -> Card {
    let commands = Command::parse_many(data);
    let deck = FocusedDeck::new(119_315_717_514_047);
    let times: usize = 101_741_582_076_661;

    // TODO: apply more times
    let resulting = deck.apply_many(&commands, 2020);

    return resulting
}

fn main() {
    let result = solve_1(DATA);
    println!("Part 1: {:?}", result);

    let result = solve_2(DATA);
    println!("Part 2: {:?}", result);
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_helper_full(commands: &[Command], expected: &[Card]) {
        let deck = Deck::new(expected.len(), |idx| idx);
        let obtained = deck.apply_many(commands);
        assert_eq!(obtained.cards, expected);
    }

    fn test_helper_focused(commands: &[Command], expected: &[Card]) {
        let deck = FocusedDeck::new(expected.len());
        let obtained: Vec<Card> = (0..expected.len())
            .map(|idx| deck.apply_many(commands, idx))
            .collect();

        assert_eq!(obtained, expected);
    }

    fn test_helper(data: &str, expected_str: &str) {
        let commands = Command::parse_many(data);
        let expected: Vec<Card> = expected_str
            .split(' ')
            .map(|x| x.parse().unwrap())
            .collect();

        test_helper_full(&commands, &expected);
        test_helper_focused(&commands, &expected);
    }

    #[test]
    fn test_deal_into_new_stack() {
        test_helper("deal into new stack", "9 8 7 6 5 4 3 2 1 0");
    }

    #[test]
    fn test_cut_positive_n() {
        test_helper("cut 3", "3 4 5 6 7 8 9 0 1 2");
    }

    #[test]
    fn test_cut_negative_n() {
        test_helper("cut -4", "6 7 8 9 0 1 2 3 4 5");
    }

    #[test]
    fn test_multi_cut() {
        test_helper("cut 3\ncut 5", "8 9 0 1 2 3 4 5 6 7");
    }

    #[test]
    fn test_reverse_then_cut() {
        test_helper("deal into new stack\ncut 3", "6 5 4 3 2 1 0 9 8 7");
    }

    #[test]
    fn test_deal_with_increment() {
        test_helper("deal with increment 3", "0 7 4 1 8 5 2 9 6 3");
    }

    #[test]
    fn test_deal_with_increment_alternate() {
        test_helper(
            "cut 1\ndeal with increment 3",
            "1 8 5 2 9 6 3 0 7 4",
        );
    }

    #[test]
    fn test_solve_1_1() {
        let data = "deal with increment 7
deal into new stack
deal into new stack";

        test_helper(data, "0 3 6 9 2 5 8 1 4 7");
    }

    #[test]
    fn test_solve_1_2() {
        let data = "cut 6
deal with increment 7
deal into new stack";

        test_helper(data, "3 0 7 4 1 8 5 2 9 6");
    }

    #[test]
    fn test_solve_1_3() {
        let data = "deal with increment 7
deal with increment 9
cut -2";

        test_helper(data, "6 3 0 7 4 1 8 5 2 9");
    }

    #[test]
    fn test_solve_1_4() {
        let data = "deal into new stack
cut -2
deal with increment 7
cut 8
cut -4
deal with increment 7
cut 3
deal with increment 9
deal with increment 3
cut -1";

        test_helper(data, "9 2 5 8 1 4 7 0 3 6");
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(DATA), Some(4485));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(DATA), 123_456);
    }
}
