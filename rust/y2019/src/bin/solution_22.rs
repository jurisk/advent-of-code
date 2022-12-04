extern crate core;

use crate::Command::{Cut, DealIntoNewStack, DealWithIncrement};
use itertools::Itertools;
use num_bigint::BigInt;
use num_bigint::ToBigInt;
use num_traits::{One, ToPrimitive, Zero};

const DATA: &str = include_str!("../../resources/22.txt");

type Card = usize;

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
struct SimpleDeck<T> {
    cards: Vec<T>,
}

impl<T: Copy + Clone + Eq> SimpleDeck<T> {
    fn new<F>(size: usize, create: F) -> SimpleDeck<T>
    where
        F: Fn(usize) -> T,
    {
        SimpleDeck {
            cards: (0..size).map(create).collect(),
        }
    }

    fn apply_many(&self, commands: &[Command]) -> SimpleDeck<T> {
        commands
            .iter()
            .fold(self.clone(), |d, c| d.apply_command(c))
    }

    fn apply_command(&self, command: &Command) -> SimpleDeck<T> {
        match command {
            DealIntoNewStack => {
                let mut cards = self.cards.clone();
                cards.reverse();
                SimpleDeck { cards }
            }
            Cut(n) if *n > 0 => {
                let m = *n as usize;
                SimpleDeck {
                    cards: vec![&self.cards[m..], &self.cards[..m]].concat(),
                }
            }
            Cut(n) => {
                let m = (self.cards.len() as isize + *n) as usize;
                SimpleDeck {
                    cards: vec![&self.cards[m..], &self.cards[..m]].concat(),
                }
            }
            DealWithIncrement(n) => {
                // This can be improved but it works
                let len = self.cards.len();
                let factory = SimpleDeck::new(len, |idx| idx).cards;
                let index: Vec<_> = (0..len).map(|idx| factory[(idx * n) % len]).collect();
                let mapping: Vec<_> = (0..len)
                    .map(|n| index.iter().position(|x| *x == n).unwrap())
                    .collect();
                SimpleDeck {
                    cards: mapping.iter().map(|x| self.cards[*x]).collect(),
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
        } else if let Some(s) = s.strip_prefix(DEAL_WITH_INCREMENT_PREFIX) {
            DealWithIncrement(s.parse().unwrap())
        } else if let Some(s) = s.strip_prefix(CUT_PREFIX) {
            Cut(s.parse().unwrap())
        } else {
            panic!("Unexpected {s}");
        }
    }

    fn parse_many(data: &str) -> Vec<Command> {
        data.split('\n')
            .filter(|s| !s.is_empty())
            .map(Command::parse)
            .collect()
    }
}

// Approach from https://www.reddit.com/r/adventofcode/comments/ee0rqi/comment/fbnkaju/
// Note - this only works for Part 2 numbers as they are all prime
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
struct PowerDeck {
    size: BigInt,
    offset: BigInt,
    increment: BigInt,
}

impl PowerDeck {
    fn new(size: usize) -> PowerDeck {
        PowerDeck {
            size: size.to_bigint().unwrap(),
            offset: BigInt::zero(),
            increment: BigInt::one(),
        }
    }

    fn at(&self, idx: usize) -> Card {
        let offset = self.offset.clone();
        let increment = self.increment.clone();
        let size = self.size.clone();
        let result = (offset + idx * increment) % size.clone();
        let result = (result + size.clone()) % size;
        result.to_usize().unwrap_or_else(|| panic!("{result:?}"))
    }

    fn apply_many(&self, commands: &[Command]) -> PowerDeck {
        commands
            .iter()
            .fold(self.clone(), |d, c| d.apply_command(c))
    }

    fn inv(&self, n: &BigInt) -> BigInt {
        // gets the modular inverse of n
        // as length is prime, use Euler's theorem
        let exponent = self.size.clone() - BigInt::from(2);
        n.modpow(&exponent, &self.size.clone())
    }

    fn apply_command(&self, command: &Command) -> PowerDeck {
        match command {
            DealWithIncrement(n) => {
                // difference between two adjacent numbers is multiplied by the inverse of the increment.
                // increment_mul *= inv(q)
                let q = n.to_bigint().unwrap();
                let mul_with = self.inv(&q);
                let increment = self.increment.clone() * mul_with;

                // increment_mul %= cards
                let increment = increment % self.size.clone();

                PowerDeck {
                    size: self.size.clone(),
                    offset: self.offset.clone(),
                    increment,
                }
            }
            Cut(n) => {
                let q = n.to_bigint().unwrap();
                // shift q left
                // offset_diff += q * increment_mul
                let add_to_offset = q * self.increment.clone();
                let offset = self.offset.clone() + add_to_offset;

                // offset_diff %= cards
                let offset = offset % self.size.clone();

                PowerDeck {
                    size: self.size.clone(),
                    offset,
                    increment: self.increment.clone(),
                }
            }
            DealIntoNewStack => {
                // reverse sequence - instead of going up, go down.
                // increment_mul *= -1
                let increment = self.increment.clone() * BigInt::from(-1);
                // increment_mul %= cards
                let increment = increment % self.size.clone();

                // then shift 1 left
                // offset_diff += increment_mul
                let offset = self.offset.clone() + increment.clone();
                // offset_diff %= cards
                let offset = offset % self.size.clone();

                PowerDeck {
                    size: self.size.clone(),
                    offset,
                    increment,
                }
            }
        }
    }

    fn many_times(&self, n: usize) -> PowerDeck {
        // calculate (increment, offset) for the number of iterations of the process
        // increment = increment_mul^iterations
        let iterations = BigInt::from(n);
        // increment = pow(increment_mul, iterations, cards)
        let increment = self.increment.modpow(&iterations, &self.size);

        // offset = 0 + offset_diff * (1 + increment_mul + increment_mul^2 + ... + increment_mul^iterations)
        // use geometric series.
        // offset = offset_diff * (1 - increment) * inv((1 - increment_mul) % cards)
        let offset =
            self.offset.clone() * (1 - increment.clone()) * self.inv(&(1 - self.increment.clone()));

        // offset %= cards
        let offset = offset % self.size.clone();

        PowerDeck {
            size: self.size.clone(),
            offset,
            increment,
        }
    }
}

fn solve_1(data: &str) -> Option<usize> {
    let commands = Command::parse_many(data);
    let deck: SimpleDeck<Card> = SimpleDeck::new(10007, |idx| idx);
    let result = deck.apply_many(&commands);
    result.find(2019)
}

fn solve_2(data: &str) -> Card {
    let commands = Command::parse_many(data);
    let deck = PowerDeck::new(119_315_717_514_047);
    let times: usize = 101_741_582_076_661;

    let single = deck.apply_many(&commands);
    let resulting = single.many_times(times);

    resulting.at(2020)
}

fn main() {
    let result = solve_1(DATA);
    println!("Part 1: {result:?}");

    let result = solve_2(DATA);
    println!("Part 2: {result:?}");
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_helper(data: &str, expected_str: &str) {
        let commands = Command::parse_many(data);
        let expected: Vec<Card> = expected_str
            .split(' ')
            .map(|x| x.parse().unwrap())
            .collect();

        let deck = SimpleDeck::new(expected.len(), |idx| idx);
        let obtained = deck.apply_many(&commands);

        assert_eq!(obtained.cards, expected);
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
        test_helper("cut 1\ndeal with increment 3", "1 8 5 2 9 6 3 0 7 4");
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
        assert_eq!(solve_2(DATA), 91_967_327_971_097);
    }
}
