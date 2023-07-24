use std::cmp;
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter};
use std::str::FromStr;

use advent_of_code_common::parsing::{
    parse_lines_to_vec, parse_str, split_into_two_strings, Error,
};
use itertools::Itertools;
use memoize::lazy_static::lazy_static;
use regex::Regex;

use crate::Address::{Bot, Output};
use crate::Instruction::{BotNGiveLowToXAndHighToY, ValueXGoesToBotY};

#[derive(Eq, PartialEq, Hash, Copy, Clone, Ord, PartialOrd)]
struct BotId(u8);

impl Debug for BotId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for BotId {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(BotId(parse_str(s)?))
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
struct OutputId(u8);

impl Debug for OutputId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for OutputId {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(OutputId(parse_str(s)?))
    }
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Copy, Clone, Hash)]
struct MicrochipValue(u8);

impl Debug for MicrochipValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "value {}", self.0)
    }
}

impl FromStr for MicrochipValue {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(MicrochipValue(parse_str(s)?))
    }
}

#[derive(Copy, Clone)]
enum Address {
    Bot(BotId),
    Output(OutputId),
}

impl Debug for Address {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Bot(bot_id) => write!(f, "bot {bot_id:?}"),
            Output(output_id) => write!(f, "output {output_id:?}"),
        }
    }
}

impl FromStr for Address {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (a, b) = split_into_two_strings(s, " ")?;
        match a.as_str() {
            "output" => Ok(Output(parse_str(&b)?)),
            "bot" => Ok(Bot(parse_str(&b)?)),
            _ => Err(format!("Unrecognized: {s}")),
        }
    }
}

#[derive(Debug)]
enum Instruction {
    ValueXGoesToBotY {
        value:  MicrochipValue,
        bot_id: BotId,
    },
    BotNGiveLowToXAndHighToY {
        bot_id: BotId,
        low:    Address,
        high:   Address,
    },
}

impl FromStr for Instruction {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        lazy_static! {
            static ref BOT_GIVES_RE: Regex =
                Regex::new(r"bot (\d+) gives low to ([a-z]+ \d+) and high to ([a-z]+ \d+)")
                    .unwrap();
            static ref VALUE_GOES_RE: Regex = Regex::new(r"value (\d+) goes to bot (\d+)").unwrap();
        }

        if let Some(captures) = BOT_GIVES_RE.captures(s) {
            assert_eq!(captures.len(), 4);
            let bot_id = parse_str(&captures[1])?;
            let low = parse_str(&captures[2])?;
            let high = parse_str(&captures[3])?;

            Ok(BotNGiveLowToXAndHighToY { bot_id, low, high })
        } else if let Some(captures) = VALUE_GOES_RE.captures(s) {
            assert_eq!(captures.len(), 3);
            let value = parse_str(&captures[1])?;
            let bot_id = parse_str(&captures[2])?;
            Ok(ValueXGoesToBotY { value, bot_id })
        } else {
            Err(format!("Unrecognized {s}"))
        }
    }
}

fn parse(input: &str) -> Result<Vec<Instruction>, Error> {
    parse_lines_to_vec(input)
}

#[derive(Debug)]
struct BotDefinition {
    low:  Address,
    high: Address,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
enum MailboxState {
    Empty,
    One(MicrochipValue),
    Two {
        low:  MicrochipValue,
        high: MicrochipValue,
    },
}

impl MailboxState {
    fn updated(self, added: MicrochipValue) -> Self {
        match self {
            MailboxState::Empty => MailboxState::One(added),
            MailboxState::One(existing) if existing == added => self,
            MailboxState::One(existing) => {
                MailboxState::Two {
                    low:  cmp::min(added, existing),
                    high: cmp::max(added, existing),
                }
            },
            MailboxState::Two { .. } => panic!("Unexpected adding of {added:?} in state {self:?}"),
        }
    }
}

fn next_to_process(mailboxes: &HashMap<BotId, MailboxState>) -> Option<(BotId, MailboxState)> {
    mailboxes
        .iter()
        .find(|(_bot_id, state)| matches!(state, MailboxState::Two { .. }))
        .map(|(a, b)| (*a, *b))
}

fn add_to_mailbox(
    mailboxes: &mut HashMap<BotId, MailboxState>,
    bot_id: BotId,
    value: MicrochipValue,
) {
    let previous = *mailboxes.get(&bot_id).unwrap_or(&MailboxState::Empty);
    let updated = previous.updated(value);
    mailboxes.insert(bot_id, updated);
}

const DEBUG: bool = false;

fn solve(
    instructions: &Vec<Instruction>,
) -> (
    HashMap<MailboxState, BotId>,
    HashMap<OutputId, MicrochipValue>,
) {
    if DEBUG {
        println!("Instructions:");
        for instruction in instructions {
            println!("{instruction:?}");
        }
        println!();
    }

    let mut routing: HashMap<MailboxState, BotId> = HashMap::new();
    let mut outputs: HashMap<OutputId, MicrochipValue> = HashMap::new();
    let mut definitions: HashMap<BotId, BotDefinition> = HashMap::new();
    let mut mailboxes: HashMap<BotId, MailboxState> = HashMap::new();

    for instruction in instructions {
        match instruction {
            ValueXGoesToBotY { value, bot_id } => add_to_mailbox(&mut mailboxes, *bot_id, *value),
            BotNGiveLowToXAndHighToY { bot_id, low, high } => {
                definitions.insert(
                    *bot_id,
                    BotDefinition {
                        low:  *low,
                        high: *high,
                    },
                );
            },
        }
    }

    if DEBUG {
        println!("Definitions:");
        for bot_id in definitions.keys().sorted() {
            let definition = definitions.get(bot_id).unwrap();
            println!("{bot_id:?}: {definition:?}");
        }
        println!();
    }

    loop {
        if DEBUG {
            println!("Mailboxes:");
            for bot_id in mailboxes.keys().sorted() {
                let mailbox = mailboxes.get(bot_id).unwrap();
                println!("Bot {bot_id:?} has items: {mailbox:?}");
            }
            println!();
        }

        if let Some((bot_id, state)) = next_to_process(&mailboxes) {
            if DEBUG {
                println!("Next to process {bot_id:?} {state:?}...");
            }

            if let MailboxState::Two { low, high } = state {
                routing.insert(state, bot_id);

                let definition = definitions.get(&bot_id).unwrap();
                match definition.low {
                    Bot(target) => add_to_mailbox(&mut mailboxes, target, low),
                    Output(target) => _ = outputs.insert(target, low),
                };

                match definition.high {
                    Bot(target) => add_to_mailbox(&mut mailboxes, target, high),
                    Output(target) => _ = outputs.insert(target, high),
                };

                mailboxes.insert(bot_id, MailboxState::Empty);
            }
        } else {
            break;
        }
    }

    (routing, outputs)
}

fn solve_1(
    instructions: &Vec<Instruction>,
    target_low: MicrochipValue,
    target_high: MicrochipValue,
) -> Option<BotId> {
    let (routing, _outputs) = solve(instructions);
    routing
        .get(&MailboxState::Two {
            low:  target_low,
            high: target_high,
        })
        .copied()
}

fn solve_2(instructions: &Vec<Instruction>, target_outputs: &HashSet<OutputId>) -> u32 {
    let (_routing, outputs) = solve(instructions);
    target_outputs
        .iter()
        .map(|target| outputs.get(target).map_or(0, |x| u32::from(x.0)))
        .product()
}

fn part_1(input: &str, low: MicrochipValue, high: MicrochipValue) -> Result<Option<BotId>, Error> {
    parse(input).map(|input| solve_1(&input, low, high))
}

fn part_2(input: &str, target_outputs: &HashSet<OutputId>) -> Result<u32, Error> {
    parse(input).map(|input| solve_2(&input, target_outputs))
}

const DATA: &str = include_str!("../../resources/10.txt");

fn outputs_of_interest() -> HashSet<OutputId> {
    vec![0, 1, 2].into_iter().map(OutputId).collect()
}

fn main() -> Result<(), Error> {
    let result_1 = part_1(DATA, MicrochipValue(17), MicrochipValue(61))?;
    println!("Part 1: {result_1:?}");

    let result_2 = part_2(DATA, &outputs_of_interest())?;
    println!("Part 2: {result_2:?}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST: &str = include_str!("../../resources/10-test.txt");

    #[test]
    fn test_solve_1_test() {
        assert_eq!(
            part_1(TEST, MicrochipValue(2), MicrochipValue(5)),
            Ok(Some(BotId(2)))
        );
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(
            part_1(DATA, MicrochipValue(17), MicrochipValue(61)),
            Ok(Some(BotId(101)))
        );
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA, &outputs_of_interest()), Ok(31 * 23 * 53));
    }
}
