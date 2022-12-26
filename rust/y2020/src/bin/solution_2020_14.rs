use crate::Command::{SetMask, SetMemory};
use regex::Regex;
use std::collections::{BTreeMap, VecDeque};
use std::fmt;

#[macro_use]
extern crate lazy_static;

#[derive(Debug)]
enum Command {
    SetMask { mask: Mask },
    SetMemory { address: u64, value: u64 },
}

#[derive(Clone, Copy)]
struct Mask {
    mask_0: u64,
    mask_1: u64,
    mask_x: u64, // could be removed and calculated, but it slowed down the calculation
}

impl fmt::Debug for Mask {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!(
            "mask_0 = {:036b}, mask_1 = {:036b}, mask_x = {:036b}",
            self.mask_0, self.mask_1, self.mask_x
        ))
    }
}

impl Mask {
    fn initial() -> Mask {
        Mask {
            mask_0: 0,
            mask_1: 0,
            mask_x: 0,
        }
    }
}

type ErrorMessage = String;

fn parse_mask(line: &str) -> Result<Command, ErrorMessage> {
    lazy_static! {
        static ref RE: Regex = regex::Regex::new(r"mask = (\w+)").expect("Failed to parse RegEx");
    }
    let elems = &RE.captures(line).ok_or("Failed to match mask")?;

    let bits: &str = &elems[1];

    let mut mask_0: u64 = 0;
    let mut mask_1: u64 = 0;
    let mut mask_x: u64 = 0;

    bits.chars().enumerate().for_each(|(i, ch)| {
        const LEN: usize = 36;
        let shift = (LEN - 1) - i;
        match ch {
            '0' => mask_0 |= 1 << shift,
            '1' => mask_1 |= 1 << shift,
            'X' => mask_x |= 1 << shift,
            _ => unreachable!(),
        }
    });

    Ok(SetMask {
        mask: Mask {
            mask_0,
            mask_1,
            mask_x,
        },
    })
}

fn parse_set_memory(line: &str) -> Result<Command, ErrorMessage> {
    lazy_static! {
        static ref RE: Regex = Regex::new(r"^mem\[(\d+)] = (\d+)$").expect("Failed to parse RegEx");
    }

    let caps = RE.captures(line).ok_or("Invalid command")?;
    let address: u64 = caps[1].parse().map_err(|_| "Invalid address")?;
    let value: u64 = caps[2].parse().map_err(|_| "Invalid value")?;

    Ok(SetMemory { address, value })
}

fn parse_line(input: &str) -> Result<Command, ErrorMessage> {
    parse_mask(input).or_else(|_| parse_set_memory(input))
}

fn parse_lines(input: &str) -> Result<Vec<Command>, ErrorMessage> {
    input.lines().map(parse_line).collect()
}

fn apply_mask(value: u64, mask_0: u64, mask_1: u64) -> u64 {
    (value & !mask_0) | mask_1
}

fn write_to_memory_applying_mask(
    memory: &mut BTreeMap<u64, u64>,
    mask: Mask,
    address: u64,
    value: u64,
) {
    memory.insert(address, apply_mask(value, mask.mask_0, mask.mask_1));
}

fn solve1(commands: &[Command]) -> u64 {
    solve(commands, write_to_memory_applying_mask)
}

fn iterate_bits<F>(mask: u64, value: u64, mut callback: F)
where
    F: FnMut(u64),
{
    struct Entry {
        remaining_mask: u64,
        mask_0: u64,
        mask_1: u64,
    }

    let mut queue: VecDeque<Entry> = VecDeque::new();

    queue.push_front(Entry {
        remaining_mask: mask,
        mask_0: 0,
        mask_1: 0,
    });

    while !queue.is_empty() {
        match queue.pop_back() {
            None => {},
            Some(entry) => {
                let trailing_zeros = entry.remaining_mask.trailing_zeros();
                if trailing_zeros < 64 {
                    let least_significant = 1 << trailing_zeros;
                    let without_least_significant = entry.remaining_mask & !least_significant;
                    queue.push_back(Entry {
                        remaining_mask: without_least_significant,
                        mask_0: entry.mask_0 | least_significant,
                        mask_1: entry.mask_1,
                    });
                    queue.push_back(Entry {
                        remaining_mask: without_least_significant,
                        mask_0: entry.mask_0,
                        mask_1: entry.mask_1 | least_significant,
                    });
                } else {
                    // remaining_mask is empty now
                    let applied = apply_mask(value, entry.mask_0, entry.mask_1);
                    callback(applied);
                }
            },
        }
    }
}

fn write_to_memory_with_floating_bits(
    memory: &mut BTreeMap<u64, u64>,
    mask: Mask,
    address: u64,
    value: u64,
) {
    iterate_bits(mask.mask_x, address, |floated_bits| {
        let floated_address = floated_bits | mask.mask_1;
        memory.insert(floated_address, value);
    });
}

fn solve<F>(commands: &[Command], mut f: F) -> u64
where
    F: FnMut(&mut BTreeMap<u64, u64>, Mask, u64, u64),
{
    let mut memory: BTreeMap<u64, u64> = BTreeMap::new();
    let mut mask: Mask = Mask::initial();

    commands.iter().for_each(|command| match command {
        SetMemory { address, value } => {
            f(&mut memory, mask, *address, *value);
        },
        SetMask { mask: new_mask } => {
            mask = *new_mask;
        },
    });

    memory.values().sum()
}

fn solve2(commands: &[Command]) -> u64 {
    solve(commands, write_to_memory_with_floating_bits)
}

fn main() {
    let test_str_1 = include_str!("../../resources/14-test-1.txt");
    let test_str_2 = include_str!("../../resources/14-test-2.txt");
    let real_str = include_str!("../../resources/14.txt");

    let test_data_1 = parse_lines(test_str_1).unwrap();
    let test_data_2 = parse_lines(test_str_2).unwrap();
    let real_data = parse_lines(real_str).unwrap();

    assert_eq!(solve1(&test_data_1), 165);
    println!("{}", solve1(&real_data));
    assert_eq!(solve1(&real_data), 15_403_588_588_538);

    assert_eq!(solve2(&test_data_2), 208);
    assert_eq!(solve2(&real_data), 3_260_587_250_457);
    println!("{}", solve2(&real_data));
}
