use advent_of_code::intcode::{parse_machine_code, MachineCode, Process};
use rand::prelude::SliceRandom;
use std::collections::HashSet;
use std::ops::Add;

#[derive(Debug)]
struct Output {
    items: Vec<String>,
    doors_to: Vec<Direction>,
    ejected_back: bool,
    password: Option<String>,
}

impl Output {
    fn itemize(rows: &[&str], header: &str) -> Vec<String> {
        const ITEM_PREFIX: &str = "- ";
        let idx = rows.iter().position(|x| x == &header);
        match idx {
            None => vec![],
            Some(idx) => rows
                .iter()
                .skip(idx + 1)
                .take_while(|x| x.starts_with(ITEM_PREFIX))
                .map(std::string::ToString::to_string)
                .map(|x| x[ITEM_PREFIX.len()..].to_string())
                .collect(),
        }
    }

    fn parse(s: &str) -> Output {
        let ejected_back = s.contains("you are ejected back to the checkpoint");
        let rows: Vec<&str> = s.split('\n').collect();

        if s.contains("Analysis complete! You may proceed.") {
            let get_in = "get in by typing ";
            let idx = s.find(get_in).unwrap();
            let chars: String = s[idx + get_in.len()..]
                .chars()
                .take_while(char::is_ascii_digit)
                .collect();
            return Output {
                items: vec![],
                doors_to: vec![],
                ejected_back: false,
                password: Some(chars),
            };
        }

        let doors_to_strings = Output::itemize(&rows, "Doors here lead:");
        let doors_to: Vec<Direction> = doors_to_strings
            .iter()
            .filter_map(|x| Direction::new(x))
            .collect();

        let items = Output::itemize(&rows, "Items here:");

        Output {
            items,
            doors_to,
            ejected_back,
            password: None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Direction {
    N,
    E,
    S,
    W,
}

impl Direction {
    fn new(s: &str) -> Option<Direction> {
        match s {
            "north" => Some(Direction::N),
            "east" => Some(Direction::E),
            "south" => Some(Direction::S),
            "west" => Some(Direction::W),
            _ => None,
        }
    }

    fn command(self) -> String {
        (match self {
            Direction::N => "north",
            Direction::E => "east",
            Direction::S => "south",
            Direction::W => "west",
        })
        .to_string()
        .add("\n")
    }
}

fn machine_code() -> MachineCode {
    parse_machine_code(include_str!("../../resources/25.txt"))
}

const FORBIDDEN: [&str; 5] = [
    "escape pod",
    "photons",
    "infinite loop",
    "molten lava",
    "giant electromagnet",
];

fn attempt() -> Option<String> {
    let mut process = Process::new(&machine_code());
    let mut rng = rand::thread_rng();
    let mut taken_items: HashSet<String> = HashSet::new();
    let mut ignored_items: HashSet<String> = HashSet::new();

    loop {
        process.run_to_unsatisfied_input();
        let output = process.read_output_as_ascii();

        let parsed = Output::parse(&output);

        if parsed.password.is_some() {
            return parsed.password;
        }

        if parsed.ejected_back {
            println!("{:?}", taken_items);
            return None;
        }

        for item in parsed.items {
            if !FORBIDDEN.contains(&&*item)
                && !taken_items.contains(&*item)
                && !ignored_items.contains(&*item)
            {
                let take: bool = rand::random();
                if take {
                    taken_items.insert(item.clone());
                    process.provide_input_as_string(&format!("take {}\n", item));
                } else {
                    ignored_items.insert(item);
                }
            }
        }

        let chosen_direction = *parsed.doors_to.choose(&mut rng).unwrap();
        process.provide_input_as_string(&chosen_direction.command());
    }
}

fn main() {
    loop {
        let result = attempt();
        if let Some(password) = result {
            println!("Password: {}", password);
            assert_eq!(password, "134227456");
            return;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        main();
    }
}
