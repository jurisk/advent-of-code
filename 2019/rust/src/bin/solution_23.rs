use advent_of_code::intcode::{parse_machine_code, Entry, MachineCode, Process};
use std::collections::HashMap;
use std::hash::Hash;

const NAT_ADDRESS: Entry = 255;
const COMPUTER_COUNT: ComputerIndex = 50;

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
struct XYPair {
    x: Entry,
    y: Entry,
}

struct MessageBuffers<K: Eq + Hash, V: Clone> {
    map: HashMap<K, Vec<V>>,
}

impl<K: Eq + Hash, V: Clone> MessageBuffers<K, V> {
    fn new() -> MessageBuffers<K, V> {
        MessageBuffers {
            map: HashMap::new(),
        }
    }

    fn push(&mut self, k: K, v: V) {
        let mut messages: Vec<V> = self.map.get(&k).unwrap_or(&vec![]).clone();
        messages.push(v);
        self.map.insert(k, messages);
    }

    fn pop_messages_for(&mut self, k: &K) -> Vec<V> {
        let m: &mut HashMap<K, Vec<V>> = &mut self.map;
        let result = m.get(k).unwrap_or(&vec![]).clone();
        m.remove(k);
        result
    }
}

fn machine_code() -> MachineCode {
    parse_machine_code(include_str!("../../resources/23.txt"))
}

type ComputerIndex = usize;

fn create_computers() -> Vec<Process> {
    (0..COMPUTER_COUNT)
        .map(|i| {
            let mut process = Process::new(&machine_code());
            process.provide_input(i as Entry);
            process
        })
        .collect()
}

fn send_message_buffers_to_computers(
    computers: &mut [Process],
    buffers: &mut MessageBuffers<usize, XYPair>,
) {
    for idx in 0..computers.len() {
        let computer = &mut computers[idx];

        let messages: Vec<XYPair> = buffers.pop_messages_for(&idx);

        if messages.is_empty() {
            computer.provide_input(-1 as Entry);
        } else {
            for message in messages {
                computer.provide_input(message.x);
                computer.provide_input(message.y);
            }
        }
    }
}

fn solve_1() {
    let mut computers: Vec<_> = create_computers();

    let mut buffers: MessageBuffers<ComputerIndex, XYPair> = MessageBuffers::new();

    loop {
        // get all computers hungry for input or halted
        for idx in 0..COMPUTER_COUNT {
            let computer = &mut computers[idx];
            let halted = computer.run_to_unsatisfied_input();
            if halted {
                println!("Computer {idx} has halted");
            }
        }

        // send the message buffers to computers
        send_message_buffers_to_computers(&mut computers, &mut buffers);

        // place messages from computers into message buffers
        for idx in 0..COMPUTER_COUNT {
            let computer = &mut computers[idx];

            if computer.output_len() >= 3 {
                let addr = computer.next_output_unsafe();
                let x = computer.next_output_unsafe();
                let y = computer.next_output_unsafe();
                println!("{addr} {x} {y}");
                let xy_pair = XYPair { x, y };

                if addr == NAT_ADDRESS {
                    println!("Part 1: {y}");
                    assert_eq!(y, 20_372);
                    return;
                }

                buffers.push(addr as ComputerIndex, xy_pair);
            }
        }
    }
}

fn solve_2() {
    let computer_count: ComputerIndex = 50;
    let mut last_packet_received_by_nat: Option<XYPair> = None;
    let mut last_y_sent_to_0: Option<Entry> = None;

    let mut computers: Vec<_> = create_computers();

    let mut buffers: MessageBuffers<ComputerIndex, XYPair> = MessageBuffers::new();

    loop {
        // get all computers hungry for input or halted
        for idx in 0..computer_count {
            let computer = &mut computers[idx];
            let halted = computer.run_to_unsatisfied_input();
            if halted {
                println!("Computer {idx} has halted");
            }
        }

        let idle_computer_count = computers
            .iter()
            .filter(|c| c.unsatisfied_input() && c.output_len() == 0)
            .count();

        if idle_computer_count == computer_count {
            if let Some(p) = last_packet_received_by_nat {
                if Some(p.y) == last_y_sent_to_0 {
                    println!("Part 2: {}", p.y);
                    assert_eq!(p.y, 13_334);
                    return;
                }
                buffers.push(0, p);
                last_y_sent_to_0 = Some(p.y);
            }
        }

        // send the message buffers to computers
        send_message_buffers_to_computers(&mut computers, &mut buffers);

        // place messages from computers into message buffers
        for idx in 0..computer_count {
            let computer = &mut computers[idx];

            if computer.output_len() >= 3 {
                let addr = computer.next_output_unsafe();
                let x = computer.next_output_unsafe();
                let y = computer.next_output_unsafe();
                println!("{addr} {x} {y}");
                buffers.push(addr as ComputerIndex, XYPair { x, y });

                let xy_pair = XYPair { x, y };

                if addr == NAT_ADDRESS {
                    last_packet_received_by_nat = Some(xy_pair);
                } else {
                    buffers.push(addr as ComputerIndex, xy_pair);
                }
            }
        }
    }
}

fn main() {
    solve_1();
    solve_2();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_solve_1() {
        solve_1();
    }

    #[test]
    fn test_solve_2() {
        solve_2();
    }
}
