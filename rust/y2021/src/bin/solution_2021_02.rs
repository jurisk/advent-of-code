enum Command {
    Up(i32),
    Down(i32),
    Forward(i32),
}

struct Location {
    position: i32,
    depth:    i32,
}

impl Location {
    fn zero() -> Location {
        Location {
            position: 0,
            depth:    0,
        }
    }

    fn apply(&self, command: &Command) -> Location {
        match command {
            Command::Up(x) => {
                Location {
                    depth: self.depth - x,
                    ..*self
                }
            },
            Command::Down(x) => {
                Location {
                    depth: self.depth + x,
                    ..*self
                }
            },
            Command::Forward(x) => {
                Location {
                    position: self.position + x,
                    ..*self
                }
            },
        }
    }
}

struct LocationWithAim {
    position: i32,
    depth:    i32,
    aim:      i32,
}

impl LocationWithAim {
    fn zero() -> LocationWithAim {
        LocationWithAim {
            position: 0,
            depth:    0,
            aim:      0,
        }
    }

    fn apply(&self, command: &Command) -> LocationWithAim {
        match command {
            Command::Up(x) => {
                LocationWithAim {
                    aim: self.aim - x,
                    ..*self
                }
            },
            Command::Down(x) => {
                LocationWithAim {
                    aim: self.aim + x,
                    ..*self
                }
            },
            Command::Forward(x) => {
                LocationWithAim {
                    position: self.position + x,
                    depth: self.depth + self.aim * x,
                    ..*self
                }
            },
        }
    }
}

impl Command {
    fn parse(s: &str) -> Command {
        let elements: Vec<_> = s.split(' ').collect();
        assert_eq!(elements.len(), 2);
        let number = elements[1].parse::<i32>().unwrap();

        match elements.first() {
            Some(&"forward") => Command::Forward(number),
            Some(&"up") => Command::Up(number),
            Some(&"down") => Command::Down(number),
            x => panic!("Unrecognized: {x:?}"),
        }
    }
}

fn parse(input: &str) -> Vec<Command> {
    input
        .lines()
        .filter(|x| !x.is_empty())
        .map(Command::parse)
        .collect()
}

fn solve_1(input: &[Command]) -> i32 {
    let result = input.iter().fold(Location::zero(), |acc, x| acc.apply(x));
    result.depth * result.position
}

fn solve_2(input: &[Command]) -> i32 {
    let result = input
        .iter()
        .fold(LocationWithAim::zero(), |acc, x| acc.apply(x));
    result.depth * result.position
}

fn part_1(input: &str) -> i32 {
    let data: Vec<_> = parse(input);
    solve_1(&data)
}

fn part_2(input: &str) -> i32 {
    let data: Vec<_> = parse(input);
    solve_2(&data)
}

#[expect(clippy::unreadable_literal)]
fn main() {
    let test_data = "forward 5
down 5
forward 8
up 3
down 8
forward 2";

    let test_result_1 = part_1(test_data);
    assert_eq!(test_result_1, 150);

    let data = include_str!("../../resources/02.txt");
    let result_1 = part_1(data);

    assert_eq!(result_1, 2036120);
    println!("Part 1: {result_1}");

    let test_result_2 = part_2(test_data);
    assert_eq!(test_result_2, 900);

    let result_2 = part_2(data);

    assert_eq!(result_2, 2015547716);
    println!("Part 2: {result_2}");
}
