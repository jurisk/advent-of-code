use crate::Command::{East, Forward, Left, North, Right, South, West};

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum Command {
    North(isize),
    South(isize),
    East(isize),
    West(isize),
    Left(isize),
    Right(isize),
    Forward(isize),
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
struct Location {
    x:       isize,
    y:       isize,
    bearing: isize,
}

fn clamp(n: isize) -> isize {
    (n + 360) % 360
}

impl Location {
    fn with_x(self, new_x: isize) -> Location {
        Location { x: new_x, ..self }
    }

    fn with_y(self, new_y: isize) -> Location {
        Location { y: new_y, ..self }
    }

    fn with_bearing(self, new_bearing: isize) -> Location {
        Location {
            bearing: new_bearing,
            ..self
        }
    }

    #[allow(clippy::match_same_arms)]
    fn apply(self, command: Command) -> Location {
        fn delta_x(n: isize, bearing: isize) -> isize {
            match bearing {
                0 => 0,
                90 => n,
                180 => 0,
                270 => -n,
                _ => panic!("Didn't expect {bearing}"),
            }
        }

        fn delta_y(n: isize, bearing: isize) -> isize {
            match bearing {
                0 => n,
                90 => 0,
                180 => -n,
                270 => 0,
                _ => panic!("Didn't expect {bearing}"),
            }
        }

        match command {
            North(n) => self.with_y(self.y + n),
            South(n) => self.with_y(self.y - n),
            East(n) => self.with_x(self.x + n),
            West(n) => self.with_x(self.x - n),
            Left(n) => self.with_bearing(clamp(self.bearing - n)),
            Right(n) => self.with_bearing(clamp(self.bearing + n)),
            Forward(n) => {
                Location {
                    x: self.x + delta_x(n, self.bearing),
                    y: self.y + delta_y(n, self.bearing),
                    ..self
                }
            },
        }
    }

    fn manhattan_distance(self) -> isize {
        self.x.abs() + self.y.abs()
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
struct LocationWithWaypoint {
    ship_x:     isize,
    ship_y:     isize,
    waypoint_x: isize,
    waypoint_y: isize,
}

impl LocationWithWaypoint {
    fn with_waypoint_x(self, new_x: isize) -> LocationWithWaypoint {
        LocationWithWaypoint {
            waypoint_x: new_x,
            ..self
        }
    }

    fn with_waypoint_y(self, new_y: isize) -> LocationWithWaypoint {
        LocationWithWaypoint {
            waypoint_y: new_y,
            ..self
        }
    }

    fn rotate_waypoint(self, degrees: isize) -> LocationWithWaypoint {
        if degrees == 90 {
            LocationWithWaypoint {
                waypoint_x: self.waypoint_y,
                waypoint_y: -self.waypoint_x,
                ..self
            }
        } else {
            self.rotate_waypoint(90).rotate_waypoint(degrees - 90)
        }
    }

    fn apply(self, command: Command) -> LocationWithWaypoint {
        match command {
            North(n) => self.with_waypoint_y(self.waypoint_y + n),
            South(n) => self.with_waypoint_y(self.waypoint_y - n),
            East(n) => self.with_waypoint_x(self.waypoint_x + n),
            West(n) => self.with_waypoint_x(self.waypoint_x - n),
            Left(n) => self.rotate_waypoint(clamp(-n)),
            Right(n) => self.rotate_waypoint(clamp(n)),
            Forward(n) => {
                LocationWithWaypoint {
                    ship_x: self.ship_x + n * self.waypoint_x,
                    ship_y: self.ship_y + n * self.waypoint_y,
                    ..self
                }
            },
        }
    }

    fn manhattan_distance(self) -> isize {
        self.ship_x.abs() + self.ship_y.abs()
    }
}

fn parse_line(line: &str) -> Command {
    let first_char: char = line.chars().next().unwrap();
    let remainder: &str = &line[1 ..];
    let number = remainder.parse::<isize>().unwrap();
    match first_char {
        'N' => North(number),
        'S' => South(number),
        'E' => East(number),
        'W' => West(number),
        'L' => Left(number),
        'R' => Right(number),
        'F' => Forward(number),
        _ => panic!("Unrecognized char {first_char}"),
    }
}

fn parse_lines(input: &str) -> Vec<Command> {
    input.lines().map(parse_line).collect::<Vec<_>>()
}

fn solve1(location: Location, commands: &[Command]) -> Location {
    commands
        .iter()
        .fold(location, |acc, command| acc.apply(*command))
}

fn solve2(location: LocationWithWaypoint, commands: &[Command]) -> LocationWithWaypoint {
    commands
        .iter()
        .fold(location, |acc, command| acc.apply(*command))
}

fn main() {
    let starting_location = Location {
        x:       0,
        y:       0,
        bearing: 90,
    };
    let test_data = include_str!("../../resources/12-test.txt");
    let test_commands = parse_lines(test_data);
    let solution1 = solve1(starting_location, &test_commands);
    let result1 = solution1.manhattan_distance();
    assert_eq!(result1, 25);

    let real_data = include_str!("../../resources/12.txt");
    let real_commands = parse_lines(real_data);
    let solution1 = solve1(starting_location, &real_commands);
    println!("{}", solution1.manhattan_distance());

    let starting_location = LocationWithWaypoint {
        ship_x:     0,
        ship_y:     0,
        waypoint_x: 10,
        waypoint_y: 1,
    };
    let solution2 = solve2(starting_location, &test_commands);
    let result2 = solution2.manhattan_distance();
    assert_eq!(result2, 286);

    let solution2 = solve2(starting_location, &real_commands);
    println!("{}", solution2.manhattan_distance());
}
