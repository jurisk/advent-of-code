use crate::Amphipod::{A, B, C, D};
use crate::HallwaySquare::{LL, LR, ML, MM, MR, RL, RR};
use advent_of_code::parsing::Error;
use itertools::Itertools;
use num_enum::{IntoPrimitive, TryFromPrimitive};
use pathfinding::prelude::dijkstra;
use std::fmt::{Display, Formatter};
use std::str::FromStr;

type Cost = u32;
type Steps = u8;

#[repr(u8)]
#[derive(Copy, Clone, Eq, PartialEq, Hash, IntoPrimitive, TryFromPrimitive, Debug)]
enum Amphipod {
    A = b'A',
    B = b'B',
    C = b'C',
    D = b'D',
}

impl Display for Amphipod {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_char())
    }
}

impl Amphipod {
    fn all() -> [Amphipod; 4] {
        [A, B, C, D]
    }

    fn one_move_cost(self) -> Cost {
        match self {
            A => 1,
            B => 10,
            C => 100,
            D => 1000,
        }
    }

    fn many_moves_cost(self, moves: Steps) -> Cost {
        self.one_move_cost() * u32::from(moves)
    }

    fn as_char(self) -> char {
        let num: u8 = self.into();
        num as char
    }

    fn parse_as_option(ch: char) -> Result<Option<Amphipod>, Error> {
        match ch {
            '.' => Ok(None),
            _ => (ch as u8)
                .try_into()
                .map(Some)
                .map_err(|err| format!("{err:?} when parsing {ch}")),
        }
    }

    fn option_as_char(option: Option<Amphipod>) -> char {
        match option {
            None => '·',
            Some(a) => a.as_char(),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
struct Room<const N: usize> {
    expected_amphipod: Amphipod,
    amphipods: [Option<Amphipod>; N],
}

impl<const N: usize> Room<N> {
    fn from_vec(expected_amphipod: Amphipod, vec: Vec<Option<Amphipod>>) -> Result<Self, Error> {
        let amphipods: [Option<Amphipod>; N] = vec.try_into().map_err(|err| format!("{err:?}"))?;

        Ok(Room {
            expected_amphipod,
            amphipods,
        })
    }

    fn full_of(expected_amphipod: Amphipod) -> Self {
        let amphipods = [Some(expected_amphipod); N];

        Room {
            expected_amphipod,
            amphipods,
        }
    }

    fn free_of_strangers(&self) -> bool {
        self.amphipods.iter().all(|x| match x {
            None => true,
            Some(q) => *q == self.expected_amphipod,
        })
    }

    fn first_occupied(&self) -> usize {
        self.amphipods.iter().position(Option::is_some).unwrap_or(N)
    }

    fn first_free(&self) -> usize {
        self.first_occupied() - 1
    }

    fn push(&self) -> Room<N> {
        let idx = self.first_free();
        let mut amphipods = self.amphipods;
        amphipods[idx] = Some(self.expected_amphipod);
        Room {
            expected_amphipod: self.expected_amphipod,
            amphipods,
        }
    }

    fn has_space(&self) -> Option<Steps> {
        // Makes some assumptions about data consistency, but it should be OK
        if self.amphipods.iter().all(Option::is_some) {
            None
        } else {
            Some(self.first_occupied() as Steps - 1)
        }
    }

    fn has_amphiphod_who_can_leave(&self) -> Option<(Amphipod, Steps)> {
        let idx = self.first_occupied();
        let found = self.amphipods.get(idx)?;
        let amphipod: Amphipod = (*found)?;
        Some((amphipod, idx as Steps))
    }

    fn pop(&self) -> Room<N> {
        let idx = self.first_occupied();
        let mut amphipods = self.amphipods;
        amphipods[idx] = None;
        Room {
            expected_amphipod: self.expected_amphipod,
            amphipods,
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
enum HallwaySquare {
    LL,
    LR,
    ML,
    MM,
    MR,
    RL,
    RR,
}

impl HallwaySquare {
    fn all() -> [HallwaySquare; 7] {
        [LL, LR, ML, MM, MR, RL, RR]
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
struct State<const N: usize> {
    ll: Option<Amphipod>,
    lr: Option<Amphipod>,
    ml: Option<Amphipod>,
    mm: Option<Amphipod>,
    mr: Option<Amphipod>,
    rl: Option<Amphipod>,
    rr: Option<Amphipod>,
    room_for_a: Room<N>,
    room_for_b: Room<N>,
    room_for_c: Room<N>,
    room_for_d: Room<N>,
}

impl<const N: usize> State<N> {
    fn finish() -> State<N> {
        State {
            ll: None,
            lr: None,
            ml: None,
            mm: None,
            mr: None,
            rl: None,
            rr: None,
            room_for_a: Room::full_of(A),
            room_for_b: Room::full_of(B),
            room_for_c: Room::full_of(C),
            room_for_d: Room::full_of(D),
        }
    }

    fn room_for_amphipod(&self, amphipod: Amphipod) -> &Room<N> {
        match amphipod {
            A => &self.room_for_a,
            B => &self.room_for_b,
            C => &self.room_for_c,
            D => &self.room_for_d,
        }
    }

    fn hallway_square(&self, square: HallwaySquare) -> Option<Amphipod> {
        match square {
            LL => self.ll,
            LR => self.lr,
            ML => self.ml,
            MM => self.mm,
            MR => self.mr,
            RL => self.rl,
            RR => self.rr,
        }
    }

    fn update_hallway_square(&self, square: HallwaySquare, value: Option<Amphipod>) -> State<N> {
        match square {
            LL => State { ll: value, ..*self },
            LR => State { lr: value, ..*self },
            ML => State { ml: value, ..*self },
            MM => State { mm: value, ..*self },
            MR => State { mr: value, ..*self },
            RL => State { rl: value, ..*self },
            RR => State { rr: value, ..*self },
        }
    }

    fn update_room(&self, new_room: &Room<N>) -> State<N> {
        match new_room.expected_amphipod {
            A => State {
                room_for_a: *new_room,
                ..*self
            },
            B => State {
                room_for_b: *new_room,
                ..*self
            },
            C => State {
                room_for_c: *new_room,
                ..*self
            },
            D => State {
                room_for_d: *new_room,
                ..*self
            },
        }
    }

    fn clear_hallway_square(&self, square: HallwaySquare) -> State<N> {
        self.update_hallway_square(square, None)
    }

    fn move_from_hallway_to_room(
        &self,
        hallway_square: HallwaySquare,
        amphipod: Amphipod,
    ) -> State<N> {
        let room = self.room_for_amphipod(amphipod);
        let new_room = room.push();
        self.clear_hallway_square(hallway_square)
            .update_room(&new_room)
    }

    // LL--LR----ML----MM----MR----RL--RR
    //       \  / \   / \   /  \  /
    //        A0    B0    C0    D0
    //        A1    B1    C1    D1
    #[allow(clippy::match_same_arms)]
    fn path_from_hallway_square_to_room(
        hallway_square: HallwaySquare,
        room: Amphipod,
    ) -> (Vec<HallwaySquare>, Steps) {
        match hallway_square {
            LL => match room {
                A => (vec![LR], 3),
                B => (vec![LR, ML], 5),
                C => (vec![LR, ML, MM], 7),
                D => (vec![LR, ML, MM, MR], 9),
            },
            LR => {
                let (path, steps) = Self::path_from_hallway_square_to_room(LL, room);
                (path[1..].to_vec(), steps - 1)
            },
            ML => match room {
                A => (vec![], 2),
                B => (vec![], 2),
                C => (vec![MM], 4),
                D => (vec![MM, MR], 6),
            },
            MM => match room {
                A => (vec![ML], 4),
                B => (vec![], 2),
                C => (vec![], 2),
                D => (vec![MR], 4),
            },
            MR => match room {
                A => (vec![MM, ML], 6),
                B => (vec![MM], 4),
                C => (vec![], 2),
                D => (vec![], 2),
            },
            RL => {
                let (path, steps) = Self::path_from_hallway_square_to_room(RR, room);
                (path[1..].to_vec(), steps - 1)
            },
            RR => match room {
                A => (vec![RL, MR, MM, ML], 9),
                B => (vec![RL, MR, MM], 7),
                C => (vec![RL, MR], 5),
                D => (vec![RL], 3),
            },
        }
    }

    fn is_path_free(&self, path: &[HallwaySquare]) -> bool {
        path.iter().all(|x| self.hallway_square(*x).is_none())
    }

    fn valid_paths_from_hallway_square(
        &self,
        hallway_square: HallwaySquare,
        amphipod: Amphipod,
    ) -> Option<(State<N>, Cost)> {
        let (path_to_my_room, steps) =
            Self::path_from_hallway_square_to_room(hallway_square, amphipod);

        let room = self.room_for_amphipod(amphipod);
        let is_path_free = self.is_path_free(&path_to_my_room);

        let is_room_free_of_strangers = room.free_of_strangers();
        let extra_cost_to_go_in_room = room.has_space();

        if is_path_free && is_room_free_of_strangers {
            extra_cost_to_go_in_room.map(|extra_cost| {
                (
                    self.move_from_hallway_to_room(hallway_square, amphipod),
                    amphipod.many_moves_cost(extra_cost + steps),
                )
            })
        } else {
            None
        }
    }

    fn hallway_helper(&self, hallway_square: HallwaySquare) -> Option<(State<N>, Cost)> {
        self.hallway_square(hallway_square)
            .and_then(|x| self.valid_paths_from_hallway_square(hallway_square, x))
    }

    fn room_helper(&self, target_room: Amphipod) -> Vec<(State<N>, Cost)> {
        let room = self.room_for_amphipod(target_room);
        if let Some((amphopod, extra_steps)) = room.has_amphiphod_who_can_leave() {
            HallwaySquare::all()
                .iter()
                .filter_map(|hallway_square| {
                    let (path_to_location, steps) =
                        Self::path_from_hallway_square_to_room(*hallway_square, target_room);
                    if self.is_path_free(&path_to_location) {
                        let new_room = room.pop();
                        Some((
                            self.update_hallway_square(*hallway_square, Some(amphopod))
                                .update_room(&new_room),
                            amphopod.many_moves_cost(steps + extra_steps),
                        ))
                    } else {
                        None
                    }
                })
                .collect()
        } else {
            vec![]
        }
    }

    fn successors(&self) -> Vec<(State<N>, Cost)> {
        // println!("{self}");

        let paths_from_hallway_squares: Vec<(State<N>, Cost)> = HallwaySquare::all()
            .iter()
            .map(|hallway_square| self.hallway_helper(*hallway_square))
            .into_iter()
            .flatten()
            .collect();

        let paths_from_rooms: Vec<(State<N>, Cost)> = Amphipod::all()
            .iter()
            .map(|room| self.room_helper(*room))
            .into_iter()
            .flatten()
            .collect();

        vec![paths_from_hallway_squares, paths_from_rooms].concat()
    }
}

impl<const N: usize> Display for State<N> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "░░░░░░░░░░░░░")?;
        writeln!(
            f,
            "░{}{} {} {} {} {}{}░",
            Amphipod::option_as_char(self.ll),
            Amphipod::option_as_char(self.lr),
            Amphipod::option_as_char(self.ml),
            Amphipod::option_as_char(self.mm),
            Amphipod::option_as_char(self.mr),
            Amphipod::option_as_char(self.rl),
            Amphipod::option_as_char(self.rr)
        )?;

        for idx in 0..N {
            writeln!(
                f,
                "░░░{}░{}░{}░{}░░░",
                Amphipod::option_as_char(self.room_for_a.amphipods[idx]),
                Amphipod::option_as_char(self.room_for_b.amphipods[idx]),
                Amphipod::option_as_char(self.room_for_c.amphipods[idx]),
                Amphipod::option_as_char(self.room_for_d.amphipods[idx])
            )?;
        }
        writeln!(f, "  ░░░░░░░░░  \n\n")
    }
}

fn create_room_helper<const N: usize>(
    lines: &[Vec<char>],
    expected_amphipod: Amphipod,
    idx: usize,
) -> Result<Room<N>, Error> {
    let vec: Result<Vec<Option<Amphipod>>, Error> = lines
        .iter()
        .map(|q| Amphipod::parse_as_option(q[idx]))
        .collect();
    Room::from_vec(expected_amphipod, vec?)
}

impl<const N: usize> FromStr for State<N> {
    type Err = Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let lines: Vec<&str> = input.split('\n').collect();
        let hallway: Vec<char> = lines[1].chars().collect();
        let room_lines: Vec<Vec<char>> = lines[2..(2 + N)]
            .iter()
            .map(|q| q.chars().collect())
            .collect();
        assert_eq!(room_lines.len(), N);

        Ok(State {
            ll: Amphipod::parse_as_option(hallway[1])?,
            lr: Amphipod::parse_as_option(hallway[2])?,
            ml: Amphipod::parse_as_option(hallway[4])?,
            mm: Amphipod::parse_as_option(hallway[6])?,
            mr: Amphipod::parse_as_option(hallway[8])?,
            rl: Amphipod::parse_as_option(hallway[10])?,
            rr: Amphipod::parse_as_option(hallway[11])?,
            room_for_a: create_room_helper(&room_lines, A, 3)?,
            room_for_b: create_room_helper(&room_lines, B, 5)?,
            room_for_c: create_room_helper(&room_lines, C, 7)?,
            room_for_d: create_room_helper(&room_lines, D, 9)?,
        })
    }
}

fn solve<const N: usize>(start: &State<N>, finish: &State<N>) -> Result<Cost, Error> {
    let result: Option<(Vec<State<N>>, Cost)> =
        dijkstra(start, State::successors, |state| state == finish);

    let (path, cost) = result.ok_or_else(|| "Failed to find".to_string())?;

    for state in path {
        println!("{state}");
    }

    Ok(cost)
}

fn solve_1(input: &str) -> Result<Cost, Error> {
    let start: State<2> = input.parse()?;
    let finish: State<2> = State::<2>::finish();

    solve(&start, &finish)
}

fn solve_2(input: &str) -> Result<Cost, Error> {
    let lines: Vec<&str> = input.split('\n').collect();
    let temp = [
        &lines[0..3],
        ["  #D#C#B#A#", "  #D#B#A#C#"].as_slice(),
        &lines[3..],
    ]
    .to_vec();
    let adjusted = temp.into_iter().flatten().join("\n");

    let start: State<4> = adjusted.parse()?;
    let finish: State<4> = State::<4>::finish();

    solve(&start, &finish)
}

const DATA: &str = include_str!("../../resources/23.txt");

fn main() {
    let result_1 = solve_1(DATA);
    println!("Part 1: {result_1:?}");

    let result_2 = solve_2(DATA);
    println!("Part 2: {result_2:?}");
}

#[cfg(test)]
mod tests {
    use super::*;
    const TEST: &str = include_str!("../../resources/23-test.txt");

    fn assert_cost_from_to<const N: usize>(
        from: &State<N>,
        to: &State<N>,
        expected_cost: Cost,
    ) -> Result<(), Error> {
        let successors = from.successors();
        let results = successors.iter().find(|(s, _)| s == to);
        if results.is_none() {
            println!("Did not find:\n{from}\n => \n{to}\n");
        }
        let (_, found_cost) = results.ok_or(format!("Did not find:\n{from}\n => \n{to}\n"))?;
        assert_eq!(*found_cost, expected_cost,);
        Ok(())
    }

    const S1: &str = "#############
#...B.......#
###B#C#.#D###
  #A#D#C#A#
  #########";

    const S2: &str = "#############
#...B.......#
###B#.#C#D###
  #A#D#C#A#
  #########";

    const S3: &str = "#############
#.....D.....#
###B#.#C#D###
  #A#B#C#A#
  #########";

    const S4: &str = "#############
#.....D.....#
###.#B#C#D###
  #A#B#C#A#
  #########";

    const S5: &str = "#############
#.....D.D.A.#
###.#B#C#.###
  #A#B#C#.#
  #########";

    const S6: &str = "#############
#.........A.#
###.#B#C#D###
  #A#B#C#D#
  #########";

    #[test]
    fn test_costs_1() -> Result<(), Error> {
        let s0: State<2> = TEST.parse()?;
        let s1: State<2> = S1.parse()?;
        assert_cost_from_to(&s0, &s1, 40)
    }

    #[test]
    fn test_costs_2() -> Result<(), Error> {
        let s1: State<2> = S1.parse()?;
        let intermed: State<2> = "#############
#...B.C.....#
###B#.#.#D###
  #A#D#C#A#
  #########"
            .parse()?;

        let s2: State<2> = S2.parse()?;

        assert_cost_from_to(&s1, &intermed, 200)?;
        assert_cost_from_to(&intermed, &s2, 200)
    }

    #[test]
    fn test_costs_3() -> Result<(), Error> {
        let s2: State<2> = S2.parse()?;
        let intermed: State<2> = "#############
#...B.D.....#
###B#.#C#D###
  #A#.#C#A#
  #########"
            .parse()?;

        let s3: State<2> = S3.parse()?;
        assert_cost_from_to(&s2, &intermed, 3000)?;
        assert_cost_from_to(&intermed, &s3, 30)
    }

    #[test]
    fn test_costs_4() -> Result<(), Error> {
        let s3: State<2> = S3.parse()?;
        let intermed: State<2> = "#############
#...B.D.....#
###.#.#C#D###
  #A#B#C#A#
  #########"
            .parse()?;

        let s4: State<2> = S4.parse()?;
        assert_cost_from_to(&s3, &intermed, 20)?;
        assert_cost_from_to(&intermed, &s4, 20)
    }

    #[test]
    fn test_costs_5() -> Result<(), Error> {
        let s4: State<2> = S4.parse()?;
        let intermed: State<2> = "#############
#.....D.D...#
###.#B#C#.###
  #A#B#C#A#
  #########"
            .parse()?;
        let s5: State<2> = S5.parse()?;
        assert_cost_from_to(&s4, &intermed, 2000)?;
        assert_cost_from_to(&intermed, &s5, 3)
    }

    #[test]
    fn test_costs_6() -> Result<(), Error> {
        let s5: State<2> = S5.parse()?;
        let intermed: State<2> = "#############
#.....D...A.#
###.#B#C#.###
  #A#B#C#D#
  #########"
            .parse()?;
        let s6: State<2> = S6.parse()?;
        assert_cost_from_to(&s5, &intermed, 3000)?;
        assert_cost_from_to(&intermed, &s6, 4000)
    }

    #[test]
    fn test_costs_7() -> Result<(), Error> {
        let s6: State<2> = S6.parse()?;
        assert_cost_from_to(&s6, &State::finish(), 8)
    }

    #[test]
    fn test_solve_1() {
        let result = solve_1(TEST);
        assert_eq!(result, Ok(12521));
    }

    #[test]
    fn real_solve_1() {
        let result = solve_1(DATA);
        assert_eq!(result, Ok(19167));
    }

    #[test]
    #[ignore]
    fn test_solve_2() {
        let result = solve_2(TEST);
        assert_eq!(result, Ok(44169));
    }

    #[test]
    #[ignore]
    fn real_solve_2() {
        let result = solve_2(DATA);
        assert_eq!(result, Ok(47665));
    }
}
