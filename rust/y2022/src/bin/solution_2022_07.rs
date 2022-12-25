#![feature(map_try_insert)]

use advent_of_code_common::parsing::{normalize_newlines, Error};
use advent_of_code_common::utils::head_tail;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{char, digit1, newline};
use nom::combinator::{complete, map, map_res};
use nom::error::ErrorKind;
use nom::multi::{many0, many1};
use nom::sequence::{pair, preceded, terminated};
use nom::{AsChar, IResult, InputTakeAtPosition};
use std::collections::HashMap;

const DATA: &str = include_str!("../../resources/07.txt");

type Data = Vec<CommandWithOutput>;
type N = u32;

#[derive(Debug)]
struct CommandWithOutput {
    command: Command,
    output: Vec<OutputLine>,
}

#[derive(Debug)]
enum Command {
    CdDir(String),
    CdUp,
    CdRoot,
    Ls,
}

#[derive(Debug)]
enum OutputLine {
    Dir { name: String },
    File { name: String, size: N },
}

#[derive(Debug, Clone)]
struct Directory {
    directories: HashMap<String, Directory>,
    files: HashMap<String, N>,
}

impl Directory {
    fn empty() -> Directory {
        Directory {
            directories: HashMap::new(),
            files: HashMap::new(),
        }
    }

    fn all_directories_including_self(&self) -> Vec<&Directory> {
        let others: Vec<&Directory> = self
            .directories
            .values()
            .flat_map(Directory::all_directories_including_self)
            .collect();

        vec![vec![self], others].concat()
    }

    fn total_size(&self) -> N {
        let sub_dir_size: N = self.directories.values().map(Directory::total_size).sum();
        let file_size: N = self.files.values().sum();
        sub_dir_size + file_size
    }

    fn add_output_lines(
        &self,
        working_directory: &[String],
        output_lines: &[OutputLine],
    ) -> Directory {
        let (h, t) = head_tail(working_directory);

        let mut new_directories = self.directories.clone();
        let mut new_files = self.files.clone();

        match h {
            None => {
                for output_line in output_lines {
                    match output_line {
                        OutputLine::Dir { name } => {
                            let _ = new_directories.try_insert(name.clone(), Directory::empty());
                        },
                        OutputLine::File { name, size } => {
                            new_files.insert(name.clone(), *size);
                        },
                    }
                }
            },

            Some(dir) => {
                let key = dir.to_string();
                let found = new_directories.get(&key).unwrap();
                let result = found.add_output_lines(t, output_lines);
                new_directories.insert(key, result);
            },
        };

        Directory {
            directories: new_directories,
            files: new_files,
        }
    }

    fn apply_command_with_output(
        &self,
        working_directory: Vec<String>,
        command_with_output: &CommandWithOutput,
    ) -> (Directory, Vec<String>) {
        match &command_with_output.command {
            Command::CdDir(dir) => (
                self.clone(),
                vec![working_directory, vec![dir.clone()]].concat(), // append 1 to the end
            ),
            Command::CdUp => (
                self.clone(),
                working_directory[0..working_directory.len() - 1].to_vec(), // .init
            ),
            Command::CdRoot => (self.clone(), Vec::new()),
            Command::Ls => (
                self.add_output_lines(&working_directory, &command_with_output.output),
                working_directory.clone(),
            ),
        }
    }
}

fn process(data: &Data) -> Directory {
    let (result, _) = data.iter().fold(
        (Directory::empty(), Vec::new()),
        |(acc, working_directory), cmd| acc.apply_command_with_output(working_directory, cmd),
    );
    result
}

fn parse(input: &str) -> Result<Data, Error> {
    fn ls(input: &str) -> IResult<&str, Command> {
        map(tag("$ ls"), |_| Command::Ls)(input)
    }

    fn cd_command(input: &str) -> IResult<&str, Command> {
        alt((
            map(tag(".."), |_| Command::CdUp),
            map(tag("/"), |_| Command::CdRoot),
            map(name, |result| Command::CdDir(result.to_string())),
        ))(input)
    }

    fn cd(input: &str) -> IResult<&str, Command> {
        preceded(tag("$ cd "), cd_command)(input)
    }

    fn cmd(input: &str) -> IResult<&str, Command> {
        terminated(alt((ls, cd)), newline)(input)
    }

    fn name(input: &str) -> IResult<&str, &str> {
        input
            .split_at_position1_complete(|item| !item.is_alphanum() && item != '.', ErrorKind::Fail)
    }

    fn dir(input: &str) -> IResult<&str, OutputLine> {
        map(preceded(tag("dir "), name), |s| OutputLine::Dir {
            name: s.to_string(),
        })(input)
    }

    fn parse_u32(input: &str) -> IResult<&str, u32> {
        map_res(digit1, str::parse)(input)
    }

    fn file(input: &str) -> IResult<&str, OutputLine> {
        let (input, size) = parse_u32(input)?;
        let (input, _) = char(' ')(input)?;
        let (input, name) = name(input)?;
        Ok((
            input,
            OutputLine::File {
                name: name.to_string(),
                size,
            },
        ))
    }

    fn output_line(input: &str) -> IResult<&str, OutputLine> {
        terminated(alt((dir, file)), newline)(input)
    }

    fn command_and_n_output(input: &str) -> IResult<&str, CommandWithOutput> {
        map(pair(cmd, many0(output_line)), |(command, output)| {
            CommandWithOutput { command, output }
        })(input)
    }

    fn parser(input: &str) -> IResult<&str, Data> {
        complete(many1(command_and_n_output))(input)
    }

    let mut input_finishing_on_newline = normalize_newlines(input);
    input_finishing_on_newline.push('\n');

    let (unparsed, result) = parser(&input_finishing_on_newline).map_err(|e| format!("{e}"))?;
    assert_eq!(unparsed, "");

    Ok(result)
}

fn solve_1(root: &Directory, limit: N) -> N {
    root.all_directories_including_self()
        .into_iter()
        .map(Directory::total_size)
        .filter(|&x| x <= limit)
        .sum()
}

fn solve_2(root: &Directory, limit: N) -> N {
    let total_size = root.total_size();
    let all = root.all_directories_including_self();
    let options = all.iter().map(|x| x.total_size());
    let best_option = options.filter(|&x| total_size - x <= limit).min();
    best_option.unwrap()
}

fn part_1(input: &str, limit: N) -> Result<N, Error> {
    let data = parse(input)?;
    let processed = process(&data);
    Ok(solve_1(&processed, limit))
}

fn part_2(input: &str, limit: N) -> Result<N, Error> {
    let data = parse(input)?;
    let processed = process(&data);
    Ok(solve_2(&processed, limit))
}

const PART_1_LIMIT: N = 100_000;

const PART_2_LIMIT: N = 70_000_000 - 30_000_000;

fn main() -> Result<(), Error> {
    let result_1 = part_1(DATA, PART_1_LIMIT)?;
    println!("Part 1: {result_1}");

    let result_2 = part_2(DATA, PART_2_LIMIT)?;
    println!("Part 2: {result_2}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = include_str!("../../resources/07-test.txt");

    #[test]
    fn test_solve_1_test() {
        assert_eq!(part_1(TEST_DATA, PART_1_LIMIT), Ok(95437));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA, PART_1_LIMIT), Ok(1_428_881));
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(part_2(TEST_DATA, PART_2_LIMIT), Ok(24_933_642));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA, PART_2_LIMIT), Ok(10_475_598));
    }
}
