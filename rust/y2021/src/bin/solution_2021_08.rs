use advent_of_code_2021::parsing::{
    parse_lines_to_vec, parse_space_separated_vec, split_into_two_strings, Error,
};
use itertools::Itertools;
use std::collections::HashMap;
use std::hash::Hash;
use std::str::FromStr;

const DATA: &str = include_str!("../../resources/08.txt");

type Segment = char;

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
struct SegmentSet {
    segments: Vec<Segment>,
}

impl SegmentSet {
    fn len(&self) -> u8 {
        self.segments.len() as u8
    }

    fn overlap(&self, other: &SegmentSet) -> u8 {
        self.segments
            .iter()
            .filter(|ch| other.segments.contains(ch))
            .count() as u8
    }
}

impl FromStr for SegmentSet {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(SegmentSet {
            segments: s.chars().sorted().collect(),
        })
    }
}

#[derive(Debug)]
struct Entry {
    ten_patterns: [SegmentSet; 10],
    output_value_segments: [SegmentSet; 4],
}

impl Entry {
    fn count_1_4_7_8(&self) -> usize {
        println!("{self:?}");
        self.output_value_digits()
            .iter()
            .filter(|&n| matches!(n, 1 | 4 | 7 | 8))
            .count()
    }

    fn find_pattern<F>(&self, segment_count: u8, extra_condition: F) -> SegmentSet
    where
        F: Fn(&SegmentSet) -> bool,
    {
        let results: Vec<_> = self
            .ten_patterns
            .iter()
            .filter(|p| p.len() == segment_count)
            .filter(|p| extra_condition(p))
            .collect();
        assert_eq!(results.len(), 1);
        let result = results[0];
        result.clone()
    }

    fn mapping(&self) -> HashMap<SegmentSet, u8> {
        let digit_1 = self.find_pattern(2, |_| true);
        let digit_4 = self.find_pattern(4, |_| true);
        let digit_7 = self.find_pattern(3, |_| true);
        let digit_8 = self.find_pattern(7, |_| true);
        let digit_3 = self.find_pattern(5, |s| s.overlap(&digit_1) == digit_1.len());
        let digit_5 = self.find_pattern(5, |s| s.overlap(&digit_4) == 3 && *s != digit_3);
        let digit_2 = self.find_pattern(5, |s| s.overlap(&digit_5) == 3);
        let digit_6 = self.find_pattern(6, |s| s.overlap(&digit_1) == 1);
        let digit_9 = self.find_pattern(6, |s| s.overlap(&digit_4) == digit_4.len());
        let digit_0 =
            self.find_pattern(6, |s| s.overlap(&digit_1) == digit_1.len() && *s != digit_9);

        HashMap::from([
            (digit_0, 0),
            (digit_1, 1),
            (digit_2, 2),
            (digit_3, 3),
            (digit_4, 4),
            (digit_5, 5),
            (digit_6, 6),
            (digit_7, 7),
            (digit_8, 8),
            (digit_9, 9),
        ])
    }

    fn output_digit(&self, segment: &SegmentSet) -> u8 {
        *self.mapping().get(segment).unwrap_or_else(|| {
            panic!(
                "Failed to find {:?} in mapping {:?}",
                segment,
                self.mapping()
            )
        })
    }

    fn output_value_digits(&self) -> [u8; 4] {
        let result: Vec<_> = self
            .output_value_segments
            .iter()
            .map(|x| self.output_digit(x))
            .collect();
        result.try_into().unwrap()
    }

    fn output_value(&self) -> usize {
        let digits: [u8; 4] = self.output_value_digits();
        (digits[0] as usize) * 1000
            + (digits[1] as usize) * 100
            + (digits[2] as usize) * 10
            + (digits[3] as usize)
    }
}

impl FromStr for Entry {
    type Err = String;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let tuple: (String, String) = split_into_two_strings(input, " | ")?;
        let ten_patterns_vec: Vec<SegmentSet> = parse_space_separated_vec(&tuple.0)?;
        let output_value_vec: Vec<SegmentSet> = parse_space_separated_vec(&tuple.1)?;

        let ten_patterns = ten_patterns_vec
            .try_into()
            .map_err(|err| format!("{err:?}"))?;
        let output_value_segments = output_value_vec
            .try_into()
            .map_err(|err| format!("{err:?}"))?;

        Ok(Entry {
            ten_patterns,
            output_value_segments,
        })
    }
}

type Data = Vec<Entry>;

fn parse(input: &str) -> Result<Data, Error> {
    parse_lines_to_vec(input)
}

fn solve_1(input: &str) -> Result<usize, Error> {
    let parsed = parse(input)?;
    let result = parsed.iter().map(Entry::count_1_4_7_8).sum();
    Ok(result)
}

fn solve_2(input: &str) -> Result<usize, Error> {
    let parsed = parse(input)?;
    let result = parsed.iter().map(Entry::output_value).sum();
    Ok(result)
}

fn main() {
    let result_1 = solve_1(DATA);
    println!("Part 1: {result_1:?}");

    let result_2 = solve_2(DATA);
    println!("Part 2: {result_2:?}");
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = include_str!("../../resources/08-test.txt");

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(TEST_DATA), Ok(26));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(DATA), Ok(452));
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve_2(TEST_DATA), Ok(61229));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(DATA), Ok(1_096_964));
    }
}
