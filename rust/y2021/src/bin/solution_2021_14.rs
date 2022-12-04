use advent_of_code_common::parsing::{
    parse_lines_to_hashmap, parse_string_to_nonempty, split_into_two_strings, Error,
};
use advent_of_code_common::utils::additive_hashmap_from_vec;
use nonempty::NonEmpty;
use std::collections::HashMap;
use std::str::FromStr;

const DATA: &str = include_str!("../../resources/14.txt");

#[derive(Clone, Eq, PartialEq, Hash, Debug, Copy)]
struct Element(char);

impl FromStr for Element {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let ch = s.chars().next().ok_or("Fail")?;
        Ok(Element(ch))
    }
}

type Polymer = NonEmpty<Element>;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
struct SearchPattern {
    a: Element,
    b: Element,
}

impl FromStr for SearchPattern {
    type Err = Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let mut chars = input.chars();
        let a_ch = chars.next().ok_or("Fail")?;
        let b_ch = chars.next().ok_or("Fail")?;
        let a = Element(a_ch);
        let b = Element(b_ch);
        Ok(SearchPattern { a, b })
    }
}

struct Data {
    starting_polymer: Polymer,
    templates: HashMap<SearchPattern, Element>,
}

impl Data {
    fn starting_polymer_as_frequency_map(&self) -> HashMap<SearchPattern, usize> {
        let mut result = HashMap::new();
        for idx in 0..self.starting_polymer.len() - 1 {
            let a = self.starting_polymer[idx];
            let b = self.starting_polymer[idx + 1];
            let k = SearchPattern { a, b };
            let existing = result.get(&k).unwrap_or(&0);
            result.insert(k, existing + 1);
        }
        result
    }

    fn next_iteration(
        &self,
        frequencies: &HashMap<SearchPattern, usize>,
    ) -> HashMap<SearchPattern, usize> {
        let vec: Vec<_> = frequencies
            .iter()
            .flat_map(|(pattern, &count)| {
                let found = self.templates.get(pattern).unwrap();
                let left = SearchPattern {
                    a: pattern.a,
                    b: *found,
                };
                let right = SearchPattern {
                    a: *found,
                    b: pattern.b,
                };
                vec![(left, count), (right, count)]
            })
            .collect();

        additive_hashmap_from_vec(vec)
    }

    fn element_occurrences_after_n_steps(&self, steps: usize) -> HashMap<Element, usize> {
        let mut frequencies = self.starting_polymer_as_frequency_map();
        for _ in 0..steps {
            frequencies = self.next_iteration(&frequencies);
        }

        let first: Vec<_> = vec![(self.starting_polymer.head, 1)];
        let other: Vec<_> = frequencies.iter().map(|(k, &v)| (k.b, v)).collect();
        let joined = vec![first, other].concat();
        additive_hashmap_from_vec(joined)
    }
}

impl FromStr for Data {
    type Err = Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let (a, b) = split_into_two_strings(input, "\n\n")?;
        let starting_polymer: NonEmpty<Element> = parse_string_to_nonempty(&a)?;
        let templates: HashMap<SearchPattern, Element> = parse_lines_to_hashmap(&b, " -> ")?;

        Ok(Data {
            starting_polymer,
            templates,
        })
    }
}

fn solve(input: &str, iterations: usize) -> Result<usize, Error> {
    let data: Data = input.parse()?;
    let elements: HashMap<Element, usize> = data.element_occurrences_after_n_steps(iterations);
    let frequencies: Vec<usize> = elements.values().copied().collect();
    let min = frequencies.iter().min().unwrap();
    let max = frequencies.iter().max().unwrap();
    Ok(max - min)
}

fn main() {
    let result_1 = solve(DATA, 10);
    println!("Part 1: {result_1:?}");

    let result_2 = solve(DATA, 40);
    println!("Part 2: {result_2:?}");
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = include_str!("../../resources/14-test.txt");

    #[test]
    fn test_solve_on_test_data() {
        let test_data: Data = TEST_DATA.parse().unwrap();
        let c: Element = "C".parse().unwrap();
        let b: Element = "B".parse().unwrap();
        let n: Element = "N".parse().unwrap();
        let h: Element = "H".parse().unwrap();

        assert_eq!(
            test_data.element_occurrences_after_n_steps(0),
            HashMap::from([(c, 1), (b, 1), (n, 2)])
        );
        assert_eq!(
            test_data.element_occurrences_after_n_steps(1),
            HashMap::from([(h, 1), (b, 2), (c, 2), (n, 2)])
        );
        assert_eq!(
            test_data.element_occurrences_after_n_steps(2),
            HashMap::from([(c, 4), (h, 1), (n, 2), (b, 6)])
        );
        assert_eq!(
            test_data.element_occurrences_after_n_steps(10),
            HashMap::from([(h, 161), (n, 865), (b, 1749), (c, 298)])
        );
    }

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve(TEST_DATA, 10), Ok(1588));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve(DATA, 10), Ok(2112));
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve(TEST_DATA, 40), Ok(2_188_189_693_529));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve(DATA, 40), Ok(3_243_771_149_914));
    }
}
