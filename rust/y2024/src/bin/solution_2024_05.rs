use std::cmp::Ordering;
use std::collections::{BTreeSet, HashMap};

use advent_of_code_common::parsing::Error;
use chumsky::prelude::*;
use rayon::iter::IntoParallelRefIterator;
use rayon::iter::ParallelIterator;

const DATA: &str = include_str!("../../resources/05.txt");

type Page = u32;
type R = Page;

#[derive(Clone, Debug)]
struct Update {
    pages: Vec<Page>,
    index: HashMap<Page, usize>,
}

impl Update {
    #[must_use]
    pub fn new(pages: Vec<Page>) -> Self {
        let index = pages
            .iter()
            .enumerate()
            .map(|(idx, &page)| (page, idx))
            .collect();
        Self { pages, index }
    }

    #[must_use]
    fn middle_page(&self) -> Page {
        self.pages[self.pages.len() / 2]
    }

    fn fix_invalid(&mut self, page_ordering_rules: &PageOrderingRules) {
        self.pages.sort_by(|&a, &b| {
            if page_ordering_rules.contains(a, b) {
                Ordering::Greater
            } else if page_ordering_rules.contains(b, a) {
                Ordering::Less
            } else {
                let a_idx = self.index.get(&a);
                let b_idx = self.index.get(&b);
                a_idx.cmp(&b_idx)
            }
        });
    }
}

#[derive(Clone, Debug)]
struct PageOrderingRules {
    preconditions: HashMap<Page, BTreeSet<Page>>,
}

impl PageOrderingRules {
    #[must_use]
    fn new(pairs: Vec<(Page, Page)>) -> Self {
        let mut preconditions: HashMap<Page, BTreeSet<Page>> = HashMap::new();
        for (a, b) in pairs {
            preconditions.entry(b).or_default().insert(a);
        }
        Self { preconditions }
    }

    #[must_use]
    fn valid_for(&self, update: &Update) -> bool {
        let mut remaining: BTreeSet<Page> = update.pages.iter().copied().collect();
        for page in &update.pages {
            let pre = self.preconditions.get(page).cloned().unwrap_or_default();
            let intersect = pre.intersection(&remaining);
            if intersect.count() > 0 {
                return false;
            }
            remaining.remove(page);
        }
        true
    }

    #[must_use]
    fn contains(&self, a: Page, b: Page) -> bool {
        self.preconditions
            .get(&b)
            .is_some_and(|set| set.contains(&a))
    }
}

#[derive(Clone, Debug)]
struct Data {
    page_ordering_rules: PageOrderingRules,
    updates:             Vec<Update>,
}

fn parser<'a>() -> impl Parser<'a, &'a str, Data> {
    // Note - cannot handle missing newline at end of file
    let number = text::int(10).from_str().unwrapped().map(|n: u32| n);

    let page_pair = number
        .then_ignore(just('|'))
        .then(number)
        .then_ignore(text::newline())
        .map(|(a, b)| (a, b));

    let page_ordering_rules = page_pair.repeated().at_least(1).collect::<Vec<_>>().map(PageOrderingRules::new);

    let page_list = number
        .separated_by(just(','))
        .collect::<Vec<_>>()
        .then_ignore(text::newline())
        .map(Update::new);

    let updates = page_list.repeated().at_least(1);

    page_ordering_rules
        .then_ignore(text::newline().repeated().at_least(1))
        .then(updates.collect::<Vec<_>>())
        .then_ignore(end())
        .map(|(page_ordering_rules, updates)| {
            Data {
                page_ordering_rules,
                updates,
            }
        })
}

fn parse(input: &str) -> Result<Data, Error> {
    let (result, error) = parser().parse(input).into_output_errors();
    result.ok_or(format!("{error:?}"))
}

fn solve_1(data: &Data) -> R {
    data.updates
        .par_iter()
        .filter(|update| data.page_ordering_rules.valid_for(update))
        .map(Update::middle_page)
        .sum()
}

fn solve_2(data: Data) -> R {
    let invalids = data
        .updates
        .into_iter()
        .filter(|update| !data.page_ordering_rules.valid_for(update));
    let fixed = invalids.into_iter().map(|mut update| {
        update.fix_invalid(&data.page_ordering_rules);
        update
    });
    fixed.map(|update| update.middle_page()).sum()
}

fn main() -> Result<(), Error> {
    let data = parse(DATA)?;

    let result_1 = solve_1(&data);
    println!("Part 1: {result_1}");

    let result_2 = solve_2(data);
    println!("Part 2: {result_2}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = include_str!("../../resources/05-test-00.txt");

    fn test_data() -> Data {
        parse(TEST_DATA).unwrap()
    }

    fn real_data() -> Data {
        parse(DATA).unwrap()
    }

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(&test_data()), 143);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(&real_data()), 4924);
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve_2(test_data()), 123);
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(real_data()), 6085);
    }
}
