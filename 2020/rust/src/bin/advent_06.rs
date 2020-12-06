#![feature(iterator_fold_self)]

use std::collections::HashSet;

fn parse_multi_line(input: &str) -> Vec<Vec<&str>> {
    input
        .split("\n\n")
        .map(|x| x.lines().collect::<Vec<_>>())
        .collect::<Vec<_>>()
}

type Answer = char;
type Form = HashSet<Answer>;
type Group = Vec<Form>;

type MergeFunction<T> = fn(HashSet<T>, other: HashSet<T>) -> HashSet<T>;

fn merge_group(group: &Vec<HashSet<Answer>>, f: MergeFunction<Answer>) -> HashSet<Answer> {
    group.clone().into_iter().fold_first(f).unwrap_or_default()
}

fn solve(groups: &[Group], f: MergeFunction<Answer>) -> usize {
    groups.iter().map(|g| merge_group(g, f).len()).sum()
}

fn main() {
    let raw_data = include_str!("../../resources/06.txt");

    let groups: Vec<Group> = parse_multi_line(raw_data)
        .into_iter()
        .map(|x| {
            x.into_iter()
                .map(|s| s.chars().collect::<HashSet<Answer>>())
                .collect()
        })
        .collect();

    let union: MergeFunction<Answer> = |a: HashSet<Answer>, b: HashSet<Answer>| {
        HashSet::union(&a, &b).copied().collect::<HashSet<Answer>>()
    };

    let intersection: MergeFunction<Answer> = |a: HashSet<Answer>, b: HashSet<Answer>| {
        HashSet::intersection(&a, &b)
            .copied()
            .collect::<HashSet<Answer>>()
    };

    vec![union, intersection]
        .into_iter()
        .for_each(|f| println!("{}", solve(&groups, f)))
}
