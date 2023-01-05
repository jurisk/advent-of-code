use advent_of_code_common::parsing::{parse_lines_to_vec, Error};
use recap::Recap;
use serde::Deserialize;
use std::cmp::max;

#[derive(Debug, Deserialize, Recap)]
#[recap(
    regex = r#"^\w+: capacity (?P<capacity>-?\d+), durability (?P<durability>-?\d+), flavor (?P<flavor>-?\d+), texture (?P<texture>-?\d+), calories (?P<calories>\d+)$"#
)]
struct Ingredient {
    capacity: i32,
    durability: i32,
    flavor: i32,
    texture: i32,
    calories: u32,
}

type Extractor = dyn Fn(&Ingredient) -> i32;

impl Ingredient {
    fn score<F>(ingredients: &[Ingredient], amounts: &[u32], extract: F) -> i32
    where
        F: Fn(&Ingredient) -> i32,
    {
        let result = ingredients
            .iter()
            .zip(amounts.iter())
            .map(|(ingredient, &amount)| extract(ingredient) * i32::try_from(amount).unwrap())
            .sum();

        max(result, 0)
    }

    fn calories(ingredients: &[Ingredient], amounts: &[u32]) -> u32 {
        ingredients
            .iter()
            .zip(amounts.iter())
            .map(|(ingredient, &amount)| ingredient.calories * amount)
            .sum()
    }

    fn total_score(ingredients: &[Ingredient], amounts: &[u32]) -> i32 {
        let aspects: Vec<Box<Extractor>> = vec![
            Box::new(|x| x.capacity),
            Box::new(|x| x.durability),
            Box::new(|x| x.flavor),
            Box::new(|x| x.texture),
        ];

        aspects
            .into_iter()
            .map(|f| Ingredient::score(ingredients, amounts, f))
            .product()
    }
}

fn parse(input: &str) -> Result<Vec<Ingredient>, Error> {
    parse_lines_to_vec(input)
}

fn solve(ingredients: &[Ingredient], max_teaspoons: u32, expected_calories: Option<u32>) -> i32 {
    println!("{ingredients:?}");
    assert!(ingredients.len() <= 4);

    let mut best = i32::MIN;

    // This is questionable as we are limited to 4 ingredients but because the amounts can be 0,
    // it also works for less than 4 (such as 2 which the tests have)
    for a in 0u32..=max_teaspoons {
        for b in 0..=(max_teaspoons - a) {
            for c in 0..=(max_teaspoons - a - b) {
                let d = max_teaspoons - a - b - c;
                let amounts = vec![a, b, c, d];

                let calories_ok = if let Some(calories_limit) = expected_calories {
                    Ingredient::calories(ingredients, &amounts) == calories_limit
                } else {
                    true
                };

                if calories_ok {
                    let obtained = Ingredient::total_score(ingredients, &amounts);
                    if obtained > best {
                        best = obtained;
                    }
                }
            }
        }
    }

    best
}

fn solve_1(ingredients: &[Ingredient]) -> i32 {
    solve(ingredients, 100, None)
}

fn solve_2(ingredients: &[Ingredient]) -> i32 {
    solve(ingredients, 100, Some(500))
}

fn part_1(input: &str) -> Result<i32, Error> {
    let data = parse(input)?;
    Ok(solve_1(&data))
}

fn part_2(input: &str) -> Result<i32, Error> {
    let data = parse(input)?;
    Ok(solve_2(&data))
}

const DATA: &str = include_str!("../../resources/15.txt");

fn main() -> Result<(), Error> {
    let result_1 = part_1(DATA)?;
    println!("Part 1: {result_1}");

    let result_2 = part_2(DATA)?;
    println!("Part 2: {result_2}");

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST: &str = include_str!("../../resources/15-test.txt");

    #[test]
    fn test_solve_1_test() {
        assert_eq!(part_1(TEST), Ok(62842880));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(18965440));
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(part_2(TEST), Ok(57600000));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(15862900));
    }
}
