use advent_of_code_common::parsing::Error;
use itertools::Itertools;
use serde_json::{from_str, Value};

fn parse(input: &str) -> Result<Value, Error> {
    from_str(input).map_err(|err| format!("Failed to parse JSON: {err}"))
}

fn solve(data: &Value, ignore_objects_with_property: &Option<String>) -> i64 {
    match data {
        Value::Null | Value::Bool(_) | Value::String(_) => 0,

        Value::Number(number) => number
            .as_i64()
            .unwrap_or_else(|| panic!("Failed to convert {number} to i64")),

        Value::Array(array) => array
            .iter()
            .map(|v| solve(v, ignore_objects_with_property))
            .sum(),

        Value::Object(object) => {
            let ignore = if let Some(ref p) = ignore_objects_with_property {
                // println!("qq {:?} {} {}", object.values().collect::<Vec<_>>(), p.clone(), object.values().contains(&Value::String(p.clone())));
                object.values().contains(&Value::String(p.clone()))
            } else {
                false
            };

            if ignore {
                0
            } else {
                object
                    .values()
                    .map(|v| solve(v, ignore_objects_with_property))
                    .sum()
            }
        },
    }
}

fn solve_1(data: &Value) -> i64 {
    solve(data, &None)
}

fn solve_2(data: &Value) -> i64 {
    solve(data, &Some("red".to_string()))
}

fn part_1(input: &str) -> Result<i64, Error> {
    parse(input).map(|input| solve_1(&input))
}

fn part_2(input: &str) -> Result<i64, Error> {
    parse(input).map(|input| solve_2(&input))
}

const DATA: &str = include_str!("../../resources/12.txt");

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

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), Ok(119433));
    }

    #[test]
    fn test_solve_2_test_1() {
        assert_eq!(part_2(&r#"[1,{"c":"red","b":2},3]"#), Ok(4));
    }

    #[test]
    fn test_solve_2_test_2() {
        assert_eq!(part_2(&r#"{"d":"red","e":[1,2,3,4],"f":5}"#), Ok(0));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), Ok(68466));
    }
}
