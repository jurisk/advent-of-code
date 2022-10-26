use nonempty::NonEmpty;
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;
use std::str::FromStr;

pub type Error = String;

fn convert_error<T, E: Debug>(input: &str, result: Result<T, E>) -> Result<T, Error> {
    result.map_err(|err| format!("Error parsing {} {:?}", input, err))
}

/// # Errors
///
/// Will return `Err` if parsing fails.
pub fn parse_lines_to_vec<T: FromStr>(input: &str) -> Result<Vec<T>, Error>
where
    T::Err: Debug,
{
    let result: Result<Vec<T>, T::Err> = input
        .split('\n')
        .filter(|x| !x.is_empty())
        .map(|x| str::parse::<T>(x.trim()))
        .collect();
    convert_error(input, result)
}

/// # Errors
///
/// Will return `Err` if parsing fails.
pub fn parse_lines_to_hashset<T: FromStr + Eq + Hash>(input: &str) -> Result<HashSet<T>, Error>
where
    T::Err: Debug,
{
    let result: Result<HashSet<T>, T::Err> = input
        .split('\n')
        .filter(|x| !x.is_empty())
        .map(|x| str::parse::<T>(x.trim()))
        .collect();

    convert_error(input, result)
}

/// # Errors
///
/// Will return `Err` if parsing fails.
pub fn parse_lines_to_nonempty<T: FromStr>(input: &str) -> Result<NonEmpty<T>, Error>
where
    T::Err: Debug,
{
    let vec: Vec<T> = parse_lines_to_vec(input)?;
    NonEmpty::from_vec(vec).ok_or_else(|| "Empty".to_string())
}

fn parse_kv_pair<K: FromStr + Eq + Hash, V: FromStr>(
    input: &str,
    delimiter: &str,
) -> Result<(K, V), Error>
where
    K::Err: Debug,
    V::Err: Debug,
{
    let (k_str, v_str) = split_into_two_strings(input, delimiter)?;
    let k_e: Result<K, K::Err> = k_str.parse::<K>();
    let v_e: Result<V, V::Err> = v_str.parse::<V>();
    let k = convert_error(&k_str, k_e)?;
    let v = convert_error(&v_str, v_e)?;
    Ok((k, v))
}

/// # Errors
///
/// Will return `Err` if parsing fails.
pub fn parse_lines_to_hashmap<K: FromStr + Eq + Hash, V: FromStr>(
    input: &str,
    delimiter: &str,
) -> Result<HashMap<K, V>, Error>
where
    K::Err: Debug,
    V::Err: Debug,
{
    let k_v: Result<Vec<(K, V)>, Error> = input
        .split('\n')
        .filter(|x| !x.is_empty())
        .map(|s| parse_kv_pair(s, delimiter))
        .collect();

    k_v.map(|x| x.into_iter().collect())
}

/// # Errors
///
/// Will return `Err` if parsing fails.
pub fn parse_string_to_nonempty<T: FromStr>(input: &str) -> Result<NonEmpty<T>, Error>
where
    T::Err: Debug,
{
    let vec: Result<Vec<T>, T::Err> = input
        .chars()
        .map(|ch| ch.to_string().parse::<T>())
        .collect();
    let result_vec = convert_error(input, vec)?;
    NonEmpty::from_vec(result_vec).ok_or_else(|| "Empty".to_string())
}

/// # Errors
///
/// Will return `Err` if parsing fails.
fn parse_separated_vec<T: FromStr>(input: &str, separator: &str) -> Result<Vec<T>, Error>
where
    T::Err: Debug,
{
    let result: Result<Vec<T>, T::Err> = input
        .trim()
        .split(separator)
        .map(|x| str::parse::<T>(x.trim()))
        .collect();

    convert_error(input, result)
}

/// # Errors
///
/// Will return `Err` if parsing fails.
pub fn parse_comma_separated_vec<T: FromStr>(input: &str) -> Result<Vec<T>, Error>
where
    T::Err: Debug,
{
    parse_separated_vec(input, ",")
}

/// # Errors
///
/// Will return `Err` if parsing fails.
pub fn parse_space_separated_vec<T: FromStr>(input: &str) -> Result<Vec<T>, Error>
where
    T::Err: Debug,
{
    parse_separated_vec(input, " ")
}

/// # Errors
///
/// Will return `Err` if parsing fails.
pub fn split_into_two_strings(input: &str, separator: &str) -> Result<(String, String), Error> {
    let results: Vec<_> = input.split(separator).collect();
    if results.len() == 2 {
        Ok((results[0].to_string(), results[1].to_string()))
    } else {
        Err(format!("Invalid count of segments: {:?}", results))
    }
}
