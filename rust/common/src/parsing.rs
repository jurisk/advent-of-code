use std::collections::{BTreeSet, HashMap, HashSet};
use std::fmt::Debug;
use std::hash::Hash;
use std::str::FromStr;

use itertools::Itertools;
use nonempty::NonEmpty;
use pathfinding::matrix::Matrix;

pub type Error = String;

/// # Errors
///
/// Will return `Err` if input is `Err`.
pub fn convert_error<T, E: Debug>(input: &str, result: Result<T, E>) -> Result<T, Error> {
    result.map_err(|err| format!("Error parsing `{input}` {err:?}"))
}

/// # Errors
///
/// Will return `Err` if parsing fails.
pub fn parse_str<T: FromStr>(input: &str) -> Result<T, Error>
where
    <T as FromStr>::Err: Debug,
{
    str::parse::<T>(input).map_err(|err| format!("Error parsing `{input}`: {err:?}"))
}

/// # Errors
///
/// Will return `Err` if parsing fails.
pub fn parse_lines_to_vec<T: FromStr>(input: &str) -> Result<Vec<T>, Error>
where
    T::Err: Debug,
{
    input
        .lines()
        .filter(|x| !x.is_empty())
        .map(|x| parse_str(x.trim()))
        .collect()
}

/// # Errors
///
/// Will return `Err` if parsing fails.
pub fn parse_lines_to_vec_passing_parser<T, F>(input: &str, parser: F) -> Result<Vec<T>, Error>
where
    F: Fn(&str) -> Result<T, Error>,
{
    input
        .lines()
        .filter(|x| !x.is_empty())
        .map(|x| parser(x.trim()))
        .collect()
}

/// # Errors
///
/// Will return `Err` if parsing fails.
pub fn parse_double_newline_separated_lines_to_vec_passing_parser<T, F>(
    input: &str,
    parser: F,
) -> Result<Vec<T>, Error>
where
    F: Fn(&str) -> Result<T, Error>,
{
    segments_separated_by_double_newline(input)
        .into_iter()
        .map(|x| parser(x.trim()))
        .collect()
}

/// # Errors
///
/// Will return `Err` if parsing fails.
pub fn parse_lines_to_hashset<T: FromStr + Eq + Hash>(input: &str) -> Result<HashSet<T>, Error>
where
    T::Err: Debug,
{
    input
        .lines()
        .filter(|x| !x.is_empty())
        .map(|x| parse_str(x.trim()))
        .collect()
}

/// # Errors
///
/// Will return `Err` if parsing fails.
pub fn parse_lines_to_btreeset<T: Ord + FromStr + Eq + Hash>(
    input: &str,
) -> Result<BTreeSet<T>, Error>
where
    T::Err: Debug,
{
    input
        .lines()
        .filter(|x| !x.is_empty())
        .map(|x| parse_str(x.trim()))
        .collect()
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
    let k = parse_str(&k_str)?;
    let v = parse_str(&v_str)?;
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
        .lines()
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
    let vec: Result<Vec<T>, Error> = input.chars().map(|ch| parse_str(&ch.to_string())).collect();

    NonEmpty::from_vec(vec?).ok_or_else(|| "Empty".to_string())
}

/// # Errors
///
/// Will return `Err` if parsing fails.
pub fn parse_separated_vec<T: FromStr>(input: &str, separator: &str) -> Result<Vec<T>, Error>
where
    T::Err: Debug,
{
    input
        .trim()
        .split(separator)
        .map(|x| parse_str(x.trim()))
        .collect()
}

/// # Errors
///
/// Will return `Err` if parsing fails.
pub fn parse_ascii_whitespace_separated_vec<T: FromStr>(input: &str) -> Result<Vec<T>, Error>
where
    T::Err: Debug,
{
    input
        .trim()
        .split_ascii_whitespace()
        .map(|x| parse_str(x.trim()))
        .collect()
}

/// # Errors
///
/// Will return `Err` if parsing fails.
pub fn parse_separated_nonempty<T: FromStr>(
    input: &str,
    separator: &str,
) -> Result<NonEmpty<T>, Error>
where
    T::Err: Debug,
{
    let result_vec = parse_separated_vec(input, separator)?;
    NonEmpty::from_vec(result_vec).ok_or_else(|| "Empty".to_string())
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
        Err(format!("Invalid count of segments: {results:?}"))
    }
}

/// # Errors
///
/// Will return `Err` if parsing fails.
pub fn parse_u8_matrix(input: &str) -> Result<Matrix<u8>, Error> {
    parse_matrix(input, |ch| {
        let chr = ch as u8;
        if chr.is_ascii_digit() {
            Ok(chr - b'0')
        } else {
            Err(format!("{ch:?}"))
        }
    })
}

/// # Errors
///
/// Will return `Err` if parsing fails.
#[expect(clippy::redundant_closure, clippy::missing_panics_doc)]
pub fn parse_matrix<T, TE, PF>(input: &str, parse_element: PF) -> Result<Matrix<T>, TE>
where
    PF: Fn(char) -> Result<T, TE>,
{
    let result_vec_vec: Result<Vec<_>, TE> = input
        .lines()
        .filter(|r| !r.is_empty())
        .map(|r| {
            r.chars()
                .map(|ch| parse_element(ch))
                .collect::<Result<Vec<T>, TE>>()
        })
        .collect();

    let vec_vec = result_vec_vec?;

    let result = Matrix::from_vec(
        vec_vec.len(),
        vec_vec[0].len(),
        vec_vec.into_iter().flatten().collect(),
    );

    Ok(result.expect("Invalid matrix"))
}

#[must_use]
pub fn normalize_newlines(input: &str) -> String {
    input.lines().join("\n")
}

#[must_use]
pub fn segments_separated_by_double_newline(input: &str) -> Vec<String> {
    normalize_newlines(input)
        .split("\n\n")
        .map(ToString::to_string)
        .filter(|x| !x.is_empty())
        .collect()
}

#[expect(clippy::missing_errors_doc)]
pub fn parse_segments_separated_by_double_newline<T: FromStr>(input: &str) -> Result<Vec<T>, Error>
where
    <T as FromStr>::Err: Debug,
{
    segments_separated_by_double_newline(input)
        .into_iter()
        .map(|x| parse_str(&x))
        .collect()
}

/// # Errors
///
/// Will return `Err` if parsing fails.
pub fn split_into_two_segments_separated_by_double_newline(
    input: &str,
) -> Result<(String, String), Error> {
    split_into_two_strings(&normalize_newlines(input), "\n\n")
}

#[expect(clippy::missing_errors_doc)]
pub fn parse_pair_separated_by_double_newline<A, B, PA, PB>(
    input: &str,
    parse_left: PA,
    parse_right: PB,
) -> Result<(A, B), Error>
where
    PA: Fn(&str) -> Result<A, Error>,
    PB: Fn(&str) -> Result<B, Error>,
{
    let (a, b) = split_into_two_segments_separated_by_double_newline(input)?;
    Ok((parse_left(&a)?, parse_right(&b)?))
}

#[expect(clippy::missing_errors_doc)]
pub fn parse_pair_unsafe<A, B, PA, PB>(
    input: &str,
    delimiter: &str,
    parse_left: PA,
    parse_right: PB,
) -> Result<(A, B), Error>
where
    PA: Fn(&str) -> Result<A, Error>,
    PB: Fn(&str) -> Result<B, Error>,
{
    let (a, b) = split_into_two_strings(input, delimiter)?;
    Ok((parse_left(&a)?, parse_right(&b)?))
}
