use std::str::FromStr;

pub type Error = String;

pub fn parse_comma_separated_vec<T: FromStr>(input: &str) -> Result<Vec<T>, String> {
    let result: Result<Vec<T>, T::Err> = input
        .trim()
        .split(',')
        .map(|x| str::parse::<T>(x))
        .collect();
    match result {
        Ok(ok) => Ok(ok),
        Err(_) => Err(format!("Error parsing {}", input)),
    }
}
