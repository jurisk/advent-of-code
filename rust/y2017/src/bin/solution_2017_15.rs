type Value = u32;
type Values = (Value, Value);

#[inline]
fn next_value(value: Value, factor: Value) -> Value {
    ((u64::from(value) * u64::from(factor)) % 2_147_483_647) as Value
}

#[inline]
fn next_1(values: Values) -> Values {
    (next_value(values.0, 16807), next_value(values.1, 48271))
}

fn next_value_with_constraint(value: Value, factor: Value, constraint: Value) -> Value {
    let mut current = value;
    loop {
        current = next_value(current, factor);
        if current.is_multiple_of(constraint) {
            return current;
        }
    }
}

#[inline]
fn next_2(values: Values) -> Values {
    (
        next_value_with_constraint(values.0, 16807, 4),
        next_value_with_constraint(values.1, 48271, 8),
    )
}

#[inline]
fn last_16_bits(value: Value) -> Value {
    value & 0xFFFF
}

#[inline]
fn matches(values: Values) -> bool {
    last_16_bits(values.0) == last_16_bits(values.1)
}

fn solve<F>(start: Values, times: usize, next: F) -> usize
where
    F: Fn(Values) -> Values,
{
    let mut result: usize = 0;
    let mut current: Values = start;
    for _ in 0 .. times {
        current = next(current);
        if matches(current) {
            result += 1;
        }
    }
    result
}

fn part_1(start: Values) -> usize {
    solve(start, 40_000_000, next_1)
}

fn part_2(start: Values) -> usize {
    solve(start, 5_000_000, next_2)
}

const DATA: Values = (873, 583);

fn main() {
    let result_1 = part_1(DATA);
    println!("Part 1: {result_1}");

    let result_2 = part_2(DATA);
    println!("Part 2: {result_2}");
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST: Values = (65, 8921);

    #[test]
    fn test_solve_1_test() {
        assert_eq!(part_1(TEST), 588);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(part_1(DATA), 631);
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(part_2(TEST), 309);
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(part_2(DATA), 279);
    }
}
