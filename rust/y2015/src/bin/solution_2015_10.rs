fn parse(input: &str) -> Vec<u8> {
    input.chars().map(|ch| ch as u8 - b'0').collect()
}

fn convert(data: &[u8]) -> Vec<u8> {
    let mut result: Vec<u8> = Vec::new();
    let mut idx: usize = 0;

    while idx < data.len() {
        let len = data[idx ..].iter().take_while(|&&x| x == data[idx]).count();
        let count = u8::try_from(len).unwrap_or_else(|err| {
            panic!("Failed to convert {len} elements from {data:?}: {err:?}")
        });
        assert!(count < 10);
        assert!(count > 0);
        result.push(count);
        result.push(data[idx]);
        idx += len;
    }

    result
}

fn solve(data: &[u8], n: usize) -> Vec<u8> {
    let mut current: Vec<u8> = data.to_vec();
    for _ in 0 .. n {
        let next = convert(&current);
        current = next;
    }

    current
}

fn solution(input: &str, n: usize) -> usize {
    let data = parse(input);
    solve(&data, n).iter().map(|n| n + b'0').len()
}

const DATA: &str = "1113222113";

fn main() {
    let result_1 = solution(DATA, 40);
    println!("Part 1: {result_1}");

    let result_2 = solution(DATA, 50);
    println!("Part 2: {result_2}");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_convert_1() {
        assert_eq!(convert(&parse(&"111221")), parse("312211"));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solution(DATA, 40), 252594);
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solution(DATA, 50), 3579328);
    }
}
