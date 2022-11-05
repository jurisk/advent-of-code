use itertools::izip;

fn parse(input: &str) -> Vec<u32> {
    input
        .split('\n')
        .filter(|x| !x.is_empty())
        .map(|x| x.parse::<u32>().unwrap())
        .collect()
}

fn count_larger(data: &[u32]) -> usize {
    data[0..data.len() - 1]
        .iter()
        .zip(&data[1..])
        .filter(|(a, b)| a < b)
        .count()
}

fn part_1(input: &str) -> usize {
    let data: Vec<_> = parse(input);
    count_larger(&data)
}

fn part_2(input: &str) -> usize {
    let data: Vec<_> = parse(input);
    let a = &data[0..data.len() - 2];
    let b = &data[1..data.len() - 1];
    let c = &data[2..];
    let sums: Vec<_> = izip!(a, b, c).map(|(x, y, z)| x + y + z).collect();
    count_larger(&sums)
}

fn main() {
    let test_data = "199
200
208
210
200
207
240
269
260
263";

    let test_result_1 = part_1(test_data);
    assert_eq!(test_result_1, 7);

    let data = include_str!("../../resources/01.txt");
    let result_1 = part_1(data);

    assert_eq!(result_1, 1681);
    println!("Part 1: {result_1}");

    let test_result_2 = part_2(test_data);
    assert_eq!(test_result_2, 5);

    let result_2 = part_2(data);
    assert_eq!(result_2, 1704);

    println!("Part 2: {result_2}");
}
