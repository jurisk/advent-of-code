type Number = i64;

fn fuel1(mass: Number) -> Number {
    (mass / 3) - 2 // Take mass, divide by three, round down, and subtract 2
}

fn fuel2(mass: Number) -> Number {
    let for_mass = fuel1(mass);
    if for_mass <= 0 {
        0
    } else {
        for_mass + fuel2(for_mass)
    }
}

fn parse_lines(input: &str) -> Vec<Number> {
    input
        .split('\n')
        .filter(|x| !x.is_empty())
        .map(|str| str.parse::<Number>().unwrap())
        .collect()
}

fn solve(data: &[Number], f: fn(Number) -> Number) -> Number {
    data.iter().map(|x| f(*x)).sum()
}

fn main() {
    let test_str = include_str!("../../resources/01-test.txt");
    let test_data = parse_lines(test_str);

    let test_solution_1 = solve(&test_data, fuel1);
    assert_eq!(test_solution_1, 2 + 2 + 654 + 33_583);

    let raw_data = include_str!("../../resources/01.txt");
    let data = parse_lines(raw_data);
    let solution_1 = solve(&data, fuel1);
    println!("{solution_1}");
    assert_eq!(solution_1, 3_456_641);

    assert_eq!(fuel2(100_756), 50_346);

    let test_solution_2 = solve(&test_data, fuel2);
    assert_eq!(test_solution_2, 2 + 2 + 966 + 50_346);

    let solution_2 = solve(&data, fuel2);
    assert_eq!(solution_2, 5_182_078);
    println!("{solution_2}");
}
