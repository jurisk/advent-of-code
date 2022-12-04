type Number = i64;

struct TestData {
    arrival_timestamp: Number,
    schedule: Vec<Option<Number>>,
}

fn parse_schedule(input: &str) -> Vec<Option<Number>> {
    input
        .split(',')
        .map(|str| match str {
            "x" => None,
            s => Some(s.parse::<Number>().unwrap()),
        })
        .collect()
}

fn parse_lines(input: &str) -> TestData {
    let mut lines = input.lines();
    let arrival_timestamp: Number = lines.next().unwrap().parse::<Number>().unwrap();
    let schedule: Vec<Option<Number>> = parse_schedule(lines.next().unwrap());

    TestData {
        arrival_timestamp,
        schedule,
    }
}

fn wait_time(arrival: Number, bus_id: Number) -> Number {
    let times_arrived_before = arrival / bus_id;
    (times_arrived_before + 1) * bus_id - arrival
}

fn flatten_schedule(schedule: &[Option<Number>]) -> Vec<Number> {
    schedule.iter().flatten().copied().collect()
}

fn solve1(input: &TestData) -> Number {
    let valid_buses: Vec<Number> = flatten_schedule(&input.schedule);

    let earliest_bus = *valid_buses
        .iter()
        .min_by_key(|x| wait_time(input.arrival_timestamp, **x))
        .unwrap();

    wait_time(input.arrival_timestamp, earliest_bus) * earliest_bus
}

// From https://rosettacode.org/wiki/Chinese_remainder_theorem#Rust
#[allow(clippy::many_single_char_names)]
fn egcd(a: Number, b: Number) -> (Number, Number, Number) {
    if a == 0 {
        (b, 0, 1)
    } else {
        let (g, x, y) = egcd(b % a, a);
        (g, y - (b / a) * x, x)
    }
}

fn mod_inv(x: Number, n: Number) -> Option<Number> {
    let (g, x, _) = egcd(x, n);
    if g == 1 {
        Some((x % n + n) % n)
    } else {
        None
    }
}

fn chinese_remainder(residues: &[Number], modulii: &[Number]) -> Option<Number> {
    let prod = modulii.iter().product::<Number>();
    let mut sum = 0;
    for (&residue, &modulus) in residues.iter().zip(modulii) {
        let p = prod / modulus;
        sum += residue * mod_inv(p, modulus)? * p
    }
    Some(sum % prod)
}

fn solve2(schedule: &[Option<Number>]) -> Number {
    // divisors
    let n: Vec<Number> = flatten_schedule(schedule);

    // remainders
    let a: Vec<Number> = schedule
        .iter()
        .enumerate()
        .filter_map(|(idx, a)| a.map(|n| n - idx as Number))
        .collect();

    println!("a {a:?}");
    println!("n {n:?}");
    println!();

    chinese_remainder(&a, &n).unwrap()
}

fn main() {
    let test_str = include_str!("../../resources/13-test.txt");
    let test_data = parse_lines(test_str);
    let test_solution_1 = solve1(&test_data);
    assert_eq!(test_solution_1, 295);

    let real_str = include_str!("../../resources/13.txt");
    let real_data = parse_lines(real_str);
    let real_solution_1 = solve1(&real_data);
    println!("{real_solution_1}");

    assert_eq!(solve2(&parse_schedule("17,x,13,19")), 3417);
    assert_eq!(solve2(&test_data.schedule), 1_068_781);
    assert_eq!(solve2(&parse_schedule("67,7,59,61")), 754_018);
    assert_eq!(solve2(&parse_schedule("67,x,7,59,61")), 779_210);
    assert_eq!(solve2(&parse_schedule("67,7,x,59,61")), 1_261_476);
    assert_eq!(solve2(&parse_schedule("1789,37,47,1889")), 1_202_161_486);

    let real_solution_2 = solve2(&real_data.schedule);
    assert!(real_solution_2 > 100_000_000_000_000);
    println!("{real_solution_2}");
}
