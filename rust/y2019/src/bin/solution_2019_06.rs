use regex::Regex;

type Number = usize;

#[derive(Debug, Eq, PartialEq)]
struct Object {
    name: String,
}

impl Clone for Object {
    fn clone(&self) -> Self {
        Object {
            name: self.name.clone(),
        }
    }
}

#[derive(Debug)]
struct OrbitalRelationship {
    what:        Object,
    orbits_what: Object,
}

fn parse_lines(data: &str) -> Vec<OrbitalRelationship> {
    let re = Regex::new(r"^([0-9A-Za-z]+)\)([0-9A-Za-z]+)$").unwrap();

    data.lines()
        .filter(|s| !s.is_empty())
        .map(|s| {
            let captures = re.captures(s).unwrap();
            assert_eq!(captures.len(), 3);
            OrbitalRelationship {
                what:        Object {
                    name: captures[2].to_string(),
                },
                orbits_what: Object {
                    name: captures[1].to_string(),
                },
            }
        })
        .collect()
}

fn count_1(data: &[OrbitalRelationship], current: &Object, level: Number) -> Number {
    data.iter()
        .filter(|x| x.orbits_what == *current)
        .map(|x| count_1(data, &x.what, level + 1) + 1 + level)
        .sum()
}

fn solve_1(data: &[OrbitalRelationship], center_of_mass: &Object) -> Number {
    count_1(data, center_of_mass, 0)
}

// A monstrosity, but it works
fn path(
    data: &[OrbitalRelationship],
    ancestor: &Object,
    descendant: &Object,
) -> Option<Vec<Object>> {
    if ancestor == descendant {
        Some(vec![ancestor.clone()])
    } else {
        let children: Vec<&OrbitalRelationship> =
            data.iter().filter(|x| x.orbits_what == *ancestor).collect();
        if children.is_empty() {
            None
        } else {
            let possible_paths: Vec<Option<Vec<Object>>> = children
                .iter()
                .map(|x| x.what.clone())
                .map(|x| {
                    path(data, &x, descendant).map(|p| {
                        let mut result: Vec<Object> = vec![ancestor.clone()];
                        for v in p {
                            result.push(v);
                        }
                        result
                    })
                })
                .collect();

            let valid_path = possible_paths.iter().find(|x| x.is_some());
            valid_path.map(|x| x.clone().unwrap())
        }
    }
}

fn solve_2(
    data: &[OrbitalRelationship],
    center_of_mass: &Object,
    from: &Object,
    to: &Object,
) -> Number {
    let from_path = path(data, center_of_mass, from).unwrap();
    let to_path = path(data, center_of_mass, to).unwrap();

    let common = from_path
        .iter()
        .zip(to_path.iter())
        .take_while(|(a, b)| **a == **b)
        .count();

    from_path.len() + to_path.len() - (common * 2) - 2
}

fn main() {
    let test_str_1 = include_str!("../../resources/06-test-1.txt");
    let test_data_1 = parse_lines(test_str_1);
    assert_eq!(test_data_1.len(), 11);

    let test_str_2 = include_str!("../../resources/06-test-2.txt");
    let test_data_2 = parse_lines(test_str_2);
    assert_eq!(test_data_2.len(), 13);

    let data_str = include_str!("../../resources/06.txt");
    let data = parse_lines(data_str);
    assert_eq!(data.len(), 1_727);

    let center_of_mass: Object = Object {
        name: "COM".to_string(),
    };

    let test_part_1 = solve_1(&test_data_1, &center_of_mass);
    assert_eq!(test_part_1, 42);

    let part_1 = solve_1(&data, &center_of_mass);
    assert_eq!(part_1, 301_100);
    println!("Part 1: {part_1}");

    let you: Object = Object {
        name: "YOU".to_string(),
    };

    let san: Object = Object {
        name: "SAN".to_string(),
    };

    let test_part_2 = solve_2(&test_data_2, &center_of_mass, &you, &san);
    assert_eq!(test_part_2, 4);

    let part_2 = solve_2(&data, &center_of_mass, &you, &san);
    assert_eq!(part_2, 547);
    println!("Part 2: {part_2}");
}
