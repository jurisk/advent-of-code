use num_traits::signum;
use std::ops::Add;

type Number = i64;

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
struct Vector3D {
    x: Number,
    y: Number,
    z: Number,
}

impl Vector3D {
    fn new(x: Number, y: Number, z: Number) -> Vector3D {
        Vector3D { x, y, z }
    }

    fn energy(&self) -> Number {
        self.x.abs() + self.y.abs() + self.z.abs()
    }
}

impl Add for Vector3D {
    type Output = Vector3D;

    fn add(self, rhs: Self) -> Self::Output {
        Vector3D::new(self.x + rhs.x, self.y + rhs.y, self.z + rhs.z)
    }
}

#[derive(Eq, PartialEq, Copy, Clone, Debug)]
struct Moon {
    position: Vector3D,
    velocity: Vector3D,
}

impl Moon {
    fn new(position: Vector3D) -> Moon {
        Moon {
            position,
            velocity: Vector3D::new(0, 0, 0),
        }
    }

    fn energy(&self) -> Number {
        self.position.energy() * self.velocity.energy()
    }
}

fn data() -> Vec<Vector3D> {
    vec![
        Vector3D::new(-3, 10, -1),
        Vector3D::new(-12, -10, -5),
        Vector3D::new(-9, 0, 10),
        Vector3D::new(7, -5, -3),
    ]
}

fn diff(a: &Moon, b: &Moon) -> Vector3D {
    Vector3D {
        x: signum(b.position.x - a.position.x),
        y: signum(b.position.y - a.position.y),
        z: signum(b.position.z - a.position.z),
    }
}

fn new_velocity(for_moon: &Moon, all_moons: &[Moon]) -> Vector3D {
    let vectors: Vec<Vector3D> = all_moons
        .iter()
        .filter(|&x| x != for_moon)
        .map(|moon| diff(for_moon, moon))
        .collect();
    vectors.iter().fold(for_moon.velocity, |a, b| a + *b)
}

fn do_step(moons: &[Moon]) -> Vec<Moon> {
    let new_velocities: Vec<Vector3D> =
        moons.iter().map(|moon| new_velocity(moon, moons)).collect();
    moons
        .iter()
        .zip(new_velocities.iter())
        .map(|(&moon, &new_velocity)| Moon {
            position: moon.position + new_velocity,
            velocity: new_velocity,
        })
        .collect()
}

fn create_moons(data: &[Vector3D]) -> Vec<Moon> {
    data.iter().map(|&position| Moon::new(position)).collect()
}

fn solve_1(data: &[Vector3D], steps: u16) -> Number {
    let mut moons: Vec<Moon> = create_moons(data);

    (1..=steps).for_each(|_| {
        moons = do_step(&moons);
    });

    moons.iter().map(Moon::energy).sum()
}

fn part_1() {
    let result = solve_1(&data(), 1000);
    println!("Part 1: {}", result);
    assert_eq!(result, 10944);
}

fn loop_size(data: &[Vector3D], extract_coordinate: fn(&Vector3D) -> Number) -> Number {
    let mut moons: Vec<Moon> = create_moons(data);
    let initial_positions: Vec<Number> = data.iter().map(extract_coordinate).collect();

    for i in 1.. {
        moons = do_step(&moons);
        let current_positions: Vec<_> = moons
            .iter()
            .map(|moon| extract_coordinate(&moon.position))
            .collect();
        let zero_velocities = moons
            .iter()
            .map(|moon| extract_coordinate(&moon.velocity))
            .all(|x| x == 0);
        if current_positions == initial_positions && zero_velocities {
            return i;
        }
    }

    unreachable!()
}

fn gcd(a: Number, b: Number) -> Number {
    if b == 0 {
        a
    } else {
        gcd(b, a % b)
    }
}

fn lcm(a: Number, b: Number) -> Number {
    a * b / gcd(a, b)
}

fn lcm3(a: Number, b: Number, c: Number) -> Number {
    lcm(a, lcm(b, c))
}

fn solve_2(data: &[Vector3D]) -> Number {
    let x_loop = loop_size(data, |v| v.x);
    let y_loop = loop_size(data, |v| v.y);
    let z_loop = loop_size(data, |v| v.z);

    lcm3(x_loop, y_loop, z_loop)
}

fn part_2() {
    let data = data();
    let result = solve_2(&data);
    println!("Part 2: {}", result);
    assert_eq!(result, 484_244_804_958_744);
}

fn main() {
    part_1();
    part_2();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_1_test_1() {
        let test_data_1: Vec<Vector3D> = vec![
            Vector3D::new(-1, 0, 2),
            Vector3D::new(2, -10, -7),
            Vector3D::new(4, -8, 8),
            Vector3D::new(3, 5, -1),
        ];

        assert_eq!(solve_1(&test_data_1, 10), 179);
        assert_eq!(solve_2(&test_data_1), 2772);
    }

    #[test]
    fn test_part_1_test_2() {
        let test_data_2: Vec<Vector3D> = vec![
            Vector3D::new(-8, -10, 0),
            Vector3D::new(5, 5, 10),
            Vector3D::new(2, -7, 3),
            Vector3D::new(9, -8, -3),
        ];

        assert_eq!(solve_1(&test_data_2, 100), 1940);
        assert_eq!(solve_2(&test_data_2), 4_686_774_924);
    }

    #[test]
    fn test_part_1() {
        part_1();
    }

    #[test]
    fn test_part_2() {
        part_2();
    }
}
