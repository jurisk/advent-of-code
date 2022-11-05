use std::cmp::{max, Ordering};
use std::ops::Add;

#[derive(Debug)]
struct XY {
    x: i32,
    y: i32,
}

impl XY {
    fn zero() -> XY {
        XY { x: 0, y: 0 }
    }
}

impl Add for &XY {
    type Output = XY;

    fn add(self, rhs: Self) -> Self::Output {
        XY {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

struct Area {
    from: XY,
    to: XY,
}

impl Area {
    fn hopeless_for_y(&self, probe: &Probe) -> bool {
        probe.velocity.y <= 0 && probe.position.y < self.from.y
    }

    fn hopeless_for_x(&self, probe: &Probe) -> bool {
        probe.velocity.x == 0 && (probe.position.x > self.to.x || probe.position.x < self.from.x)
    }

    fn hitting(&self, position: &XY) -> bool {
        self.hitting_x(position) && self.hitting_y(position)
    }

    fn hitting_x(&self, position: &XY) -> bool {
        position.x >= self.from.x && position.x <= self.to.x
    }

    fn hitting_y(&self, position: &XY) -> bool {
        position.y >= self.from.y && position.y <= self.to.y
    }
}

const DATA: Area = Area {
    from: XY { x: 137, y: -98 },
    to: XY { x: 171, y: -73 },
};

#[derive(Debug)]
struct Probe {
    position: XY,
    velocity: XY,
}

impl Probe {
    fn new(start_velocity: XY) -> Probe {
        Probe {
            position: XY::zero(),
            velocity: start_velocity,
        }
    }

    fn next_step(&self) -> Probe {
        Probe {
            position: &self.position + &self.velocity,
            velocity: XY {
                x: match self.velocity.x.cmp(&0) {
                    Ordering::Less => self.velocity.x + 1,
                    Ordering::Equal => 0,
                    Ordering::Greater => self.velocity.x - 1,
                },
                y: self.velocity.y - 1,
            },
        }
    }

    fn will_hit_area(start_velocity: XY, area: &Area) -> bool {
        let mut current = Probe::new(start_velocity);
        while !area.hopeless_for_y(&current) {
            if area.hitting(&current.position) {
                return true;
            }

            current = current.next_step();
        }
        false
    }

    fn will_hit_area_x(start_velocity_x: i32, area: &Area) -> bool {
        let mut current = Probe::new(XY {
            x: start_velocity_x,
            y: 0,
        });

        while !area.hopeless_for_x(&current) {
            if area.hitting_x(&current.position) {
                return true;
            }

            current = current.next_step();
        }

        false
    }

    fn will_hit_area_y_with_max_reached(start_velocity_y: i32, area: &Area) -> Option<i32> {
        let mut current = Probe::new(XY {
            x: 0,
            y: start_velocity_y,
        });
        let mut max_y_seen = 0;
        while !area.hopeless_for_y(&current) {
            max_y_seen = max(max_y_seen, current.position.y);
            if area.hitting_y(&current.position) {
                return Some(max_y_seen);
            }

            current = current.next_step();
        }

        None
    }
}

fn solve_1(area: &Area) -> i32 {
    (-100..100)
        .filter_map(|start_y| Probe::will_hit_area_y_with_max_reached(start_y, area))
        .max()
        .unwrap()
}

fn solve_2(area: &Area) -> usize {
    let valid_x: Vec<_> = (-1000..1000)
        .filter(|start_x| Probe::will_hit_area_x(*start_x, area))
        .collect();
    let valid_y: Vec<_> = (-1000..1000)
        .filter(|start_y| Probe::will_hit_area_y_with_max_reached(*start_y, area).is_some())
        .collect();

    let mut result = 0;
    for x in &valid_x {
        for y in &valid_y {
            if Probe::will_hit_area(XY { x: *x, y: *y }, area) {
                result += 1;
            }
        }
    }
    result
}

fn main() {
    let result_1 = solve_1(&DATA);
    println!("Part 1: {result_1:?}");

    let result_2 = solve_2(&DATA);
    println!("Part 2: {result_2:?}");
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: Area = Area {
        from: XY { x: 20, y: -10 },
        to: XY { x: 30, y: -5 },
    };

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(&TEST_DATA), 45);
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(&DATA), 4753);
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve_2(&TEST_DATA), 112);
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(&DATA), 1546);
    }
}
