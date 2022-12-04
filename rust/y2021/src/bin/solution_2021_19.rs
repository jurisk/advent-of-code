extern crate core;

use advent_of_code_2021::parsing::{
    parse_comma_separated_vec, parse_lines_to_nonempty, parse_separated_nonempty, Error,
};
use nalgebra::{Matrix3, Vector3};
use nonempty::{nonempty, NonEmpty};
use num_traits::abs;
use std::collections::HashSet;
use std::fmt::{Debug, Formatter};
use std::str::FromStr;

type N = i32;

const DATA: &str = include_str!("../../resources/19.txt");

type Rotation3D = Matrix3<i32>;

// See http://www.euclideanspace.com/maths/algebra/matrix/transforms/examples/index.htm
static ALL_ROTATIONS: [Rotation3D; 24] = [
    Matrix3::new(1, 0, 0, 0, 1, 0, 0, 0, 1),
    Matrix3::new(1, 0, 0, 0, 0, 1, 0, -1, 0),
    Matrix3::new(1, 0, 0, 0, -1, 0, 0, 0, -1),
    Matrix3::new(1, 0, 0, 0, 0, -1, 0, 1, 0),
    Matrix3::new(0, 1, 0, 0, 0, 1, 1, 0, 0),
    Matrix3::new(0, 1, 0, 1, 0, 0, 0, 0, -1),
    Matrix3::new(0, 1, 0, 0, 0, -1, -1, 0, 0),
    Matrix3::new(0, 1, 0, -1, 0, 0, 0, 0, 1),
    Matrix3::new(0, 0, 1, 1, 0, 0, 0, 1, 0),
    Matrix3::new(0, 0, 1, 0, 1, 0, -1, 0, 0),
    Matrix3::new(0, 0, 1, -1, 0, 0, 0, -1, 0),
    Matrix3::new(0, 0, 1, 0, -1, 0, 1, 0, 0),
    Matrix3::new(-1, 0, 0, 0, -1, 0, 0, 0, 1),
    Matrix3::new(-1, 0, 0, 0, 0, 1, 0, 1, 0),
    Matrix3::new(-1, 0, 0, 0, 1, 0, 0, 0, -1),
    Matrix3::new(-1, 0, 0, 0, 0, -1, 0, -1, 0),
    Matrix3::new(0, -1, 0, 0, 0, -1, 1, 0, 0),
    Matrix3::new(0, -1, 0, 1, 0, 0, 0, 0, 1),
    Matrix3::new(0, -1, 0, 0, 0, 1, -1, 0, 0),
    Matrix3::new(0, -1, 0, -1, 0, 0, 0, 0, -1),
    Matrix3::new(0, 0, -1, -1, 0, 0, 0, 1, 0),
    Matrix3::new(0, 0, -1, 0, 1, 0, 1, 0, 0),
    Matrix3::new(0, 0, -1, 1, 0, 0, 0, -1, 0),
    Matrix3::new(0, 0, -1, 0, -1, 0, -1, 0, 0),
];

#[derive(Clone, Eq, PartialEq, Hash)]
struct Sensor {
    id: usize,
    beacons: NonEmpty<Vector3<N>>,
}

fn vector_to_string(v: &Vector3<N>) -> String {
    format!("{},{},{}", v.x, v.y, v.z)
}

impl Debug for Sensor {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Sensor #{}", self.id)?;
        for v in self.beacons.iter() {
            writeln!(f, "{}", vector_to_string(v))?;
        }
        writeln!(f)
    }
}

impl Sensor {
    fn aligned_at(&self, facing: Rotation3D, offset: &Vector3<N>) -> AlignedSensor {
        AlignedSensor {
            sensor: self.clone(),
            offset: *offset,
            facing,
        }
    }

    fn rotate(&self, rotation: &Rotation3D) -> Sensor {
        let beacons: NonEmpty<Vector3<N>> = self.beacons.clone().map(|beacon| rotation * beacon);

        Sensor {
            id: self.id,
            beacons,
        }
    }

    fn beacons_if_offset_by(&self, offset: &Vector3<N>) -> NonEmpty<Vector3<N>> {
        self.beacons.clone().map(|b| b - offset)
    }
}

fn parse_coords_3d(input: &str) -> Result<Vector3<N>, Error> {
    let vec: Vec<i32> = parse_comma_separated_vec(input)?;
    if vec.len() == 3 {
        Ok(Vector3::new(vec[0], vec[1], vec[2]))
    } else {
        Err(format!("Unexpected {vec:?}"))
    }
}

impl FromStr for Sensor {
    type Err = Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let lines: NonEmpty<String> = parse_lines_to_nonempty(input)?;
        let id = if let Some(tmp) = lines.head.strip_prefix("--- scanner ") {
            if let Some(id_str) = tmp.strip_suffix(" ---") {
                id_str.parse().map_err(|err| format!("{err}"))
            } else {
                Err(format!("Failed to recognize {}", lines.head))
            }
        } else {
            Err(format!("Failed to recognize {}", lines.head))
        }?;
        let beacons_vec_res: Result<Vec<Vector3<N>>, Error> = lines
            .tail
            .into_iter()
            .map(|line| parse_coords_3d(&line))
            .collect();
        let beacons_vec = beacons_vec_res?;
        let beacons: NonEmpty<Vector3<N>> =
            NonEmpty::from_vec(beacons_vec).ok_or_else(|| "Empty".to_string())?;

        Ok(Sensor { id, beacons })
    }
}

struct Sensors {
    sensors: NonEmpty<Sensor>,
}

impl FromStr for Sensors {
    type Err = Error;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let sensors = parse_separated_nonempty(input, "\n\n")?;
        Ok(Sensors { sensors })
    }
}

#[derive(Clone, Eq, PartialEq)]
struct AlignedSensor {
    sensor: Sensor,
    offset: Vector3<N>,
    facing: Rotation3D,
}

impl Debug for AlignedSensor {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{:?}", self.sensor)?;
        writeln!(f, "Offset: {}", vector_to_string(&self.offset))?;
        writeln!(f, "Facing: {:?}", self.facing)?;
        writeln!(f)
    }
}

impl AlignedSensor {
    fn adjusted_beacons(&self) -> NonEmpty<Vector3<N>> {
        self.sensor
            .beacons
            .clone()
            .map(|x| self.facing * x + self.offset)
    }
}

#[derive(Clone)]
struct Solution {
    aligned_beacons: HashSet<Vector3<N>>,
    aligned_sensors: NonEmpty<AlignedSensor>,
    pending_sensors: HashSet<Sensor>,
}

// We could use "fingerprints" for a sensor (e.g. all offsets from the first beacon in the sensor)
// to find if we have an alignment faster.
fn aligned_beacon_count(beacons: &NonEmpty<Vector3<N>>, other: &HashSet<Vector3<N>>) -> usize {
    beacons.iter().filter(|b| other.contains(b)).count()
}

impl Debug for Solution {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Aligned beacons:")?;
        for b in &self.aligned_beacons {
            writeln!(f, "{}", vector_to_string(b))?;
        }
        writeln!(f)?;
        writeln!(f, "Aligned sensors:")?;
        for s in &self.aligned_sensors {
            writeln!(f, "{s:?}")?;
        }
        writeln!(f, "Pending sensors:")?;
        for s in &self.pending_sensors {
            writeln!(f, "{s:?}")?;
        }
        writeln!(f)
    }
}

impl Solution {
    fn create(sensors: &Sensors) -> Solution {
        let aligned_beacons: HashSet<Vector3<N>> =
            sensors.sensors.head.beacons.iter().copied().collect();

        let aligned_sensors: NonEmpty<AlignedSensor> = nonempty![sensors
            .sensors
            .head
            .aligned_at(Rotation3D::identity(), &Vector3::zeros())];

        let pending_sensors: HashSet<Sensor> = sensors.sensors.tail.clone().into_iter().collect();

        Solution {
            aligned_beacons,
            aligned_sensors,
            pending_sensors,
        }
    }

    fn find_matching_offset(&self, sensor: &Sensor, have_to_fit: usize) -> Option<Vector3<N>> {
        assert!(have_to_fit > 1);
        for a in &sensor.beacons {
            for b in &self.aligned_beacons {
                let candidate_offset: Vector3<N> = a - b;
                if aligned_beacon_count(
                    &sensor.beacons_if_offset_by(&candidate_offset),
                    &self.aligned_beacons,
                ) >= have_to_fit
                {
                    return Some(candidate_offset * -1);
                }
            }
        }

        None
    }

    fn try_to_align_assuming_rotation(
        &self,
        sensor: &Sensor,
        facing: &Rotation3D,
        have_to_fit: usize,
    ) -> Option<Vector3<N>> {
        let rotated = sensor.rotate(facing);
        self.find_matching_offset(&rotated, have_to_fit)
    }

    fn try_to_align(&self, sensor: &Sensor, have_to_fit: usize) -> Option<AlignedSensor> {
        for facing in ALL_ROTATIONS {
            if let Some(offset) = self.try_to_align_assuming_rotation(sensor, &facing, have_to_fit)
            {
                return Some(AlignedSensor {
                    sensor: sensor.clone(),
                    offset,
                    facing,
                });
            }
        }

        None
    }

    fn solution_step(&self, have_to_fit: usize) -> Result<Solution, Error> {
        for sensor in self.pending_sensors.clone() {
            if let Some(aligned) = self.try_to_align(&sensor, have_to_fit) {
                let mut aligned_beacons = self.aligned_beacons.clone();
                for beacon in aligned.adjusted_beacons() {
                    aligned_beacons.insert(beacon);
                }

                let mut aligned_sensors = self.aligned_sensors.clone();
                aligned_sensors.push(aligned);

                let mut pending_sensors = self.pending_sensors.clone();
                pending_sensors.remove(&sensor);

                return Ok(Solution {
                    aligned_beacons,
                    aligned_sensors,
                    pending_sensors,
                });
            }
        }

        Err("Failed to align anything".to_string())
    }

    fn solve(&self, have_to_fit: usize) -> Result<Solution, Error> {
        let mut result = self.clone();
        println!(
            "Aligned beacons: {}, Aligned sensors: {}, Pending sensors: {}",
            result.aligned_beacons.len(),
            result.aligned_sensors.len(),
            result.pending_sensors.len()
        );
        loop {
            result = result.solution_step(have_to_fit)?;
            println!(
                "Aligned beacons: {}, Aligned sensors: {}, Pending sensors: {}",
                result.aligned_beacons.len(),
                result.aligned_sensors.len(),
                result.pending_sensors.len()
            );
            if result.pending_sensors.is_empty() {
                return Ok(result);
            }
        }
    }
}

fn manhattan_distance(a: &Vector3<N>, b: &Vector3<N>) -> N {
    abs(a.x - b.x) + abs(a.y - b.y) + abs(a.z - b.z)
}

fn largest_distance(results: &NonEmpty<Vector3<N>>) -> N {
    results
        .iter()
        .flat_map(|a| results.iter().map(|b| manhattan_distance(a, b)))
        .max()
        .unwrap()
}

fn solve(input: &str) -> Result<(usize, N), Error> {
    let sensors: Sensors = input.parse()?;
    let solution: Solution = Solution::create(&sensors);
    let solved: Solution = solution.solve(12)?;
    Ok((
        solved.aligned_beacons.len(),
        largest_distance(&solved.aligned_sensors.map(|s| s.offset)),
    ))
}

fn main() {
    let result: Result<(usize, N), Error> = solve(DATA);
    println!("{result:?}");
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST: &str = include_str!("../../resources/19-test.txt");

    #[test]
    fn test_all_rotations() {
        let rotations = ALL_ROTATIONS;
        println!("{rotations:?}");
        assert_eq!(rotations.len(), 24);
    }

    #[test]
    fn test_solve_test_1() {
        let sensors: Sensors = TEST.parse().unwrap();
        let solution: Solution = Solution::create(&sensors);
        let sensor_to_align = sensors.sensors[1].clone();
        assert_eq!(sensor_to_align.id, 1);
        let result = solution.try_to_align(&sensor_to_align, 12).unwrap();
        assert_eq!(result.offset, Vector3::new(68, -1246, -43));

        let solution = solution.solution_step(12).unwrap();
        assert_eq!(solution.aligned_beacons.len(), 25 + 25 - 12);

        let solution = solution.solution_step(12).unwrap();
        assert_eq!(solution.aligned_sensors.len(), 3);

        let solution = solution.solution_step(12).unwrap();
        assert_eq!(solution.aligned_sensors.len(), 4);

        let solution = solution.solution_step(12).unwrap();
        assert_eq!(solution.aligned_sensors.len(), 5);

        assert_eq!(solution.aligned_beacons.len(), 79);
    }

    #[test]
    fn test_solve_test() {
        assert_eq!(solve(TEST), Ok((79, 3621)));
    }

    #[ignore] // Slow
    #[test]
    fn test_solve_real() {
        assert_eq!(solve(DATA), Ok((434, 11906)));
    }
}
