from z3 import *
from dataclasses import dataclass
from pathlib import Path
from pprint import pprint
from more_itertools import quantify
import re

@dataclass(frozen=True)
class Coords3D:
    x: int
    y: int
    z: int

    def manhattan_distance(self, other: 'Coords3D') -> int:
        return abs(self.x - other.x) + abs(self.y - other.y) + abs(self.z - other.z)

    def manhattan_distance_to_origin(self) -> int:
        return abs(self.x) + abs(self.y) + abs(self.z)

origin = Coords3D(0, 0, 0)

@dataclass(frozen=True)
class Nanobot:
    location: Coords3D
    radius: int

    def id(self) -> str:
        return "p_" + str(self.location.x) + "_" + str(self.location.y) + "_" + str(self.location.z) + "_" + str(self.radius)

    def parse(line: str) -> 'Nanobot':
        x, y, z, r = map(int, re.search(r'^pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(-?\d+)$', line).groups())
        return Nanobot(Coords3D(x, y, z), r) 

def parse(file_name: str) -> list[Nanobot]:
      SCRIPT_DIR = Path(__file__).parent
      TEST_FILE = Path(SCRIPT_DIR, file_name)
      with open(TEST_FILE, "r", encoding="UTF-8") as file:
            lines = file.read().splitlines()
      return [Nanobot.parse(s) for s in lines]

def part1(nanobots: list[Nanobot]) -> int:
    strongest = max(nanobots, key = lambda n: n.radius)
    return quantify(nanobots, pred = lambda n: n.location.manhattan_distance(strongest.location) <= strongest.radius)

def find_coords_in_range_of_most_points(nanobots: list[Nanobot]) -> Coords3D:
    o = Optimize()

    def zabs(x: int):
        return If(x >= 0, x, -x)

    def boolean_to_int(x):
        return If(x, 1, 0)

    (x, y, z) = (Int("x"), Int("y"), Int("z"))
    
    def range_query(nanobot: Nanobot):
        (nx, ny, nz, nr) = (nanobot.location.x, nanobot.location.y, nanobot.location.z, nanobot.radius)
        return zabs(x - nx) + zabs(y - ny) + zabs(z - nz) <= nr

    point_in_range = [Int(n.id()) for n in nanobots]
    for idx in range(len(nanobots)):
        o.add(point_in_range[idx] == boolean_to_int(range_query(nanobots[idx])))

    points_in_range = Int("points_in_range")
    o.add(points_in_range == sum(point_in_range))

    distance_from_origin = Int("distance_from_origin")
    o.add(distance_from_origin == zabs(x) + zabs(y) + zabs(z))

    h1 = o.maximize(points_in_range)
    h2 = o.minimize(distance_from_origin)

    print(o)        
    print(o.check())

    print(o.lower(h1), o.upper(h1))
    print(o.lower(h2), o.upper(h2))

    model = o.model()
    print(model)

    result = Coords3D(model[x].as_long(), model[y].as_long(), model[z].as_long())
    print(result)
    return result

def part2(nanobots: list[Nanobot]) -> int:
    coords = find_coords_in_range_of_most_points(nanobots)
    return coords.manhattan_distance_to_origin()

if __name__ == '__main__':
    test_data_1 = parse("day23-test-1.txt")
    test_data_2 = parse("day23-test-2.txt")
    real_data = parse("day23.txt")

    test_1 = part1(test_data_1)
    print(test_1)
    assert test_1 == 7

    real_1 = part1(real_data)
    print(real_1)
    assert real_1 == 393

    test_2 = part2(test_data_2)
    print(test_2)
    assert test_2 == 36
    real_2 = part2(real_data)
    print(real_2)
    assert real_2 == 113799398
