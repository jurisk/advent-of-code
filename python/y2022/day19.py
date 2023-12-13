from dataclasses import dataclass
from functools import reduce
from pathlib import Path
import re
import pytest
from z3 import *
from operator import mul

@dataclass(frozen=True)
class Blueprint:
    id: int
    oreRobotCostsNOre: int
    clayRobotCostsNOre: int
    obsidianRobotCostsNOre: int
    obsidianRobotCostsMClay: int
    geodeRobotCostsNOre: int
    geodeRobotCostsMObsidian: int

    def parse(line: str) -> "Blueprint":
        a, b, c, d, e, f, g = map(
            int,
            re.search(
                r"^Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.$",
                line,
            ).groups(),
        )
        return Blueprint(a, b, c, d, e, f, g)


def geodes_can_open(blueprint: Blueprint, t: int) -> int:
    o = Optimize()

    def create_int_vector(prefix: str):
        return [Int(f"{prefix}_{i}") for i in range(t + 1)]

    # Resources on turn `i`
    ore = create_int_vector("ore")
    clay = create_int_vector("clay")
    obsidian = create_int_vector("obsidian")
    geodes = create_int_vector("geodes")

    # Amount of robots on turn `i`
    ore_robots = create_int_vector("ore_robots")
    clay_robots = create_int_vector("clay_robots")
    obsidian_robots = create_int_vector("obsidian_robots")
    geodes_robots = create_int_vector("geodes_robots")

    # Are we buying such a robot on turn `i`?
    buy_ore_robot = create_int_vector("buy_ore_robot")
    buy_clay_robot = create_int_vector("buy_clay_robot")
    buy_obsidian_robot = create_int_vector("buy_obsidian_robot")
    buy_geodes_robot = create_int_vector("buy_geodes_robot")

    # Starting robots
    o.add(ore_robots[0] == 1)
    o.add(clay_robots[0] == 0)
    o.add(obsidian_robots[0] == 0)
    o.add(geodes_robots[0] == 0)

    # Starting minerals
    o.add(ore[0] == 0)
    o.add(clay[0] == 0)
    o.add(obsidian[0] == 0)
    o.add(geodes[0] == 0)

    o.add(buy_ore_robot[0] == 0)
    o.add(buy_clay_robot[0] == 0)
    o.add(buy_obsidian_robot[0] == 0)
    o.add(buy_geodes_robot[0] == 0)

    # New robots are a function of previous robots and robots bought
    for i in range(1, t + 1):
        o.add(ore_robots[i] == ore_robots[i - 1] + buy_ore_robot[i - 1])
        o.add(clay_robots[i] == clay_robots[i - 1] + buy_clay_robot[i - 1])
        o.add(obsidian_robots[i] == obsidian_robots[i - 1] + buy_obsidian_robot[i - 1])
        o.add(geodes_robots[i] == geodes_robots[i - 1] + buy_geodes_robot[i - 1])

    # Can only buy robots if enough minerals
    for i in range(1, t + 1):
        o.add(ore[i] >= 0)
        o.add(clay[i] >= 0)
        o.add(obsidian[i] >= 0)
        o.add(geodes[i] >= 0)

    for i in range(1, t + 1):
        o.add(ore_robots[i] >= 1)
        o.add(clay_robots[i] >= 0)
        o.add(obsidian_robots[i] >= 0)
        o.add(geodes_robots[i] >= 0)

    # Can buy 0 or 1 robots per turn
    for i in range(1, t + 1):
        o.add(buy_ore_robot[i] >= 0)
        o.add(buy_ore_robot[i] <= 1)
        o.add(buy_clay_robot[i] >= 0)
        o.add(buy_clay_robot[i] <= 1)
        o.add(buy_obsidian_robot[i] >= 0)
        o.add(buy_obsidian_robot[i] <= 1)
        o.add(buy_geodes_robot[i] >= 0)
        o.add(buy_geodes_robot[i] <= 1)
        o.add(Sum(buy_ore_robot[i], buy_clay_robot[i], buy_obsidian_robot[i], buy_geodes_robot[i]) <= 1)
        o.add(Sum(buy_ore_robot[i], buy_clay_robot[i], buy_obsidian_robot[i], buy_geodes_robot[i]) >= 0)

    # New minerals are function of previous amount, previous robots, cost of robots bought
    for i in range(1, t + 1):
        o.add(
            ore[i] == ore[i - 1]
                + ore_robots[i - 1]
                - buy_ore_robot[i] * blueprint.oreRobotCostsNOre
                - buy_clay_robot[i] * blueprint.clayRobotCostsNOre
                - buy_obsidian_robot[i] * blueprint.obsidianRobotCostsNOre
                - buy_geodes_robot[i] * blueprint.geodeRobotCostsNOre
        )

        o.add(
            clay[i] == clay[i - 1]
                + clay_robots[i - 1]
                - buy_obsidian_robot[i] * blueprint.obsidianRobotCostsMClay
        )

        o.add(
            obsidian[i] == obsidian[i - 1]
                + obsidian_robots[i - 1]
                - buy_geodes_robot[i] * blueprint.geodeRobotCostsMObsidian
        )

        o.add(geodes[i] == geodes[i - 1] + geodes_robots[i - 1])

    objective = geodes[t]
    o.maximize(objective)

    if o.check() != sat:
        print(o)

    model = o.model()

    debug = False
    if debug:
        for i in range(t + 1):
            print(f"Minute {i}:")
            print(f"Ore robots {model[ore_robots[i]].as_long()}")
            print(f"Clay robots {model[clay_robots[i]].as_long()}")
            print(f"Obsidian robots {model[obsidian_robots[i]].as_long()}")
            print(f"Geode robots {model[geodes_robots[i]].as_long()}")
            print()
            print(f"Buy ore robots {model[buy_ore_robot[i]].as_long()}")
            print(f"Buy clay robots {model[buy_clay_robot[i]].as_long()}")
            print(f"Buy obsidian robots {model[buy_obsidian_robot[i]].as_long()}")
            print(f"Buy geode robots {model[buy_geodes_robot[i]].as_long()}")
            print()
            print(f"Ore {model[ore[i]].as_long()}")
            print(f"Clay {model[clay[i]].as_long()}")
            print(f"Obsidian {model[obsidian[i]].as_long()}")
            print(f"Geodes {model[geodes[i]].as_long()}")
            print()
            print()
    
    result = model[objective].as_long()
    print(result)
    return result


def parse(file_name: str) -> list[Blueprint]:
    SCRIPT_DIR = Path(__file__).parent
    TEST_FILE = Path(SCRIPT_DIR, file_name)
    with open(TEST_FILE, "r", encoding="UTF-8") as file:
        lines = file.read().splitlines()
    return [Blueprint.parse(s) for s in lines]


def part1(blueprints: list[Blueprint]) -> int:
    return sum(
        [
            blueprint.id * geodes_can_open(blueprint, PART_1_MINUTES)
            for blueprint in blueprints
        ]
    )


def part2(blueprints: list[Blueprint]) -> int:
    results = [geodes_can_open(blueprint, PART_2_MINUTES) for blueprint in blueprints[:3]]
    result = reduce(mul, results, 1)
    return result


PART_1_MINUTES = 24
PART_2_MINUTES = 32


@pytest.fixture
def test_data():
    return parse("day19-test.txt")


@pytest.fixture
def real_data():
    return parse("day19.txt")


def test_test_1_1(test_data):
    assert geodes_can_open(test_data[0], PART_1_MINUTES) == 9

def test_test_1_2(test_data):
    assert geodes_can_open(test_data[1], PART_1_MINUTES) == 12

def test_test_1(test_data):
    assert part1(test_data) == 33

def test_test_2_1(test_data):
    assert geodes_can_open(test_data[0], PART_2_MINUTES) == 56

def test_test_2_2(test_data):
    assert geodes_can_open(test_data[1], PART_2_MINUTES) == 62

def test_real_1(real_data):
    assert part1(real_data) == 1624

def test_real_2(real_data):
    assert part2(real_data) == 12628

# Not sure that this actually works, it doesn't seem to terminate.
if __name__ == "__main__":
    data = parse("day19.txt")
    test_data = parse("day19-test.txt")

    test_1_result = part1(test_data)
    print(test_1_result)

    test_2_result = part2(test_data)
    print(test_2_result)

    real_1_result = part1(data)
    print(real_1_result)

    real_2 = part2(data)
    print(real_2)
