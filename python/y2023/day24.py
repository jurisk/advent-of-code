from sympy import *
from pathlib import Path
from dataclasses import dataclass
import re


@dataclass(frozen=True)
class Vector3:
    x: int
    y: int
    z: int


@dataclass(frozen=True)
class Movable:
    p: Vector3
    v: Vector3

    def parse(line: str) -> 'Movable':
        (px, py, pz, vx, vy, vz) = map(int, re.search(r'^(-?\d+), (-?\d+), (-?\d+) @ (-?\d+), (-?\d+), (-?\d+)$', line).groups())
        return Movable(Vector3(px, py, pz), Vector3(vx, vy, vz))


def parse(file_name: str) -> list[Movable]:
    script_dir = Path(__file__).parent
    test_file = Path(script_dir, file_name)
    with open(test_file, "r", encoding="UTF-8") as file:
        lines = file.read().splitlines()
    return [Movable.parse(s) for s in lines]


def part2(data: list[Movable]) -> int:
    px = symbols('px')
    py = symbols('y')
    pz = symbols('pz')
    vx = symbols('vx')
    vy = symbols('vy')
    vz = symbols('vz')

    equations = []
    solve_for = [px, py, pz, vx, vy, vz]

    # Note - 3 elements is enough, more meant it takes too long
    for idx, h in enumerate(data[:3]):
        tn = symbols(f"t{idx}")
        solve_for.append(tn)
        equations.append((px + vx * tn) - (h.p.x + h.v.x * tn))
        equations.append((py + vy * tn) - (h.p.y + h.v.y * tn))
        equations.append((pz + vz * tn) - (h.p.z + h.v.z * tn))

    sols = solve(equations, solve_for, dict=True)

    assert len(sols) == 1
    solution = sols[0]
    print(solution)
    return solution[px] + solution[py] + solution[pz]


if __name__ == '__main__':
    real_data = parse("day24.txt")

    answer_2 = part2(real_data)
    print(answer_2)
    assert answer_2 == 664822352550558
