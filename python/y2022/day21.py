"""
Advent of Code 2022, Day 21
"""

from pathlib import Path
from z3 import *

def add_equations_and_get_result(input: dict[str, str], target: str, extra_clauses = []):
      s = Solver()

      for extra_clause in extra_clauses:
            s.add(extra_clause)

      for (nameS, rest) in input.items():
            name = Int(nameS)
            
            if rest.isdigit():
                  s.add(name == int(rest))
            else:
                  aS, op, bS = rest.split()
                  a = Int(aS)
                  b = Int(bS)

                  match op:
                        case '+':
                              s.add(name == a + b)
                        case '-':
                              s.add(name == a - b)
                        case '*':
                              s.add(name == a * b)
                        case '/':
                              s.add(a % b == 0)
                              s.add(name == a / b)

      print(s.check())
      model = s.model()
      result = model[Int(target)]
      print(result)
      return result

def part1(input: dict[str, str]) -> int:
      return add_equations_and_get_result(input, 'root')

def part2(input: dict[str, str]) -> int:
      del input['humn']
      root = input['root']
      del input['root']

      aS, _, bS = root.split()
      a = Int(aS)
      b = Int(bS)

      return add_equations_and_get_result(input, 'humn', [a == b])

def parse(file_name: str) -> dict[str, str]:
      SCRIPT_DIR = Path(__file__).parent
      TEST_FILE = Path(SCRIPT_DIR, file_name)
      with open(TEST_FILE, "r", encoding="UTF-8") as file:
            lines = file.read().splitlines()
      entries = map(lambda s: s.split(': '), lines)
      return dict(entries)

test_data = parse("day21-test.txt")
real_data = parse("day21.txt")

assert part1(test_data) == 152
assert part1(real_data) == 87457751482938

assert part2(test_data) == 301
assert part2(real_data) == 3221245824363
