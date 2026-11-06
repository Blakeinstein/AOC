import numpy as np
import pandas as pd

from time import perf_counter

INPUT_FILE = "input/2016/day18.txt"

type Pos = tuple[int, int]
type Map = list[str]

def get_tile(map: Map, pos: Pos) -> str:
    if pos[0] < 0 or pos[1] < 0 or pos[1] >= len(map) or pos[0] >= len(map[0]):
        return "."
    return map[pos[1]][pos[0]]

def is_next_trap(map: Map, pos: Pos) -> bool:
    x, y = pos

    left = x - 1, y - 1
    center = x , y - 1
    right = x + 1, y - 1

    prev = [get_tile(map, left), get_tile(map, center), get_tile(map, right)]
    match(prev):
        case ["^", "^", "."] | [".", "^", "^"] | ["^", ".", "."] | [".", ".", "^"]:
            return True
        case _:
            return False

def eval_next_row(map: Map) -> int:
    next_row = ""
    trap_count = 0
    row = len(map)

    for i in range(len(map[0])):
        next_row += "^" if is_next_trap(map, (i, row)) else "."
        trap_count += next_row[-1] == "."

    map.append(next_row)
    return trap_count

def load_input() -> Map:
  data = open(INPUT_FILE, mode="rt").read()
  return [data.strip()]

def solve():
  map = load_input()


  trap_count = map[0].count(".")

  n1 = 40
  while len(map) < n1:
      trap_count += eval_next_row(map)

  part1 = trap_count
  print(f"{part1 = }")

  n2 = 400000
  while len(map) < n2:
      trap_count += eval_next_row(map)

  part2 = trap_count
  print(f"{part2 = }")

if __name__ == "__main__":
  t1 = perf_counter()
  solve()
  t2 = perf_counter()
  print(f"Execution time: {t2 - t1:0.4f} seconds")
