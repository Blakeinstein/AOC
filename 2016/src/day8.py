from dataclasses import dataclass
from typing import List
import numpy as np
import pandas as pd

from time import perf_counter

INPUT_FILE = "input/2016/day8.txt"

@dataclass
class Instruction:
  is_rect: bool
  dim1: int
  dim2: int
  is_row: bool

def laod_input() -> List[Instruction]:
  data = open(INPUT_FILE, mode="rt").read()
  ins = []
  for line in data.splitlines():
    if line.startswith('rect'):
      dim1, dim2 = list(map(int, line[5:].split("x")))
      ins.append(Instruction(True, dim1, dim2, False))
    else:
      dim1, dim2 = list(map(int, line[line.find("=") + 1:].split(" by ")))
      ins.append(Instruction(False, dim1, dim2, "row" in line))

  return ins

def rect(screen: np.ndarray, x: int, y: int):
  screen[:y, :x] = True
  
def shift(screen: np.ndarray, idx: int, delta: int, is_row: bool):
  if is_row:
    screen[idx] = np.roll(screen[idx], delta)
  else:
    screen[:, idx] = np.roll(screen[:, idx], delta)

def solve():
  inp = laod_input()
  
  screen = np.full((6, 50), False, dtype=bool)
  
  for ins in inp:
    if ins.is_rect:
      rect(screen, ins.dim1, ins.dim2)
    else:
      shift(screen, ins.dim1, ins.dim2, ins.is_row)
  
  part1 = screen.sum()

  print(f"{part1 = }")
  print("part2 =>")
  for row in screen:
    print(*map(lambda x : "#" if x else " ", row), sep="")

if __name__ == "__main__":
  t1 = perf_counter()
  solve()
  t2 = perf_counter()
  print(f"Execution time: {t2 - t1:0.4f} seconds")
