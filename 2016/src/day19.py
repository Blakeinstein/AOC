from collections import defaultdict
from math import floor, log, log2

import numpy as np
import pandas as pd

from time import perf_counter

INPUT_FILE = "input/2016/day19.txt"

def load_input() -> int:
  data = open(INPUT_FILE, mode="rt").read()
  return int(data.strip())

def part_1(n: int) -> int:
    return 1 + 2 * (n - pow(2, floor(log2(n))))

def part_2(n: int) -> int:
    p = 3**int(log(n-1,3))
    return n-p+max(n-2*p,0)


def solve():
  count = load_input()

  part1 = part_1(count)
  print(f"{part1 = }")

  part2 = part_2(count)
  print(f"{part2 = }")

if __name__ == "__main__":
  t1 = perf_counter()
  solve()
  t2 = perf_counter()
  print(f"Execution time: {t2 - t1:0.4f} seconds")
