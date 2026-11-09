from pkgutil import resolve_name

import numpy as np
import pandas as pd

from time import perf_counter

INPUT_FILE = "input/2016/day20.txt"

type Range = tuple[int, int]

def load_input() -> list[Range]:
  data = open(INPUT_FILE, mode="rt").read()

  ranges = []

  for line in data.strip().splitlines():
      parts = line.strip().split('-')
      ranges.append((int(parts[0]), int(parts[1])))

  return ranges


def solve():
  ranges = load_input()

  ranges.sort()

  ip = 0
  idx = 0
  MAX = 2**32
  part1 = None
  part2 = 0

  while ip <= MAX:
      if idx >= len(ranges):
          part2 += (MAX - ip)
          break

      l, u = ranges[idx]
      if ip >= l:
          if ip <= u:
              ip = u + 1
              continue
          idx += 1
      else:
          if part1 is None:
              part1 = ip
          part2 += 1
          ip += 1

  print(f"{part1 = }")

  print(f"{part2 = }")

if __name__ == "__main__":
  t1 = perf_counter()
  solve()
  t2 = perf_counter()
  print(f"Execution time: {t2 - t1:0.4f} seconds")
