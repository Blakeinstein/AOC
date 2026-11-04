
from functools import lru_cache
import itertools
from typing import Callable
from multiprocess import Pool

import re
import hashlib

from time import perf_counter

INPUT_FILE = "input/2016/day15.txt"

def load_input() -> list[tuple[int, int]]:
  data = open(INPUT_FILE, mode="rt").read().strip()

  discs = []
  for line in data.splitlines():
      words = line.split()
      discs.append((int(words[3]), int(words[-1].rstrip('.'))))
  return discs

def is_zero(time: int, disc: tuple[int, tuple[int, int]]) -> bool:
    ac_t = time + disc[0]
    return ((disc[1][1] + ac_t) % disc[1][0]) == 0

def solve_pass_through(discs: list[tuple[int, int]]) -> int:
    time = 0

    while True:
        if all(is_zero(time, didx) for didx in enumerate(discs)):
            break
        time += 1

    return time - 1

def solve():
    discs = load_input()
    part1 = solve_pass_through(discs)
    print(f"{part1 = }")

    discs.append((11, 0))
    part2 = solve_pass_through(discs)
    print(f"{part2 = }")


if __name__ == "__main__":
  t1 = perf_counter()
  solve()
  t2 = perf_counter()
  print(f"Execution time: {t2 - t1:0.4f} seconds")
