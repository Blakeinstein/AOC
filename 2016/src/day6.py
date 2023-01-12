from collections import defaultdict
import numpy as np
import pandas as pd

from time import perf_counter

INPUT_FILE = "input/2016/day6.txt"

def laod_input():
  data = open(INPUT_FILE, mode="rt").read()
  return list(data.splitlines())

def solve():
  inp = laod_input()
  
  counts = []
  for _ in range(len(inp[0])):
    counts.append(defaultdict(int))
  
  for word in inp:
    for i, c in enumerate(word):
      counts[i][c] += 1
  
  part1 = "".join([max(x, key=x.get) for x in counts])
  part2 = "".join([min(x, key=x.get) for x in counts])
  
  print(f"{part1 = }\n{part2 = }")

if __name__ == "__main__":
  t1 = perf_counter()
  solve()
  t2 = perf_counter()
  print(f"Execution time: {t2 - t1:0.4f} seconds")
