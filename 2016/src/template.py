import numpy as np
import pandas as pd

from time import perf_counter

INPUT_FILE = None

def laod_input():
  data = open(INPUT_FILE, mode="rt").read()
  return data

def solve():
  inp = laod_input()
  
  part1 = 0
  part2 = 0

  print(f"{part1 = }\n{part2 = }")

if __name__ == "__main__":
  t1 = perf_counter()
  solve()
  t2 = perf_counter()
  print(f"Execution time: {t2 - t1:0.4f} seconds")
