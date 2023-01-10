from itertools import islice
import re
import numpy as np
import pandas as pd

from time import perf_counter

INPUT_FILE = "input/2016/day3.txt"

def batched(iterable, n):
  if n < 1:
    raise ValueError('n must be at least one')
  it = iter(iterable)
  while (batch := tuple(islice(it, n))):
      yield batch

def laod_input():
  data = open(INPUT_FILE, mode="rt").read()
  
  num = re.compile("\d+")
  
  def split(x: str):
    return list(map(int, num.findall(x)))

  return list(map(
    split,
    data.splitlines()
  ))

def solve():
  inp = laod_input()
  
  part1 = 0
  for trig in inp:
    st = sorted(trig)
    if st[0] + st[1] > st[2]:
      part1 += 1
  
  part2 = 0
  for trigs in batched(inp, 3):
    for i in range(3):
      st = sorted([trigs[0][i], trigs[1][i], trigs[2][i]])
      if st[0] + st[1] > st[2]:
        part2 += 1

  print(f"{part1 = }\n{part2 = }")

if __name__ == "__main__":
  t1 = perf_counter()
  solve()
  t2 = perf_counter()
  print(f"Execution time: {t2 - t1:0.4f} seconds")
