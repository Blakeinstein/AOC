from collections import defaultdict
from dataclasses import dataclass
from math import prod
from typing import Dict, List, Tuple
import re
import numpy as np
import pandas as pd

from time import perf_counter

INPUT_FILE = "input/2016/day10.txt"

def laod_input():
  data = open(INPUT_FILE, mode="rt").read()
  return data

@dataclass
class Node:
  is_output: bool
  index: int
  
  def append(self, ops: defaultdict[int, List[int]], bots: defaultdict[int, List[int]], val: int):
    if self.is_output:
      ops[self.index].append(val)
    else:
      bots[self.index].append(val)

def solve():
  inp = laod_input()
  
  bots: defaultdict[int, List[int]] = defaultdict(list)
  outputs: defaultdict[int, List[int]] = defaultdict(list)
  
  ops: Dict[int, Tuple[Node, Node]] = {}
  
  for line in inp.splitlines():
    parts = line.split()
    if parts[0].startswith("value"):
      bots[int(parts[-1])].append(int(parts[1]))
    else:
      ops[int(parts[1])] = (Node(parts[5] == "output", int(parts[6])), Node(parts[-2] == "output", int(parts[-1])))
  
  part1 = 0
  while bots:
    for k, v in dict(bots).items():
      if len(v) == 2:
        mn, mx = sorted(bots.pop(k))
        if mn == 17 and mx == 61:
          part1 = k
        low, high = ops[k]
        low.append(outputs, bots, mn)
        high.append(outputs, bots, mx)

  part2 = prod(outputs[k][0] for k in [0,1,2])

  print(f"{part1 = }\n{part2 = }")

if __name__ == "__main__":
  t1 = perf_counter()
  solve()
  t2 = perf_counter()
  print(f"Execution time: {t2 - t1:0.4f} seconds")
