from functools import lru_cache
from typing import Callable

import numpy as np
import pandas as pd
from astar import AStar

from time import perf_counter

INPUT_FILE = "input/2016/day13.txt"

def load_input() -> int:
  data = open(INPUT_FILE, mode="rt").read()
  return int(data.strip())

def hamming_weight(num: int) -> int:
    c = 0
    while num:
        c += 1
        num &= num - 1

    return c

class CustomAStar(AStar):

    def __init__(
        self,
        is_wall: Callable[[int, int], bool]
    ):
        self.is_wall = is_wall

    @lru_cache
    def neighbors(self, n: tuple[int, int]):
        x, y = n
        possible = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

        for (nx, ny) in possible:
            if nx >= 0 and ny >= 0 and not self.is_wall(nx, ny):
                yield (nx,ny)

    def distance_between(self, n1, n2):
        x1, y1 = n1
        x2, y2 = n2

        return abs(x2 - x1) + abs(y2 - y1)

    def heuristic_cost_estimate(self, current, goal):
        return 1

    def is_goal_reached(self, current, goal):
        return current == goal

def solve():
  spc = load_input()

  @lru_cache
  def is_wall(x: int, y: int) -> bool:
      num = (x*x) + (3*x) + (2*x*y) + (y) + (y*y) + spc
      return bool(hamming_weight(num) & 1)

  astar = CustomAStar(is_wall)
  p1 = astar.astar((1, 1), (31, 39))
  fp = list(p1)
  part1 = len(fp) - 1

  nodes = set()
  visited = set()

  nodes.add((1, 1))

  steps = 50
  while steps >= 0:
      new_nodes= set()
      for node in nodes:
          if node in visited:
              continue
          visited.add(node)
          for new_node in astar.neighbors(node):
            new_nodes.add(new_node)
      nodes = new_nodes
      steps -= 1


  part2 = len(visited)

  print(f"{part1 = }\n{part2 = }")

if __name__ == "__main__":
  t1 = perf_counter()
  solve()
  t2 = perf_counter()
  print(f"Execution time: {t2 - t1:0.4f} seconds")
