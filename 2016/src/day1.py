import numpy as np
import pandas as pd

from dataclasses import dataclass

from time import perf_counter

from src.util import Point

INPUT_FILE = "input/2016/day1.txt"

@dataclass
class Ins:
  mag: int = 0
  left: bool = False
  
def rotate(p: Point, is_left: bool) -> Point:
  new_p = Point(p.y, p.x)
  
  if is_left:
    new_p.y *= -1
  else:
    new_p.x *= -1

  return new_p

def laod_input():
  data = open(INPUT_FILE, mode="rt").read()
  return map(lambda x: Ins(int(x[1:]), x[0] == "L"), data.split(", "))

def solve():
  inp = laod_input()
  
  pos = Point()
  dir = Point(0, 1)
  
  grid = np.ndarray((2000, 2000)) 
  part2 = 0
  
  for ins in inp:
    dir = rotate(dir, ins.left)
    if not part2:
      for i in range(0, ins.mag):
        pos += dir
        grid[pos.yx()] += 1
        if grid[pos.yx()] > 1 and not part2:
          part2 = abs(pos)
    else:
      pos += dir * ins.mag
    
  part1 = abs(pos)

  print(f"{part1 = }\n{part2 = }")

if __name__ == "__main__":
  t1 = perf_counter()
  solve()
  t2 = perf_counter()
  print(f"Execution time: {t2 - t1:0.4f} seconds")
