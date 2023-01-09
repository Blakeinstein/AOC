import numpy as np
import pandas as pd

from time import perf_counter

from src.util import Point

INPUT_FILE = "input/2016/day2.txt"

def laod_input():
  data = open(INPUT_FILE, mode="rt").read()
  return data.splitlines()

def move(dir: str, p: Point) -> Point:
  match dir:
    case 'U':
      p.y -= 1
    case 'D':
      p.y += 1
    case 'R':
      p.x += 1
    case 'L':
      p.x -= 1
      
  return p

BOUNDS = range(0, 3)

def eval_line(line: str, p: Point) -> Point:
  for char in line:
    ideal = move(char, p.copy())
    if ideal.x in BOUNDS and ideal.y in BOUNDS:
      p = ideal
      
  return p

def eval_line2(line: str, p: Point) -> Point:
  for char in line:
    ideal = move(char, p.copy())
    dy = abs(ideal.y - 2)
    dx = abs(ideal.x - 2)
  
    if ideal.x in range(0 + dy, 5 - dy) and ideal.y in range(0 + dx, 5 - dx):
      p = ideal
  return p

def solve():
  inp = laod_input()
  nums = list(range(1, 10))
  nums2 = [
    [" ", " ", "1"],
    [" ", "2", "3", "4"],
    ["5", "6", "7", "8", "9"],
    [" ", "A", "B", "C"],
    [" ", " ", "D"],
  ]
  
  point = Point(1, 1)
  point2 = Point(0, 2)
  part1 = ""
  part2 = ""
  
  for l in inp:
    point = eval_line(l, point)
    part1 += str(nums[point.y * 3 + point.x])
    point2 = eval_line2(l, point2)
    part2 += nums2[point2.y][point2.x]
  
  print(f"{part1 = }\n{part2 = }")

if __name__ == "__main__":
  t1 = perf_counter()
  solve()
  t2 = perf_counter()
  print(f"Execution time: {t2 - t1:0.4f} seconds")
