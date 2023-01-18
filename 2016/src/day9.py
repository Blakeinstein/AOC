import numpy as np
import pandas as pd

from time import perf_counter

INPUT_FILE = "input/2016/day9.txt"

def laod_input():
  data = open(INPUT_FILE, mode="rt").read()
  return data.strip()

def solve():
  
  def expand(s: str, part2: bool):
    if "(" not in s:
      return len(s)
    
    ret = 0
    while "(" in s:
      pos = s.find("(")
      ret += pos
      s = s[pos:]
      end = s.find(")")
      marker = list(map(int, s[1:end].split("x")))
      s = s[end + 1:]
      if part2:
        ret += expand(s[:marker[0]], True) * marker[1]
      else:
        ret += len(s[:marker[0]]) * marker[1]
      s = s[marker[0]:]
    ret += len(s)
    return ret
  
  inp = laod_input()
  part1 = expand(inp, False)
  part2 = expand(inp, True)

  print(f"{part1 = }\n{part2 = }")

if __name__ == "__main__":
  t1 = perf_counter()
  solve()
  t2 = perf_counter()
  print(f"Execution time: {t2 - t1:0.4f} seconds")
