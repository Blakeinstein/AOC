import numpy as np
import pandas as pd

import re
from time import perf_counter

INPUT_FILE = "input/2016/day7.txt"

def laod_input():
  data = open(INPUT_FILE, mode="rt").read()
  return data.splitlines()

def abba(s: str) -> bool:
  i = 0
  while i + 3 < len(s):
    if s[i] == s[i+3] and s[i+1] == s[i+2] and s[i] != s[i+1]:
      return True
    i += 1
  return False

def aba(word: str):
  pairs = []
  i = 0
  while i + 2 < len(word):
    if word[i] == word[i+2] and word[i] != word[i+1]:
      pairs.append((word[i], word[i+1]))
    i += 1
  return pairs

def split(word: str):
  hypernet = []
  reg = []
  is_hp = False
  curr_word = ""
  word += "]"
  for c in word:
    if c == "[" or c == "]":
      if is_hp:
        hypernet.append(curr_word)
      else:
        reg.append(curr_word)
      is_hp = not is_hp
      curr_word = ""
      continue
    curr_word += c
  return hypernet, reg

def solve():
  inp = laod_input()
  
  part1 = 0
  part2 = 0
  for word in inp:
    hypernet, reg = split(word)

    for p in hypernet:
      if abba(p):
        break
    else:
      for p in reg:
        if abba(p):
          part1 += 1
        
    abas = set([x for p in reg for x in aba(p)]) 
    for p in [x for p in hypernet for x in aba(p)]:
      if (p[1], p[0]) in abas:
        break
    else:
      part2 -= 1
    part2 += 1


  print(f"{part1 = }\n{part2 = }")

if __name__ == "__main__":
  t1 = perf_counter()
  solve()
  t2 = perf_counter()
  print(f"Execution time: {t2 - t1:0.4f} seconds")
