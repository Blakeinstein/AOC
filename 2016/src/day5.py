from random import random
import numpy as np
import pandas as pd
from hashlib import md5
from time import perf_counter

from src.util import bcolors

def solve():
  inp = "reyedfim"
  # inp = "abc"
  
  print(f"{inp=}")
  part1 = ""
  part2 = ["_" for _ in range(8)]
  count = 0
  i = 0
  while len(part1) < 8 or count < 8:
    hash = md5((inp + str(i)).encode()).hexdigest()
    i += 1
    if hash[:5] == "00000":
      c = hash[5]
      v = hash[6]
      if len(part1) < 8:
        part1 += c
      if c.isnumeric() and (idx := int(c)) < 8 and part2[idx] == "_":
        part2[idx] = v
        count += 1
    if i % 30000 == 0:
      print(bcolors.HEADER + "Decoding: " + bcolors.ENDC, end='')
      for char in part2:
        print(
          bcolors.HEADER + str(random())[-1] + bcolors.ENDC if char == '_'
          else bcolors.OKGREEN + char + bcolors.ENDC,
          end = ''
        )
      print('\r', end='')
        
  part2 = "".join(part2)

  print(f"{part1 = }\n{part2 = }")

if __name__ == "__main__":
  t1 = perf_counter()
  solve()
  t2 = perf_counter()
  print(f"Execution time: {t2 - t1:0.4f} seconds")
