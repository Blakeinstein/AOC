from collections import Counter
from dataclasses import dataclass
from functools import cmp_to_key
import re
import string
import numpy as np
import pandas as pd

from time import perf_counter

INPUT_FILE = "input/2016/day4.txt"

@dataclass
class Room:
  name: str
  id: int
  checksum: str

def laod_input():
  data = open(INPUT_FILE, mode="rt").read()
  r = re.compile("([a-z-]+)-(\d+)\[([a-z]+)\]")
  
  def make_room(x: str):
    m = r.match(x)
    return Room(m.group(1), int(m.group(2)), m.group(3))
  
  return list(map(
    make_room,
    data.splitlines()
  ))
  
def make_checksum(i: str):
  counts = Counter(i.replace("-", ""))
  chars = [c for c in string.ascii_lowercase]
  return "".join(
    sorted(chars, key= lambda x : -counts[x])[:5]
  )
  
DIST = ord('z') - ord('a') + 1

def rotate(c, delta):
  next = (ord(c) - ord('a') + delta) % DIST + ord('a')
  return chr(next)

def decrypt_name(name: str, dist: int):
  delta = dist % DIST
  parts = [[rotate(c, delta) for c in part] for part in name.split("-")]
  
  return " ".join(["".join(c) for c in parts])

def solve():
  inp = laod_input()
  
  part1 = 0
  part2 = 0
  for room in inp:
    checksum = make_checksum(room.name)
    if room.checksum == checksum:
      real_name = decrypt_name(room.name, room.id)
      if "northpole" in real_name:
        part2 = room.id
      part1 += room.id


  print(f"{part1 = }\n{part2 = }")

if __name__ == "__main__":
  t1 = perf_counter()
  solve()
  t2 = perf_counter()
  print(f"Execution time: {t2 - t1:0.4f} seconds")
