
from functools import lru_cache
import itertools
from typing import Callable
from multiprocess import Pool

import re
import hashlib

from time import perf_counter

INPUT_FILE = "input/2016/day14.txt"

def load_input() -> str:
  data = open(INPUT_FILE, mode="rt").read()
  return data.strip()

salt = load_input()


trip = r'([a-z0-9])\1{2}'



def is_key(calc_hash: Callable[[int], str], idx: int) -> bool:
    triplet = calc_first_triplet(calc_hash, idx)
    if triplet:
        for i in range(1000):
            next_hash = calc_hash(idx + i + 1)
            if triplet*5 in next_hash:
                return True

    return False

@lru_cache
def calc_first_triplet(calc_hash: Callable[[int], str], idx: int) -> str | None:
    key = calc_hash(idx)
    if m := re.search(trip, key):
        return m.group()[0]
    else:
        return None

def part_1():
    @lru_cache
    def calc_hash(idx: int) -> str:
        src = f'{salt}{idx}'
        return hashlib.md5(src.encode()).hexdigest()

    count = 0
    idx = 0

    while count < 64:
        if is_key(calc_hash, idx):
              count += 1
        idx += 1

    return idx - 1

def part_2():
    @lru_cache
    def calc_hash(idx: int) -> str:
        src = f'{salt}{idx}'

        for _ in range(2017):
            src = hashlib.md5(src.encode()).hexdigest()

        return src

    def thread_work(work_range: tuple[int, int]) -> list[int]:
        # print("processing ", work_range)
        start_idx, end_idx = work_range
        result = []
        for i in range(start_idx, end_idx):
            if is_key(calc_hash, i):
                result.append(i)
        return result

    batch_size = 2000
    thread_count = 14

    batches = ((i, i + batch_size) for i in itertools.count(0, batch_size))

    key_idx = []
    with Pool(thread_count) as pool:
        for res in pool.imap(thread_work, batches):
            key_idx.extend(res)
            if len(key_idx) > 64:
                return sorted(key_idx)[63]

def solve():
  part1 = part_1()
  print(f'{part1 = }')
  part2 = part_2()
  print(f'{part2 = }')

if __name__ == "__main__":
  t1 = perf_counter()
  solve()
  t2 = perf_counter()
  print(f"Execution time: {t2 - t1:0.4f} seconds")
