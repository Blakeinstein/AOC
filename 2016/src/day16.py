from bisect import insort
from functools import reduce

import numpy as np
import pandas as pd

from time import perf_counter

INPUT_FILE = "input/2016/day16.txt"


def load_input() -> tuple[str, int]:
    data = open(INPUT_FILE, mode="rt").read()
    p = data.split(",")
    return (p[0], int(p[1]))


tbl = str.maketrans("01", "10")


def next_gen(num: str) -> str:
    return f"{num}0{num[::-1].translate(tbl)}"


def calc_hash(num: str) -> str:
    next_hash = ""
    it = iter(num)
    for a, b in zip(it, it):
        next_hash += "1" if a == b else "0"

    if len(next_hash) & 1:
        return next_hash
    return calc_hash(next_hash)


def solve():
    start, limit = load_input()

    gen = start

    while len(gen) < limit:
        gen = next_gen(gen)

    part1 = calc_hash(gen[:limit])
    print(f"{part1 = }")

    new_limit = 35651584

    while len(gen) < new_limit:
        gen = next_gen(gen)

    part2 = calc_hash(gen[:new_limit])
    print(f"{part2 = }")


if __name__ == "__main__":
    t1 = perf_counter()
    solve()
    t2 = perf_counter()
    print(f"Execution time: {t2 - t1:0.4f} seconds")
