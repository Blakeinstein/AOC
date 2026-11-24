from collections import defaultdict, deque
from functools import cache, lru_cache
from itertools import permutations

from astar import AStar
import numpy as np
import pandas as pd

from time import perf_counter

INPUT_FILE = "input/2016/day24.txt"

type Map = list[list[bool]]
type Pos = tuple[int, int]
type RelPos = dict[int, Pos]

def load_input() -> tuple[Map, RelPos]:
    data = open(INPUT_FILE, mode="rt").read()
    map = []
    locs = {}
    for i, line in enumerate(data.splitlines()):
        row = []
        for j, w in enumerate(line):
            match (w):
                case '#':
                    row.append(False)
                case '.':
                    row.append(True)
                case _:
                    locs[int(w)] = (j, i)
                    row.append(True)
        map.append(row)

    return map, locs

class MinD(AStar):
    def __init__(self, map: Map):
        self.map = map
        self.width = len(map[0])
        self.height = len(map)

    def neighbors(self, node: Pos):
        x, y = node
        possible = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

        for nx, ny in possible:
            if 0 <= nx < self.width and 0 <= ny < self.height and self.map[ny][nx]:
                yield (nx, ny)

    def distance_between(self, n1: Pos, n2: Pos):
        return 1

    def heuristic_cost_estimate(self, current, goal):
        x1, y1 = current
        x2, y2 = goal
        return abs(x2 - x1) + abs(y2 - y1)

    def is_goal_reached(self, current, goal):
        return current == goal

def solve():
    map, locs = load_input()
    part1 = 2**32
    part2 = 2**32


    loc_keys = list(locs.keys())
    min_d = defaultdict(dict)

    solver = MinD(map)

    for pair in [(x, y) for i, x in enumerate(loc_keys) for y in loc_keys[i + 1:]]:
        path = solver.astar(locs[pair[0]], locs[pair[1]])
        assert path is not None
        d = len(list(path)) - 1

        min_d[pair[0]][pair[1]] = d
        min_d[pair[1]][pair[0]] = d

    for perm in permutations(range(1, max(min_d.keys()) + 1)):
        path = (0,) + perm
        curr_d = 0
        for i in range(len(path) - 1):
            curr_d += min_d[path[i]][path[i+1]]


        part1 = min(curr_d, part1)
        part2 = min(curr_d + min_d[path[-1]][0], part2)

    print(f"{part1 = }")

    print(f"{part2 = }")


if __name__ == "__main__":
    t1 = perf_counter()
    solve()
    t2 = perf_counter()
    print(f"Execution time: {t2 - t1:0.4f} seconds")
