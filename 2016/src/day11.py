import copy
import re
from collections import defaultdict

from time import perf_counter
import functools
import operator

INPUT_FILE = "input/2016/day11.txt"

def test_input() -> str:
    return """The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.
    The second floor contains a hydrogen generator.
    The third floor contains a lithium generator.
    The fourth floor contains nothing relevant."""

def parse_word(word: str, key_map: dict[str, int]) -> int:

    is_generator = 'generator' in word

    key = word[:(-10 if is_generator else -21)]

    idx = key_map[key]

    return -idx if is_generator else idx

def parse_line(line: str, key_map: dict[str, int]) -> list[int]:
    if "nothing" in line:
        return []

    parts = filter(bool, re.split(r"\s*(?:\b(?:and|or)\b|[,.])\s*", line.split("contains ")[1][:-1]))

    return [
        parse_word(part, key_map) for part in parts
    ]

def load_input() -> tuple[list[list[int]], dict[str, int]]:
    data = open(INPUT_FILE, mode="rt").read()
    mp = defaultdict(lambda : len(mp))
    mp['elevator'] = 0
    return [parse_line(line, mp) for line in  data.splitlines()], mp

def getHash(elevator, floors):
    return str(elevator) + str([(len(floors[i]), sum(x < 0 for x in floors[i])) for i in range(4)])

def move(elevator, floors, direction, from_index, to_index):
    if from_index != None and to_index != None:
        floors[elevator + direction].insert(to_index, floors[elevator].pop(from_index))


def isValidState(floor):
    hasGen   = sum(x < 0 for x in floor)
    unpaired = sum(x > 0 and -x not in floor for x in floor)

    return not (hasGen and unpaired)


def traverse(start: list[list[int]]) -> int:
    end_hash = getHash(3, [[], [], [], functools.reduce(operator.iadd, start, [])])
    states   = set()

    def exploreState(req, elevator, floors, direction, moves, index1, index2 = None):
        if (direction * elevator) < req:
            move(elevator, floors, direction, index2, 0)
            move(elevator, floors, direction, index1, 0)

            next_hash = getHash(elevator + direction, floors)

            if next_hash not in states and floors[elevator + direction] and isValidState(floors[elevator + direction]) and isValidState(floors[elevator]):
                queue.append([elevator + direction, copy.deepcopy(floors), moves + 1, next_hash])

            move(elevator + direction, floors, -direction, 0, index1)
            move(elevator + direction, floors, -direction, 0, index2)

    queue = []
    queue.append([0, start, 0, getHash(0, start)])
    while True:
        elevator, floors, moves, cur_hash = queue.pop(0)

        if cur_hash not in states:
            states.add(cur_hash)

            if cur_hash == end_hash:
                return moves

            for index1 in range(len(floors[elevator])):
                for index2 in range(index1 + 1, len(floors[elevator])):
                    exploreState(3, elevator, floors, 1, moves, index1, index2)
                    exploreState(0, elevator, floors, -1, moves, index1, index2)

                exploreState(3, elevator, floors, 1, moves, index1)
                exploreState(0, elevator, floors, -1, moves, index1)

def solve():
    inp, km = load_input()

    part1 = traverse(inp)

    inp[0].extend([-km['an elerium'], km['an elerium'], -km['a dilithium'], km['a dilithium']])

    part2 = traverse(inp)

    print(f"{part1 = }\n{part2 = }")

if __name__ == "__main__":
    t1 = perf_counter()
    solve()
    t2 = perf_counter()
    print(f"Execution time: {t2 - t1:0.4f} seconds")
