import hashlib
from collections.abc import Generator
from time import perf_counter

INPUT_FILE = "input/2016/day17.txt"

def load_input() -> str:
  data = open(INPUT_FILE, mode="rt").read()
  return data.strip()

type Pos = tuple[int,int]

def get_md5_dir(path: str) -> list[bool]:
    hash = hashlib.md5(path.encode('utf-8')).hexdigest()
    return [c > 'a' for c in hash[:4]]

char = ['U', 'D', 'L', 'R']

def get_next_pos(n: Pos, past_steps: str) -> Generator[tuple[Pos, str]]:
    x, y = n
    doors = get_md5_dir(past_steps)
    possible = [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]


    for idx, ((nx, ny), is_open) in enumerate(zip(possible, doors)):
        if nx >= 0 and ny >= 0 and nx <= 3 and ny <= 3 and is_open:
            yield (nx,ny), char[idx]


def part_1(current: Pos, goal: Pos, path: str) -> str | None:
    if current == goal:
        return path

    current_min = None
    for next_pos, dir in get_next_pos(current, path):
        # print(next_pos, dir)
        if found := part_1(next_pos, goal, path + dir):
            if current_min is None or len(found) < len(current_min):
                current_min = found

    return current_min

def part_2(current: Pos, goal: Pos, path: str) -> str | None:
    if current == goal:
        return path

    current_max = None
    for next_pos, dir in get_next_pos(current, path):
        # print(next_pos, dir)
        if found := part_2(next_pos, goal, path + dir):
            if current_max is None or len(found) > len(current_max):
                current_max = found

    return current_max


def solve():
  passcode = load_input()

  start = (0, 0)
  end_goal = (3,3)


  part1 = (part_1(start, end_goal, passcode) or '')[len(passcode):] or None
  print(f"{part1 = }")

  part2 = len(part_2(start, end_goal, passcode) or '') - len(passcode)
  print(f"{part2 = }")

if __name__ == "__main__":
  t1 = perf_counter()
  solve()
  t2 = perf_counter()
  print(f"Execution time: {t2 - t1:0.4f} seconds")
