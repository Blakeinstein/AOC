import io
from time import perf_counter

import numpy as np
import pandas as pd

INPUT_FILE = "input/2016/day22.txt"

type Pos = tuple[int, int]


def load_input(path: str = INPUT_FILE) -> pd.DataFrame:
    with open(path) as f:
        data = f.read()

    df = pd.read_csv(io.StringIO(data.split("\n", 1)[1]), sep=r"\s+")

    df[["PosX", "PosY"]] = df["Filesystem"].str.extract(r"x(\d+)-y(\d+)").astype(int)
    for col in ("Size", "Used", "Avail", "Use%"):
        df[col] = df[col].str.extract(r"(\d+)").astype(int)

    return df


def count_viable(df: pd.DataFrame) -> int:
    used_all = df["Used"].to_numpy()
    avail_all = df["Avail"].to_numpy()

    used = np.sort(used_all[used_all > 0])

    count = np.searchsorted(used, avail_all, side="left").sum()
    count -= np.count_nonzero((avail_all > used_all) & (used_all > 0))

    return int(count)


def to_grids(df: pd.DataFrame) -> tuple[np.ndarray, np.ndarray]:
    x = df["PosX"].to_numpy()
    y = df["PosY"].to_numpy()
    lx = x.max() + 1
    ly = y.max() + 1

    used = np.zeros((ly, lx), dtype=int)
    used[y, x] = df["Used"].to_numpy()

    passable = used <= df["Avail"].to_numpy().max()

    return used, passable


def bfs_distances(passable: np.ndarray, start: Pos, blocked: Pos | None = None) -> np.ndarray:
    dist = np.full(passable.shape, np.inf)
    dist[start[1], start[0]] = 0.0

    while True:
        neighbour = np.full_like(dist, np.inf)
        neighbour[1:, :] = np.minimum(neighbour[1:, :], dist[:-1, :])
        neighbour[:-1, :] = np.minimum(neighbour[:-1, :], dist[1:, :])
        neighbour[:, 1:] = np.minimum(neighbour[:, 1:], dist[:, :-1])
        neighbour[:, :-1] = np.minimum(neighbour[:, :-1], dist[:, 1:])
        step = np.where(passable, np.minimum(dist, neighbour + 1), np.inf)
        if blocked is not None:
            step[blocked[1], blocked[0]] = np.inf
        if np.array_equal(step, dist):
            return dist
        dist = step


def shortest_path(dist: np.ndarray, start: Pos, end: Pos) -> list[Pos]:
    if not np.isfinite(dist[end[1], end[0]]):
        raise ValueError(f"no path from {start} to {end}")

    path: list[Pos] = []
    x, y = end
    while (x, y) != start:
        path.append((x, y))
        for nx, ny in ((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)):
            if 0 <= ny < dist.shape[0] and 0 <= nx < dist.shape[1] and dist[ny, nx] == dist[y, x] - 1:
                x, y = nx, ny
                break
        else:
            raise ValueError(f"distance grid inconsistent at {(x, y)}")
    path.reverse()
    return path


def print_grid(used: np.ndarray, passable: np.ndarray, goal: Pos) -> None:
    grid = np.where(passable, ".", "#")
    grid[used == 0] = "_"
    grid[goal[1], goal[0]] = "G"
    print("\n".join("".join(row) for row in grid))


def part_2(df: pd.DataFrame) -> int:
    used, passable = to_grids(df)
    start = (0, 0)
    goal = (used.shape[1] - 1, 0)
    ys, xs = np.nonzero(used == 0)
    empty = (int(xs[0]), int(ys[0]))

    path_gs = shortest_path(bfs_distances(passable, goal), goal, start)
    moves = 0
    while goal != start:
        path_ = shortest_path(bfs_distances(passable, empty, blocked=goal), empty, path_gs.pop(0))
        moves += len(path_) + 1
        empty = goal
        goal = path_[-1]

    return moves


def solve() -> None:
    df = load_input()

    part1 = count_viable(df)
    part2 = part_2(df)
    print(f"{part1 = }")
    print(f"{part2 = }")


if __name__ == "__main__":
    t1 = perf_counter()
    solve()
    t2 = perf_counter()
    print(f"Execution time: {t2 - t1:0.4f} seconds")
