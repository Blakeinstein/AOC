from dataclasses import dataclass
from enum import Enum

import numpy as np
import pandas as pd

from time import perf_counter

INPUT_FILE = "input/2016/day12.txt"

def load_input():
  data = open(INPUT_FILE, mode="rt").read()
  return data

class Op(Enum):
    CPY = 'cpy'
    INC = 'inc'
    DEC = 'dec'
    JNZ = 'jnz'

@dataclass
class Instruction:
    op: Op
    op1: str | int
    op2: str | int | None

def parse_line(line: str) -> Instruction:
    parts = line.split(" ")
    return Instruction(
        Op[parts[0].upper()],
        parts[1],
        parts[2] if len(parts) > 2 else None
    )

def parse(inp: str) -> list[Instruction]:
    return list(
        map(
            parse_line,
            inp.splitlines()
        )
    )

def exec(mem: dict[str, int], ins: Instruction, curr_idx: int) -> int:
    match(ins.op):
        case Op.CPY:
            val = mem[ins.op1] if ins.op1 in mem else int(ins.op1)
            mem[str(ins.op2)] = val

        case Op.INC:
            mem[str(ins.op1)] += 1

        case Op.DEC:
            mem[str(ins.op1)] -= 1

        case Op.JNZ:
            key = str(ins.op1)
            val = mem[key] if key in mem else int(ins.op1)
            if val != 0 and ins.op2 is not None:
                return curr_idx + int(ins.op2)

        case _:
            raise "INVALID INSTRUCTION " + ins.op

    return curr_idx + 1

def solve():
  inp = load_input()

  ins = parse(inp)

  mem = {
      'a': 0,
      'b': 0,
      'c': 0,
      'd': 0,
  }

  idx = 0

  while idx < len(ins):
      idx = exec(mem, ins[idx], idx)

  part1 = mem['a']

  mem = {
      'a': 0,
      'b': 0,
      'c': 1,
      'd': 0,
  }

  idx = 0

  while idx < len(ins):
      idx = exec(mem, ins[idx], idx)

  part2 = mem['a']

  print(f"{part1 = }\n{part2 = }")

if __name__ == "__main__":
  t1 = perf_counter()
  solve()
  t2 = perf_counter()
  print(f"Execution time: {t2 - t1:0.4f} seconds")
