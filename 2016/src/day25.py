from dataclasses import dataclass
from enum import Enum

from itertools import islice

from time import perf_counter

INPUT_FILE = "input/2016/day25.txt"


class Op(Enum):
    CPY = "cpy"
    INC = "inc"
    DEC = "dec"
    JNZ = "jnz"
    OUT = "out"


@dataclass
class Instruction:
    op: Op
    op1: str | int
    op2: str | int | None


def parse_line(line: str) -> Instruction:
    parts = line.split(" ")
    return Instruction(
        Op[parts[0].upper()], parts[1], parts[2] if len(parts) > 2 else None
    )


def parse(inp: str) -> list[Instruction]:
    return list(map(parse_line, inp.splitlines()))


def program(reg_a: int, ins_set: list[Instruction]):
    mem = {
        "a": reg_a,
        "b": 0,
        "c": 0,
        "d": 0,
    }

    idx = 0

    while True:
        ins = ins_set[idx]
        match ins.op:
            case Op.CPY:
                val = mem[ins.op1] if ins.op1 in mem else int(ins.op1)
                mem[str(ins.op2)] = val
                idx += 1

            case Op.INC:
                mem[str(ins.op1)] += 1
                idx += 1

            case Op.DEC:
                mem[str(ins.op1)] -= 1
                idx += 1

            case Op.JNZ:
                key = str(ins.op1)
                val = mem[key] if key in mem else int(key)
                idx += int(ins.op2) if val != 0 else 1

            case Op.OUT:
                key = str(ins.op1)
                idx += 1
                yield (mem[key] if key in mem else int(key))

def load_input():
    data = open(INPUT_FILE, mode="rt").read()
    return data


def solve():
    inp = load_input()

    ins = parse(inp)
    part1 = 0
    while part1 < 1000:
        itr = program(part1, ins)

        s = ''.join(str(itr_s) for itr_s in islice(itr, 20))

        if s.startswith('01'*10):
            break


        part1 += 1

    print(f"{part1 = }")


if __name__ == "__main__":
    t1 = perf_counter()
    solve()
    t2 = perf_counter()
    print(f"Execution time: {t2 - t1:0.4f} seconds")
