from collections import defaultdict
from dataclasses import dataclass
from enum import Enum

import numpy as np
import pandas as pd

from time import perf_counter

INPUT_FILE = "input/2016/day23.txt"


class Op(Enum):
    CPY = "cpy"
    INC = "inc"
    DEC = "dec"
    JNZ = "jnz"
    TGL = "tgl"


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


def exec(mem: list[dict[str, int]], ins_set: list[Instruction], curr_idx: int, optimize = False) -> int:
    ins = ins_set[curr_idx]
    match ins.op:
        case Op.CPY:

            if ins.op2 is not None and type(ins.op2) == str and not ins.op2.isdigit():
                val = mem[-1][ins.op1] if ins.op1 in mem[-1] else int(ins.op1)
                mem[-1][str(ins.op2)] = val

        case Op.INC:
            mem[-1][str(ins.op1)] += 1

        case Op.DEC:
            mem[-1][str(ins.op1)] -= 1

        case Op.JNZ:
            key = str(ins.op1)
            val = mem[-1][key] if key in mem[-1] else int(ins.op1)

            if val != 0 and ins.op2 is not None:
                target = int(mem[-1][ins.op2]) if ins.op2 in mem[-1] else int(ins.op2)

                if key.isalpha() and target < 0:
                    t_mem = mem[curr_idx]
                    if mem[-1]['ttc'] == t_mem['ttc']:
                        try:
                            reps = val // (t_mem[key] - val)
                            mem[-1]['a'] += (mem[-1]['a']-t_mem['a'])*reps
                            mem[-1]['b'] += (mem[-1]['b']-t_mem['b'])*reps
                            mem[-1]['c'] += (mem[-1]['c']-t_mem['c'])*reps
                            mem[-1]['d'] += (mem[-1]['d']-t_mem['d'])*reps
                            target = 1
                            mem[curr_idx] = {**mem[-1]}
                            mem[curr_idx]['ttc'] = -1
                        except ZeroDivisionError:
                            mem[curr_idx] = {**mem[-1]}
                    else:
                        mem[curr_idx] = {**mem[-1]}

                return curr_idx + (target or 1)

        case Op.TGL:
            key = str(ins.op1)
            val = curr_idx + mem[-1][key]
            if val >= 0 and val < len(ins_set):
                rel_ins = ins_set[val]
                if rel_ins.op2:
                    new_ins = Op.CPY if rel_ins.op == Op.JNZ else Op.JNZ
                    ins_set[val] = Instruction(new_ins, rel_ins.op1, rel_ins.op2)
                else:
                    new_ins = Op.DEC if rel_ins.op == Op.INC else Op.INC

                    ins_set[val] = Instruction(new_ins, rel_ins.op1, None)

    return curr_idx + 1


def load_input():
    data = open(INPUT_FILE, mode="rt").read()
    return data



def solve():
    inp = load_input()

    ins = parse(inp)

    mem = [{
        "a": 7,
        "b": 0,
        "c": 0,
        "d": 0,
        "ttc": -1,
    } for _ in range(len(ins ) + 1)]

    mem[-1]['ttc'] = 0

    idx = 0

    while idx < len(ins):
        idx = exec(mem, ins, idx)

    part1 = mem[-1]["a"]
    print(f"{part1 = }")

    ins = parse(inp)

    mem = [{
        "a": 12,
        "b": 0,
        "c": 0,
        "d": 0,
        "ttc": -1,
    } for _ in range(len(ins) + 1)]

    mem[-1]['ttc'] = 0

    idx = 0
    while idx < len(ins):
        idx = exec(mem, ins, idx, True)

    part2 = mem[-1]['a']
    print(f"{part2 = }")


if __name__ == "__main__":
    t1 = perf_counter()
    solve()
    t2 = perf_counter()
    print(f"Execution time: {t2 - t1:0.4f} seconds")
