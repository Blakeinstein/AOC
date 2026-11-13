from enum import Enum
from time import perf_counter

INPUT_FILE = "input/2016/day21.txt"

class Op(Enum):
    SP = "swap position"
    SL = "swap letter"
    RL = "rotate left"
    RR = "rotate right"
    RB = "rotate based"
    RP = "reverse positions"
    MP = "move position"

def from_str(label: str) -> Op:
    match(label):
        case "swap position":
            return Op.SP
        case "swap letter":
            return Op.SL
        case "rotate left":
            return Op.RL
        case "rotate right":
            return Op.RR
        case "rotate based":
            return Op.RB
        case "reverse positions":
            return Op.RP
        case "move position":
            return Op.MP
        case _:
            raise ValueError("Not defined: " + label)

type Ins = tuple[Op, list[str]]

def parse_ops(op: Op, parts: list[str]) -> list[str]:
    match(op):
        case Op.SP | Op.SL | Op.MP:
            return [parts[2], parts[5]]
        case Op.RL | Op.RR:
            return [parts[2]]
        case Op.RB:
            return [parts[6]]
        case Op.RP:
            return [parts[2], parts[4]]
        case _:
            raise ValueError("Invalid operand: " + " ".join(parts))

def parse_instruction(line: str) -> Ins:
    parts = line.split(" ")

    op = from_str(f"{parts[0]} {parts[1]}")

    operands = parse_ops(op, parts)

    return (op, operands)


def load_input() -> list[Ins]:
    data = open(INPUT_FILE, mode="rt").read()
    return list(map(parse_instruction, data.strip().splitlines()))

def rotate(src: list[str], n: int) -> list[str]:
    n = n % len(src)
    return src[n:] + src[:n]

def apply_ins(src: list[str], ins: Ins) -> list[str]:
    match ins[0]:
        case Op.SP:
            a, b = int(ins[1][0]), int(ins[1][1])
            src[b], src[a] = src[a], src[b]
        case Op.SL:
            a, b = ins[1]
            for i in range(len(src)):
                if src[i] == a:
                    src[i] = b
                elif src[i] == b:
                    src[i] = a
        case Op.RL:
            return rotate(src, int(ins[1][0]))
        case Op.RR:
            return rotate(src, -int(ins[1][0]))
        case Op.RB:
            idx = src.index(ins[1][0]) + 1
            if idx > 4:
                idx += 1
            return rotate(src, -idx)
        case Op.RP:
            idx1 = int(ins[1][0])
            idx2 = int(ins[1][1]) + 1
            return src[:idx1] + src[idx1:idx2][::-1] + src[idx2:]
        case Op.MP:
            a, b = int(ins[1][0]),int(ins[1][1])
            if b > a:
                return src[:a] + src[a+1:b+1] + [src[a]] + src[b+1:]
            else:
                return src[:b] + [src[a]] + src[b:a] + src[a+1:]

    return src

def apply_ins_inv(src: list[str], ins: Ins) -> list[str]:
    match ins[0]:
        case Op.SP:
            a, b = int(ins[1][0]), int(ins[1][1])
            src[b], src[a] = src[a], src[b]
        case Op.SL:
            a, b = ins[1]
            for i in range(len(src)):
                if src[i] == a:
                    src[i] = b
                elif src[i] == b:
                    src[i] = a
        case Op.RL:
            return rotate(src, -int(ins[1][0]))
        case Op.RR:
            return rotate(src, int(ins[1][0]))

        # New Stuff
        case Op.RB:
            nxt = [*src]

            for i in range(len(src)):
                nxt = rotate(nxt, 1)

                idx = nxt.index(ins[1][0]) + 1
                if idx > 4:
                    idx += 1
                print(nxt, rotate(nxt, -idx))
                if src == rotate(nxt, -idx):
                    return nxt

            raise ValueError("No rotation possible")
        case Op.RP:
            idx1 = int(ins[1][0])
            idx2 = int(ins[1][1]) + 1
            return src[:idx1] + src[idx1:idx2][::-1] + src[idx2:]
        case Op.MP:
            b, a = int(ins[1][0]),int(ins[1][1])
            if b > a:
                return src[:a] + src[a+1:b+1] + [src[a]] + src[b+1:]
            else:
                return src[:b] + [src[a]] + src[b:a] + src[a+1:]

    return src



def scramble(inp: list[Ins], src_str: str) -> str:
    src = list(src_str)
    for ins in inp:
        src = apply_ins(src, ins)

    return "".join(src)

def unscramble(inp: list[Ins], src_str: str) -> str:
    src = list(src_str)

    for ins in reversed(inp):
        print(''.join(src))
        print(ins)
        src = apply_ins_inv(src, ins)

    return "".join(src)

def solve():
    inp = load_input()

    part1_inp = "abcdefgh"
    part1 = scramble(inp,part1_inp)
    print(f"{part1 = }")

    part1_rev = unscramble(inp, part1)
    assert part1_rev == part1_inp

    part2 = unscramble(inp, "fbgdceah")
    print(f"{part2 = }")


if __name__ == "__main__":
    t1 = perf_counter()
    solve()
    t2 = perf_counter()
    print(f"Execution time: {t2 - t1:0.4f} seconds")
