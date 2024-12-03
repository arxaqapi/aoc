"""
mul(x,y) -> x*y (1-3 digit)
invalid characters do nothing (even spaces)
"""

from pathlib import Path
import re


def load(file: Path = Path("data/03.txt")) -> None:
    with file.open("r") as f:
        line = f.read()
    return line.strip()


def parse(mul: str):
    a, b = mul[4:-1].split(",")
    return int(a), int(b)


def solve_part_1(line: str):
    out = re.findall(r"mul\([0-9]{1,3},[0-9]{1,3}\)", line)

    total = 0
    for s in out:
        a, b = parse(s)
        total += a * b
    return total


def solve_part_2(line: str):
    out = re.findall(r"(mul\([0-9]{1,3},[0-9]{1,3}\))|(do\(\))|(don't\(\))", line)
    # out[0]: ('mul(565,609)', '', '') | ('', 'do()', '') | ('', '', "don't()")

    do = True
    total = 0
    for s in out:
        if s[1] == "do()":
            do = True
        elif s[2] == "don't()":
            do = False
        else:
            if do:
                a, b = parse(s[0])
                total += a * b
    return total


if __name__ == "__main__":
    out = load()

    # 163931492
    print(solve_part_1(out))
    # 76911921
    print(solve_part_2(out))
