from collections import defaultdict
from collections import Counter
from pathlib import Path


def load(file: Path = Path("data/01.txt")) -> tuple[list[int], list[int]]:
    l, r = [], []
    with file.open("r") as f:
        for line in f.readlines():
            a, b = line.split()
            l.append(int(a))
            r.append(int(b))
    return l, r


def solve_part_1(l: list[int], r: list[int]):
    l.sort()
    r.sort()

    total = 0
    for lv, rv in zip(l, r):
        total += abs(lv - rv)
    return total


def solve_part_2(l: list[int], r: list[int]):
    count = defaultdict(int)
    # count r list elements
    for e in r:
        count[e] += 1

    total = 0
    for e in l:
        total += e * count[e]

    return total


if __name__ == "__main__":
    # 1151792
    res_1 = solve_part_1(*load())
    # 21790168
    res_2 = solve_part_2(*load())

    print(res_1)
    print(res_2)