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


def solve_p_1_small(l: list[int], r: list[int]):
    return sum(abs(a - b) for a, b in zip(sorted(l), sorted(r)))


def solve_part_2(l: list[int], r: list[int]):
    count = defaultdict(int)
    # count r list elements
    for e in r:
        count[e] += 1

    total = 0
    for e in l:
        total += e * count[e]

    return total


def solve_p_2_small(l: list[int], r: list[int]):
    return sum(x * r.count(x) for x in l)


if __name__ == "__main__":
    # 1151792
    res_1 = solve_part_1(*load())
    # 21790168
    res_2 = solve_part_2(*load())

    print(res_1)
    print(solve_p_1_small(*load()))
    print(res_2)
    print(solve_p_2_small(*load()))
