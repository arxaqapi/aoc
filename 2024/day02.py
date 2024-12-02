from itertools import pairwise
from pathlib import Path
from copy import deepcopy


def load(file: Path = Path("data/02.txt")) -> list[list[int]]:
    reports: list[list[int]] = []
    with file.open("r") as f:
        for line in f.readlines():
            reports.append([int(x) for x in line.strip().split()])
    return reports


def is_valid(report: list[int]):
    diffs = [x - y for x, y in pairwise(report)]
    return all(1 <= x <= 3 for x in diffs) or all(-1 >= x >= -3 for x in diffs)


def is_valid_pw(report: list[tuple[int]]):
    diffs = [x - y for x, y in report]
    return all(1 <= x <= 3 for x in diffs) or all(-1 >= x >= -3 for x in diffs)


def solve_part_1(reports: list[list[int]]):
    total = 0
    for report in reports:
        if is_valid(report):
            total += 1
    return total


def gen_comb(report: list[int]):
    reports = []
    for i in range(len(report)):
        l = deepcopy(report)
        l.pop(i)
        reports.append(l)
    return reports


def solve_part_2(reports: list[list[int]]):
    total = 0
    for report in reports:
        val = 0
        for sub_rep in gen_comb(report):
            if is_valid(sub_rep):
                total += 1
                break
    return total


if __name__ == "__main__":
    print(solve_part_1(load()))
    print(solve_part_2(load()))
