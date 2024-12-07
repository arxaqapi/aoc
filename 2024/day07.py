from pathlib import Path
from dataclasses import dataclass
from itertools import product, zip_longest


def load(file: Path = Path("data/07.txt")) -> None:
    d = {}
    with file.open("r") as f:
        for line in f.readlines():
            res, n = line.split(":")
            d[int(res)] = tuple(n.split())
    return d


def all_possible_solution(values: tuple[str], operators: str = "+*"):
    op_list = list(product(operators, repeat=len(values) - 1))
    for ops in op_list:
        s = tuple(filter(None, sum(zip_longest(values, ops), ())))
        # print(s)
        sub_tot = eval(" ".join(s[:3]))
        for i in range(2, len(s) - 2, 2):
            sub_tot = eval(" ".join((str(sub_tot),) + s[i + 1 : i + 3]))
        yield sub_tot


def all_possible_solution_p2(values: tuple[str], operators: str = "+*|"):
    op_list = list(product(operators, repeat=len(values) - 1))
    for ops in op_list:
        s = tuple(filter(None, sum(zip_longest(values, ops), ())))
        sub_tot = int(s[0] + s[2]) if s[1] == "|" else eval(" ".join(s[:3]))
        for i in range(2, len(s) - 2, 2):
            if s[i + 1] == "|":
                # NOTE concat 'a || b' as 'ab'
                sub_tot = int(str(sub_tot) + s[i + 2])
            else:
                sub_tot = eval(" ".join((str(sub_tot),) + s[i + 1 : i + 3]))
        yield sub_tot


def solve_part_1(d: dict[int, tuple[str]], log: bool = False):
    total = 0
    for k, values in d.items():
        solutions = list(all_possible_solution(values))
        if k in solutions:
            total += k
            if log:
                print(f"[log] - {k=}: {solutions.count(k)}")
    return total


def solve_part_2(d: dict[int, tuple[str]], log: bool = False):
    total = 0
    for target, values in d.items():
        for pos_sol in all_possible_solution_p2(values):
            if target == pos_sol:
                total += target
                break
    return total


if __name__ == "__main__":
    # permutations can be cached for faster computation 

    # 5512534574980
    print(solve_part_1(load()))
    # 328790210468594
    print(solve_part_2(load()))

    # print(solve_part_1(load(Path("data/07.test.txt"))))
    # print(solve_part_2(load(Path("data/07.test.txt"))))
