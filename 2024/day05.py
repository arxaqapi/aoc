from pathlib import Path
from dataclasses import dataclass
from functools import reduce, partial
from collections import defaultdict


def load(file: Path = Path("data/05.txt")) -> None:
    with file.open("r") as f:
        orders, updates = f.read().strip().split("\n\n")

    return [tuple(map(int, order.split("|"))) for order in orders.split()], [
        tuple(map(int, update.split(","))) for update in updates.split()
    ]


def mk_map(orders: list[tuple[int, int]]):
    """map from x to list of []"""
    d = defaultdict(list)
    for x, y in orders:
        d[x].append(y)
    return d


def check_line(update: tuple[int], d) -> bool:
    """given an update tuple of ints, check that each element appears in the correct order
    if yes, then return True and the middle value of the update tuple.

    """
    # x|y -> x before
    # d: map[x, [y,...]]
    for i, e in enumerate(update):
        for next in update[i + 1 :]:
            if next not in d[e]:
                return False
    return True


def solve_part_1(orders, updates, log: bool = False):
    d = mk_map(orders)
    if log:
        print(d)

    total = 0
    for update in updates:
        if check_line(update, d):
            total += update[len(update) // 2]

            if log:
                print(f"  {update[len(update) // 2]}  -- {update}")

    return total


def reorder(update: tuple[int], d: dict[int, list[int]]) -> tuple[int]:
    # get subdict of elements in update list
    # constrain list in the mapping to the elements in the update list
    # sort by number of elements in the mappings value (decr)
    new_map = {}
    for e in update:
        new_map[e] = [el for el in d[e] if el in update]

    corrected_updates = [
        k
        for k, _ in sorted(new_map.items(), key=lambda item: len(item[1]), reverse=True)
    ]
    return corrected_updates


def solve_part_2(orders, updates, log: bool = False):
    """Takes only the incorrecly ordered ones, correct the order and sum the middle elements."""
    d = mk_map(orders)
    if log:
        print(d)

    total = 0
    for update in updates:
        if not check_line(update, d):
            new_update = reorder(update, d)
            total += new_update[len(new_update) // 2]

            if log:
                print(f"old: {update} -- new: {new_update}")

    return total


if __name__ == "__main__":
    # 5588
    print(solve_part_1(*load()))
    # 5331
    print(solve_part_2(*load()))

    # print(solve_part_1(*load(Path("data/05.test.txt")), log=True))
    # print(solve_part_2(*load(Path("data/05.test.txt")), log=True))
