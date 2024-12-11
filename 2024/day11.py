from pathlib import Path


def load(file: Path = Path("data/11.txt")) -> None:
    with file.open("r") as f:
        line = f.readline().strip().split()
    return list(map(int, line))


def resolve_rule(num: int):
    if num == 0:
        return (1,)
    elif len(str(num)) % 2 == 0:
        n = str(num)
        return (int(n[: len(n) // 2]), int(n[len(n) // 2 :]))
    else:
        return (num * 2024,)


def solve_part_1(line: list[int], times: int = 25, log: bool = False):
    nl = []
    for _ in range(times):
        for e in line:
            nl.extend(resolve_rule(e))

        if log:
            print(f"i: {_}: {nl}")

        line = nl[::]
        nl = []
    return len(line)


def dfs(edges: list[int], d: int, mem: dict = {}):
    if d == 0:
        return len(edges)
    total = 0
    for e in edges:
        if (e, d) in mem:
            total += mem[(e, d)]
        else:
            val = dfs(resolve_rule(e), d - 1, mem)
            mem[(e, d)] = val
            total += val
    return total


def solve_part_2(line: list[int], times: int = 75, log: bool = False):
    return dfs(line, times)


if __name__ == "__main__":
    # 239714
    print(solve_part_1(load()))
    # 284973560658514
    print(solve_part_2(load()))

    # print(solve_part_1(load(Path("data/11.test.txt")), 25))
    # print(solve_part_2(load(Path("data/11.test.txt")), 25))
