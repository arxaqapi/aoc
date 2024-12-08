from pathlib import Path
from collections import defaultdict
from itertools import combinations


def load(file: Path = Path("data/08.txt")) -> None:
    grid = []
    with file.open("r") as f:
        for line in f.readlines():
            grid.append(list(line.strip()))
    # NOTE - dict of freq to list of 2D locations
    """
    │
    y
    └──x──
    """
    d = defaultdict(list)
    for y in range(len(grid)):
        for x in range(len(grid[0])):
            if grid[y][x] != ".":
                d[grid[y][x]].append((x, len(grid) - y - 1))
    return grid, d


def write(grid, new_points):
    for point in new_points:
        x, y = point
        x, y = len(grid) - y - 1, x
        if grid[x][y] == ".":
            grid[x][y] = "#"
        # else:
        #     grid[x][y] = "X"

    with open("debug.txt", "w") as f:
        f.writelines(["".join(line) + "\n" for line in grid])


def line_eq(p1: tuple[int, int], p2: tuple[int, int]):
    x1, y1 = p1
    x2, y2 = p2
    diff_x = x2 - x1
    diff_y = y2 - y1
    slope = diff_y / diff_x
    f = lambda x: int(slope * (x - x1) + y1)
    return f


def solve_part_1(grid, d: dict[str, list[tuple[int, int]]], log: bool = False):
    """
    single lowercase, uppercase letter or digit = frequency
    - get a line between 2 freqs, extend and put down antennas (skip outside of bounds)
    - antennas can be antinodes (no overwrite but count)
    - count possible antinodes, within the bounds of the map

    Note: there are no straight lines, simplifying the x,y search from f(x)
    """
    grid_max_x = len(grid[0])
    grid_max_y = len(grid)
    # for each pair of locations, create a y line function f(x)=y,
    # get distance and get 2 new positions
    new_points = set()
    for freq, locations in d.items():
        if log:
            print(f"{freq}: {locations}")
        for p1, p2 in list(combinations(locations, 2)):
            f = line_eq(p1, p2)
            x_min = min(p1[0], p2[0])
            x_max = max(p1[0], p2[0])
            diff_x = x_max - x_min
            # left & right
            new_p1 = x_min - diff_x, f(x_min - diff_x)
            new_p2 = x_max + diff_x, f(x_max + diff_x)
            if log:
                print(f"  >  {p1}; {p2}")
                print(f"    >  {new_p1=}")
                print(f"    >  {new_p2=}")
            for np in (new_p1, new_p2):
                if 0 <= np[0] < grid_max_x and 0 <= np[1] < grid_max_y:
                    new_points.add(np)
    if log:
        print("np: ", sorted(new_points))
        write(grid, new_points)
    return len(new_points)


def solve_part_2(grid, d: dict[str, list[tuple[int, int]]], log: bool = False):
    grid_max_x = len(grid[0])
    grid_max_y = len(grid)
    # for each pair of locations, create a y line function f(x)=y,
    # get distance and get 2 new positions
    new_points = set()
    for freq, locations in d.items():
        for p1, p2 in list(combinations(locations, 2)):
            f = line_eq(p1, p2)
            x_min = min(p1[0], p2[0])
            x_max = max(p1[0], p2[0])
            diff_x = x_max - x_min
            # extend until border 0 <= x < grid_max_x
            # and                 0 <= y < grid_max_y
            x_line = [x_min - i * diff_x for i in range(1, x_min // diff_x + 1)]
            x_line += [
                x_max + i * diff_x
                for i in range(1, (grid_max_x - diff_x) // diff_x + 1)
            ]
            for xxx in x_line:
                np = xxx, f(xxx)
                if 0 <= np[0] < grid_max_x and 0 <= np[1] < grid_max_y:
                    new_points.add(np)
            # NOTE - add initial points
            new_points.add(p1)
            new_points.add(p2)
    if log:
        write(grid, new_points)
    return len(new_points)


if __name__ == "__main__":
    grid, d = load()

    # 256
    print(solve_part_1(grid, d))
    # 1005
    print(solve_part_2(grid, d, log=True))

    # print(solve_part_1(*load(Path("data/08.test.txt")), log=True))
    # print(solve_part_2(*load(Path("data/08.test.txt")), log=True))
    # print(solve_part_2(*load(Path("data/08.test2.txt")), log=True))
