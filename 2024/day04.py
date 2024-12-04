from pathlib import Path
from dataclasses import dataclass
from functools import reduce, partial


@dataclass
class Position:
    x: int
    y: int


def load(file: Path = Path("data/04.txt")) -> None:
    with file.open("r") as f:
        lines = [line.strip() for line in f.readlines()]

    return lines


def get_available_segments(pos: Position, grid: list[list[str]]):
    """given a position and a grid, return all segments that are of size 4 and valid.
    The segments converge towards the the position `Position`, such that each pattern is computed once."""
    xmax = len(grid)
    ymax = len(grid[0])
    segments: list[list[str]] = []

    # horizontal
    if pos.y >= 3:
        h_l = grid[pos.x][pos.y - 3 : pos.y + 1][::-1]
        segments.append(h_l)
    if (pos.y + 4) <= ymax:
        h_r = grid[pos.x][pos.y : pos.y + 4]
        segments.append(h_r)

    # vertical, up
    if pos.x >= 3:
        v_up = [grid[i][pos.y] for i in range(pos.x - 3, pos.x + 1)][::-1]
        segments.append(v_up)
    # vertical, down
    if (pos.x + 4) <= xmax:
        v_down = [grid[i][pos.y] for i in range(pos.x, pos.x + 4)]
        segments.append(v_down)

    # diagonal, up-left
    if pos.x >= 3 and pos.y >= 3:
        d_up_l = [grid[pos.x - i][pos.y - i] for i in range(4)]
        segments.append(d_up_l)

    # diagonal, up-right
    if pos.x >= 3 and (pos.y + 4) <= ymax:
        d_up_r = [grid[pos.x - i][pos.y + i] for i in range(4)]
        segments.append(d_up_r)

    # diagonal, down-left
    if (pos.x + 4) <= xmax and pos.y >= 3:
        d_down_l = [grid[pos.x + i][pos.y - i] for i in range(4)]
        segments.append(d_down_l)

    # diagonal, down-right
    if (pos.x + 4) <= xmax and (pos.y + 4) <= ymax:
        d_down_r = [grid[pos.x + i][pos.y + i] for i in range(4)]
        segments.append(d_down_r)

    return segments


def is_pattern(
    segment: list[str], pattern: str = "XMAS", reverse: bool = False
) -> bool:
    """given a segment, return if it is XMAS, regardless of rotation."""
    if reverse:
        return "".join(segment) == pattern or "".join(segment[::-1]) == pattern
    return "".join(segment) == pattern


is_pattern_mas = partial(is_pattern, pattern="MAS", reverse=True)


def get_diags(pos: Position, grid: list[list[str]]):
    """given a position and a grid, return all diagonals of size 3."""
    xmax = len(grid)
    ymax = len(grid[0])
    # grid[pos.x][pos.y]
    segments: list[list[str]] = []

    # left-up to down right
    if 1 <= pos.x < (xmax - 1) and 1 <= pos.y < (ymax - 1):
        d_1 = [grid[pos.x + i][pos.y + i] for i in (-1, 0, 1)]
        # (x+1 | y-1), 0, (x - 1, y + 1)
        d_2 = [grid[pos.x - i][pos.y + i] for i in (-1, 0, 1)]
        segments.append(d_1)
        segments.append(d_2)
    return segments


def solve_part_1(grid: list[list[str]], log: bool = False):
    # gen all positions
    #
    total = 0
    for x in range(len(grid)):
        for y in range(len(grid[0])):
            segments = get_available_segments(Position(x, y), grid)
            if log:
                print(f"{x},{y}", "> ", segments)

            total += reduce(
                lambda b, e: b + 1 if e else b, map(is_pattern, segments), 0
            )
    return total


def solve_part_2(grid: list[list[str]], log: bool = False):
    total = 0
    for x in range(len(grid)):
        for y in range(len(grid[0])):
            segments = get_diags(Position(x, y), grid)
            if log:
                print(f"{x},{y}", "> ", segments)
                print(reduce(lambda b, e: b and e, map(is_pattern_mas, segments), True))

            # handle empty segments list
            if (
                reduce(lambda b, e: b and e, map(is_pattern_mas, segments), True)
                and len(segments) > 0
            ):
                total += 1
    return total


if __name__ == "__main__":
    # 2567
    print(solve_part_1(load()))
    # 2029
    print(solve_part_2(load()))

    # print(
    #     solve_part_2(
    #         grid=[
    #             # list("MMMSXXMASM"),
    #             # list("MSAMXMSMSA"),
    #             # list("AMXSXMAAMM"),
    #             # list("MSAMASMSMX"),
    #             # list("XMASAMXAMM"),
    #             # list("XXAMMXXAMA"),
    #             # list("SMSMSASXSS"),
    #             # list("SAXAMASAAA"),
    #             # list("MAMMMXMMMM"),
    #             # list("MXMXAXMASX"),
    #             # ==================
    #             list("....XXMAS."),
    #             list(".SAMXMS..."),
    #             list("...S..A..."),
    #             list("..A.A.MS.X"),
    #             list("XMASAMX.MM"),
    #             list("X.....XA.A"),
    #             list("S.S.S.S.SS"),
    #             list(".A.A.A.A.A"),
    #             list("..M.M.M.MM"),
    #             list(".X.X.XMASX"),
    #             # ==================
    #             # list("S  S  S"),
    #             # list(" A A A "),
    #             # list("  MMM  "),
    #             # list("SAMXMAS"),
    #             # list("  MMM  "),
    #             # list(" A A A "),
    #             # list("S  S  S"),
    #             # ==================
    #             # list("A A A"),
    #             # list(" MMM "),
    #             # list("AMXMA"),
    #             # list(" MMM "),
    #             # list("A A A"),
    #             # ==================
    #             # list("..X..."),
    #             # list(".SAMX."),
    #             # list(".A..A."),
    #             # list("XMAS.S"),
    #             # list(".X...."),
    #         ],
    #         log=False,
    #     )
    # )
