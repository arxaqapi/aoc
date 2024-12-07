from pathlib import Path
from dataclasses import dataclass

from typing import TypeAlias


@dataclass(frozen=True)
class Position:
    x: int
    y: int

    def __add__(self, obj):
        return Position(self.x + obj.x, self.y + obj.y)


Direction: TypeAlias = Position


def print_new_grid(grid, archive, last_position, extra: list[Position] = []):
    grid[archive[0].x][archive[0].y] = "^"

    for pos in archive[1:]:
        grid[pos.x][pos.y] = "x"

    try:
        grid[last_position.x][last_position.y] = "O"
    except:
        print(f"[log] - overflow {last_position}")

    for e in extra:
        grid[e.x][e.y] = "F"

    with open("debug.txt", "w") as f:
        f.writelines(["".join(line) + "\n" for line in grid])


def rotate_dir_90_cw(pos: Direction) -> Direction:
    # https://en.wikipedia.org/wiki/Rotation_matrix
    return Position(
        x=int(pos.y * 1),
        y=int(pos.x * -1),
    )


def load(file: Path = Path("data/06.txt")) -> None:
    with file.open("r") as f:
        rows = f.read().strip().split("\n")
    return [list(row) for row in rows]


def is_final_pos(grid, current_position: Position, move_directiion: Direction) -> bool:
    new_pos = current_position + move_directiion
    xmax = len(grid)
    ymax = len(grid[0])
    return new_pos.x < 0 or new_pos.y < 0 or new_pos.x >= xmax or new_pos.y >= ymax


def move(
    grid, current_position: Position, move_directiion: Direction
) -> tuple[Position, Direction]:
    # return new_current_position, next move_direction
    np = current_position + move_directiion
    if grid[np.x][np.y] == "#":
        return move(grid, current_position, rotate_dir_90_cw(move_directiion))

    return np, move_directiion


def get_initial_position(grid) -> Position:
    for i in range(len(grid)):
        for j in range(len(grid[0])):
            if grid[i][j] == "^":
                return Position(i, j)


def solve_part_1(grid: list[list[int]], log: bool = False):
    archive = set()
    init_pos = get_initial_position(grid)
    direction = Position(-1, 0)

    new_pos = init_pos + direction

    archive.add(init_pos)
    archive.add(new_pos)

    # NOTE - does not handle the following case:
    #   |v#
    #   |#
    while not is_final_pos(grid, new_pos, direction):
        new_pos, direction = move(grid, new_pos, direction)
        archive.add(new_pos)

    if log:
        print_new_grid(grid, archive, new_pos)
    return len(archive)


if __name__ == "__main__":
    # 5131
    print(solve_part_1(load(), log=True))
