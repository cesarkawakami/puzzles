#!/usr/bin/env python3

from enum import Enum
from sys import stdin
from typing import List, Tuple


class Dir(Enum):
    UP = (-1, 0)
    RIGHT = (0, 1)
    DOWN = (1, 0)
    LEFT = (0, -1)

    def move(self, pos: Tuple[int, int]) -> Tuple[int, int]:
        return pos[0] + self.value[0], pos[1] + self.value[1]


def idx(grid: List[str], pos: Tuple[int, int]) -> str:
    return grid[pos[0]][pos[1]]


def part1(inp: str) -> None:
    grid = inp.splitlines()
    i, j = 0, grid[0].index('|')
    d = Dir.DOWN
    seen = []
    while True:
        i, j = d.move((i, j))
        if grid[i][j] == '+':
            candidates = []
            if idx(grid, Dir.DOWN.move((i, j))) == '|' and d != Dir.UP:
                candidates.append(Dir.DOWN)
            if idx(grid, Dir.UP.move((i, j))) == '|' and d != Dir.DOWN:
                candidates.append(Dir.UP)
            if idx(grid, Dir.LEFT.move((i, j))) == '-' and d != Dir.RIGHT:
                candidates.append(Dir.LEFT)
            if idx(grid, Dir.RIGHT.move((i, j))) == '-' and d != Dir.LEFT:
                candidates.append(Dir.RIGHT)
            [d] = candidates
        elif grid[i][j] == '|' or grid[i][j] == '-':
            pass
        elif grid[i][j].isalpha():
            seen.append(grid[i][j])
        else:
            print(f'breaking on {grid[i][j]!r}')
            break
    print(f'Part 1: {"".join(seen)}')


def part2(inp: str) -> None:
    grid = inp.splitlines()
    i, j = 0, grid[0].index('|')
    d = Dir.DOWN
    seen = []
    step_count = 0
    while True:
        step_count += 1
        i, j = d.move((i, j))
        if grid[i][j] == '+':
            candidates = []
            if idx(grid, Dir.DOWN.move((i, j))) == '|' and d != Dir.UP:
                candidates.append(Dir.DOWN)
            if idx(grid, Dir.UP.move((i, j))) == '|' and d != Dir.DOWN:
                candidates.append(Dir.UP)
            if idx(grid, Dir.LEFT.move((i, j))) == '-' and d != Dir.RIGHT:
                candidates.append(Dir.LEFT)
            if idx(grid, Dir.RIGHT.move((i, j))) == '-' and d != Dir.LEFT:
                candidates.append(Dir.RIGHT)
            [d] = candidates
        elif grid[i][j] == '|' or grid[i][j] == '-':
            pass
        elif grid[i][j].isalpha():
            seen.append(grid[i][j])
        else:
            print(f'breaking on {grid[i][j]!r}')
            break
    print(f'Part 2: {step_count}')


def main() -> None:
    inp = stdin.read()
    part1(inp)
    part2(inp)


if __name__ == '__main__':
    main()
