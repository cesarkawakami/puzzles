#!/usr/bin/env python3

from functools import reduce
from sys import stdin
from typing import List


def rotate_left(lst: List[int], shift: int) -> List[int]:
    return lst[shift:] + lst[:shift]


def rotate_right(lst: List[int], shift: int) -> List[int]:
    return lst[-shift:] + lst[:-shift]


def flip_beginning(lst: List[int], length: int) -> List[int]:
    return list(reversed(lst[:length])) + lst[length:]


def flip(lst: List[int], current_position: int, length: int) -> List[int]:
    lst = rotate_left(lst, current_position)
    lst = flip_beginning(lst, length)
    lst = rotate_right(lst, current_position)
    return lst


def sparse_to_dense(lst: List[int]) -> List[int]:
    assert len(lst) == 256
    rv = []
    for block_index in range(16):
        start_index = block_index * 16
        end_index = (block_index + 1) * 16
        dense_value = reduce(lambda x, y: x ^ y, lst[start_index:end_index])
        rv.append(dense_value)
    return rv


def knot_hash(plain_text: str) -> List[int]:
    total_length = 256
    lst = list(range(total_length))
    current_position = 0
    skip_size = 0
    lengths = [ord(x) for x in plain_text]
    lengths += [17, 31, 73, 47, 23]
    for round_number in range(64):
        for length in lengths:
            lst = flip(lst, current_position, length)
            current_position += length + skip_size
            current_position %= total_length
            skip_size += 1
    lst = sparse_to_dense(lst)
    return lst


def part1(inp: str) -> List[str]:
    inp = inp.strip()
    output = 0
    rows = []
    for i in range(128):
        plain_text = f'{inp}-{i}'
        hash_ = knot_hash(plain_text)
        row = ''.join(f'{x:08b}' for x in hash_)
        rows.append(row)
        # print(row)
        output += sum(1 if x == '1' else 0 for x in row)
    print(f'Part 1: {output}')
    return rows


def flood_fill(table: List[List[int]], group: int, i: int, j: int) -> None:
    table[i][j] = group
    dis, djs = [0, 1, 0, -1], [1, 0, -1, 0]
    for di, dj in zip(dis, djs):
        ni, nj = i + di, j + dj
        if (
            0 <= ni < len(table) and
            0 <= nj < len(table[0])
            and table[ni][nj] == -2
        ):
            flood_fill(table, group, ni, nj)


def part2(rows: List[str]) -> None:
    table = [[{'0': -1, '1': -2}[c] for c in row] for row in rows]
    group_count = 0
    for i, row in enumerate(table):
        for j, _ in enumerate(row):
            if table[i][j] == -2:
                flood_fill(table, group_count, i, j)
                group_count += 1
    print(f'Part 2: {group_count}')


def main() -> None:
    inp = stdin.read()
    rows = part1(inp)
    part2(rows)


if __name__ == '__main__':
    main()
