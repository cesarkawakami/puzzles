#!/usr/bin/env python3

from sys import stdin

DIRECTIONS = {
    's': -1,
    'n': 1,
    'ne': 1j,
    'sw': -1j,
    'se': -1 + 1j,
    'nw': 1 - 1j,
}


def rotate_one(coord: complex) -> complex:
    return (coord.real + coord.imag) + (-coord.real) * (1j)


def rotate(coord: complex) -> complex:
    for _ in range(6):
        print(coord)
        if coord.real >= 0 and coord.imag >= 0:
            return coord
        coord = rotate_one(coord)
    raise RuntimeError('unreachable')


def solve_assuming_positive(coord: complex) -> int:
    return int(coord.real + coord.imag)


def main() -> None:
    moves = stdin.read().strip().split(',')
    deltas = [DIRECTIONS[x] for x in moves]

    current_position = 0j
    max_distance_seen = 0
    for delta in deltas:
        current_position += delta
        current_distance = solve_assuming_positive(rotate(current_position))
        max_distance_seen = max(max_distance_seen, current_distance)

    print(max_distance_seen)


if __name__ == '__main__':
    main()
