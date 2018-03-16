#!/usr/bin/env python3

MOD = 2147483647


def advance(x: int, factor: int, multiple: int) -> int:
    while True:
        x = (x * factor) % MOD
        if x % multiple == 0:
            return x


def part1() -> None:
    N = 40000000
    a, b = 873, 583
    # N = 10
    # a, b = 65, 8921
    match_count = 0
    for i in range(N):
        a = advance(a, 16807, 1)
        b = advance(b, 48271, 1)
        if a % 65536 == b % 65536:
            match_count += 1
    print(f'Part 1: {match_count}')


def part2() -> None:
    N = 5000000
    a, b = 873, 583
    # N = 10
    # a, b = 65, 8921
    match_count = 0
    for i in range(N):
        a = advance(a, 16807, 4)
        b = advance(b, 48271, 8)
        if a % 65536 == b % 65536:
            match_count += 1
    print(f'Part 2: {match_count}')


def main() -> None:
    part1()
    part2()


if __name__ == '__main__':
    main()
