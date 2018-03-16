#!/usr/bin/env python3

from typing import List, Tuple


def rotate_right(s: List[int], amount: int) -> List[int]:
    return s[-amount:] + s[:-amount]


def swap_by_pos(s: List[int], a: int, b: int) -> List[int]:
    rv = list(s)
    rv[a], rv[b] = rv[b], rv[a]
    return rv


def apply_perm_pos(s: List[int], perm_pos: List[int]) -> List[int]:
    return [s[c] for c in perm_pos]


def apply_perm_label(s: List[int], perm_label: List[int]) -> List[int]:
    px = [perm_label.index(x) for x in range(16)]
    mapping = {a: b for a, b in zip(range(16), px)}
    return [mapping[c] for c in s]


def get_permutation(inp: str) -> Tuple[List[int], List[int]]:
    perm_pos = list(range(16))
    perm_label = list(range(16))
    moves = inp.strip().split(',')
    for move in moves:
        if move.startswith('s'):
            amount = int(move[1:])
            perm_pos = rotate_right(perm_pos, amount)
        elif move.startswith('x'):
            pa, pb = map(int, move[1:].split('/'))
            perm_pos = swap_by_pos(perm_pos, pa, pb)
        elif move.startswith('p'):
            ca, cb = move[1:].split('/')
            pa, pb = [ord(x) - ord('a') for x in [ca, cb]]
            perm_label = swap_by_pos(perm_label, pa, pb)
        else:
            raise RuntimeError('unreachable?')
    return perm_pos, perm_label


def perm_to_matrix(s: List[int]) -> List[List[int]]:
    rv = [[0 for _ in range(16)] for _ in range(16)]
    for i, v in enumerate(s):
        rv[v][i] = 1
    return rv


def matrix_to_perm(m: List[List[int]]) -> List[int]:
    rv = [0 for _ in range(16)]
    for i in range(16):
        for j in range(16):
            if m[i][j]:
                rv[j] = i
    return rv


def vector_matrix_multiply(a: List[int], b: List[List[int]]) -> List[int]:
    return matrix_matrix_multiply([a], b)[0]


def matrix_matrix_multiply(
    a: List[List[int]],
    b: List[List[int]],
) -> List[List[int]]:
    rv = [[0 for _ in range(16)] for _ in range(16)]
    for i in range(16):
        for j in range(16):
            for k in range(16):
                rv[i][j] += a[i][k] * b[k][j]
    return rv


def matrix_power(m: List[List[int]], n: int) -> List[List[int]]:
    if n == 1:
        return m
    elif n <= 0:
        raise NotImplementedError()
    elif n % 2 == 1:
        return matrix_matrix_multiply(matrix_power(m, n - 1), m)
    else:
        sub_m = matrix_power(m, n // 2)
        return matrix_matrix_multiply(sub_m, sub_m)


def to_str(s: List[int]) -> str:
    return ''.join(chr(x + ord('a')) for x in s)


def part1(inp: str) -> None:
    perm_pos, perm_label = get_permutation(inp)
    s = list(range(16))
    s = apply_perm_pos(s, perm_pos)
    s = apply_perm_label(s, perm_label)
    print(f'Part 1: {to_str(s)}')


def part2(inp: str) -> None:
    N = 1000000000
    perm_pos, perm_label = get_permutation(inp)
    perm_pos_m = perm_to_matrix(perm_pos)
    perm_label_m = perm_to_matrix(perm_label)
    final_perm_pos_m = matrix_power(perm_pos_m, N)
    final_perm_label_m = matrix_power(perm_label_m, N)
    final_perm_pos = matrix_to_perm(final_perm_pos_m)
    final_perm_label = matrix_to_perm(final_perm_label_m)
    s = list(range(16))
    s = apply_perm_pos(s, final_perm_pos)
    s = apply_perm_label(s, final_perm_label)
    print(f'Part 2: {to_str(s)}')


def main() -> None:
    # inp = stdin.read()
    inp = open('input').read()
    part1(inp)
    part2(inp)


if __name__ == '__main__':
    main()
