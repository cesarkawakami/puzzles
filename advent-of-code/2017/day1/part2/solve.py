#!/usr/bin/env python3

import sys

inp = sys.stdin.read().strip()
print(inp)
solution = 0
shift_amount = len(inp) // 2
for a, b in zip(inp, inp[shift_amount:] + inp[:shift_amount]):
    if a == b:
        solution += int(a)
print(solution)