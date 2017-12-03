#!/usr/bin/env python3

import sys

inp = sys.stdin.read().strip()
print(inp)
solution = 0
for a, b in zip(inp, inp[1:] + inp[0]):
    if a == b:
        solution += int(a)
print(solution)