#!/usr/bin/env python3

from sys import stdin


count = 0
for line in stdin:
    line = line.split()
    if len(line) == len(set(line)):
        count += 1

print(count)