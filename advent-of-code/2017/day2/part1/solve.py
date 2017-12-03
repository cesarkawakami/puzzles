#!/usr/bin/env python3

import sys

checksum = 0
for line in sys.stdin:
    line = map(int, line.split())
    largest, smallest = max(line), min(line)
    difference = largest - smallest
    checksum += difference

print(checksum)