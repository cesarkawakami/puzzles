#!/usr/bin/env python3

import sys

checksum = 0
for line in sys.stdin:
    line = map(int, line.split())
    for i, vi in enumerate(line):
        for j, vj in enumerate(line):
            if i != j and vi % vj == 0:
                checksum += vi // vj

print(checksum)