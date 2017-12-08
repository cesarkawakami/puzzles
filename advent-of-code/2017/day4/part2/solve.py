#!/usr/bin/env python3

from sys import stdin

def sort_word(word):
    return ''.join(sorted(word))

count = 0
for line in stdin:
    line = line.split()
    line = [sort_word(word) for word in line]
    if len(line) == len(set(line)):
        count += 1

print(count)