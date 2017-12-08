#!/usr/bin/env python3

from sys import stdin

instructions = [int(x) for x in stdin.read().split()]

current_position = 0
step_count = 0
while True:
    step_count += 1

    # print(' '.join(str(x) if current_position != i else f'({x})' for i, x in enumerate(instructions)))

    next_position = current_position + instructions[current_position]
    if instructions[current_position] >= 3:
        instructions[current_position] -= 1
    else:
        instructions[current_position] += 1

    if next_position < 0 or next_position >= len(instructions):
        break
    current_position = next_position

# print(' '.join(str(x) for x in instructions))

print(step_count)