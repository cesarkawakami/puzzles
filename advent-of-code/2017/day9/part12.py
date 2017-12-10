#!/usr/bin/env python3

from enum import Enum, auto
from sys import stdin
from typing import Tuple


class GarbageStates(Enum):
    BASE = auto()
    GARBAGE = auto()
    GARBAGE_CANCELLING = auto()


def parse(stream: str) -> Tuple[int, int]:
    state = GarbageStates.BASE
    level = 0
    value_count = 0
    garbage_count = 0

    for c in stream:
        if state == GarbageStates.BASE:
            if c == '<':
                state = GarbageStates.GARBAGE
            elif c == '{':
                level += 1
            elif c == '}':
                value_count += level
                level -= 1
            else:
                pass
        elif state == GarbageStates.GARBAGE:
            if c == '>':
                state = GarbageStates.BASE
            elif c == '!':
                state = GarbageStates.GARBAGE_CANCELLING
            else:
                garbage_count += 1
        elif state == GarbageStates.GARBAGE_CANCELLING:
            state = GarbageStates.GARBAGE
        else:
            raise RuntimeError('unreachable')

    return value_count, garbage_count


stream = stdin.read().strip()
value_count, garbage_count = parse(stream)
print(f'Value count is: {value_count}')
print(f'Garbage count: {garbage_count}')
