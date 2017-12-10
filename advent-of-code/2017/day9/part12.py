#!/usr/bin/env python3

from ast import literal_eval
from enum import Enum, auto
from sys import stdin
from typing import Any, List, Tuple


class GarbageStates(Enum):
    A = auto()
    B = auto()
    C = auto()


def remove_garbage(stream: str) -> Tuple[str, int]:
    current_state = GarbageStates.A
    output = []
    garbage_count = 0

    for c in stream:
        if current_state == GarbageStates.A:
            if c == '<':
                current_state = GarbageStates.B
            else:
                output.append(c)
        elif current_state == GarbageStates.B:
            if c == '>':
                current_state = GarbageStates.A
            elif c == '!':
                current_state = GarbageStates.C
            else:
                garbage_count += 1
        elif current_state == GarbageStates.C:
            current_state = GarbageStates.B
        else:
            raise RuntimeError('unreachable')

    return ''.join(output), garbage_count


def count_value_impl(level: int, data: List[Any]) -> int:
    if not data:
        return level
    return level + sum(count_value_impl(level + 1, x) for x in data)


def remove_empty_elements(stream: str) -> str:
    while True:
        new_stream = stream.replace('{,', '{')
        if new_stream == stream:
            break
        stream = new_stream
    return stream


def convert_empty_dicts_into_sets(stream: str) -> str:
    return stream.replace('{', '[').replace('}', ']')


def count_value(stream: str) -> int:
    stream = remove_empty_elements(stream)
    stream = convert_empty_dicts_into_sets(stream)
    ast = literal_eval(stream)
    return count_value_impl(1, ast)


stream = stdin.read().strip()
stream, garbage_count = remove_garbage(stream)
value = count_value(stream)
print(f'Value is: {value}')
print(f'Garbage count: {garbage_count}')
