from functools import reduce
from sys import argv, stdin
from typing import List


def rotate_left(lst: List[int], shift: int) -> List[int]:
    return lst[shift:] + lst[:shift]


def rotate_right(lst: List[int], shift: int) -> List[int]:
    return lst[-shift:] + lst[:-shift]


def flip_beginning(lst: List[int], length: int) -> List[int]:
    return list(reversed(lst[:length])) + lst[length:]


def flip(lst: List[int], current_position: int, length: int) -> List[int]:
    lst = rotate_left(lst, current_position)
    lst = flip_beginning(lst, length)
    lst = rotate_right(lst, current_position)
    return lst


def sparse_to_dense(lst: List[int]) -> List[int]:
    assert len(lst) == 256
    rv = []
    for block_index in range(16):
        start_index = block_index * 16
        end_index = (block_index + 1) * 16
        dense_value = reduce(lambda x, y: x ^ y, lst[start_index:end_index])
        rv.append(dense_value)
    return rv


def knot_hash_repr(lst: List[int]) -> str:
    return ''.join(f'{x:02x}' for x in lst)


def main() -> None:
    total_length = 256
    lst = list(range(total_length))
    current_position = 0
    skip_size = 0
    lengths = [ord(x) for x in stdin.read().strip()]
    lengths += [17, 31, 73, 47, 23]
    for round_number in range(64):
        for length in lengths:
            lst = flip(lst, current_position, length)
            current_position += length + skip_size
            current_position %= total_length
            skip_size += 1
    lst = sparse_to_dense(lst)
    print(knot_hash_repr(lst))


if __name__ == '__main__':
    main()
