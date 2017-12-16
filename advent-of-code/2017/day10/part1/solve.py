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


def main() -> None:
    total_length = int(argv[1])
    lst = list(range(total_length))
    current_position = 0
    skip_size = 0
    lengths = [int(x) for x in stdin.read().strip().split(',')]
    for length in lengths:
        lst = flip(lst, current_position, length)
        current_position += length + skip_size
        current_position %= total_length
        skip_size += 1

    print(lst)
    print(lst[0] * lst[1])


if __name__ == '__main__':
    main()
