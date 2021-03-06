#!/usr/bin/env python3

import sys
import math


# 2   17       16       15       14       13
# 1   18        5 (2)    4 (1)    3 (1)   12
# 0   19        6 (2)    1 (0)    2 (1)   11
# -1  20        7 (2)    8 (2)    9 (2)   10 (3)
# -2  21       22       23     --->      ...
#     x: -2     -1       0         1       2

def square_index(n):
    return math.floor(math.sqrt(float(n - 1)))


def square_size(n):
    return (square_index(n) + 1) // 2 * 2 + 1


def square_starting_position(n):
    k = square_size(n) // 2 
    return (k, -k)


def square_start(n):
    return (square_size(n) - 2) ** 2


def position_in_square(n):
    return n - square_start(n)


def quadrant(n):
    if n == 1:
        return 0
    return (position_in_square(n) - 1) // (square_size(n) - 1)


def quadrant_starting_position(n):
    x, y = square_starting_position(n)
    q = quadrant(n)
    if q == 0:
        return x, y
    elif q == 1:
        return x, -y
    elif q == 2:
        return -x, -y
    elif q == 3:
        return -x, y


def position_in_quadrant(n):
    k = position_in_square(n)
    return k - (square_size(n) - 1) * quadrant(n)


def position(n):
    x, y = quadrant_starting_position(n)
    q = quadrant(n)
    p = position_in_quadrant(n)
    if q == 0:
        return x, y + p
    elif q == 1:
        return x - p, y
    elif q == 2:
        return x, y - p
    elif q == 3:
        return x + p, y


def dist(n):
    x, y = position(n)
    return abs(x) + abs(y)


def main():
    for i in range(1, 25 + 1):
        print(
            i, 
            square_size(i), 
            square_starting_position(i), 
            position_in_square(i), 
            quadrant(i), 
            quadrant_starting_position(i),
            position_in_quadrant(i),
            position(i),
            dist(i),
        )

    print(dist(1024))
    print(dist(265149))


if __name__ == '__main__':
    main()