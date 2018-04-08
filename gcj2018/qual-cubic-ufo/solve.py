#!/usr/bin/env python3

from sys import stderr
from itertools import product, combinations
from math import pi, sin, cos, asin, sqrt, atan2


def area1(a):
    return cos(a) + sin(a)


def area2(b):
    return sqrt(2) * cos(b) + sin(b)


def bsearch(left, right, f, target):
    for iteration in range(1000):
        mid = (left + right) / 2
        if target <= f(mid):
            right = mid
        else:
            left = mid
    return left


def faces_for_ab(a, b):
    return (
        (-1/2*sin(a), 1/2*cos(a)*cos(b), 1/2*cos(a)*sin(b)),
        (1/2*cos(a), 1/2*sin(a)*cos(b), 1/2*sin(a)*sin(b)),
        (0, -1/2*sin(b), 1/2*cos(b)),
    )


def main():
    case_count = int(input())
    for case_number in range(1, case_count + 1):
        print('Case #{}:'.format(case_number))
        target = float(input())
        if target <= sqrt(2):
            a = bsearch(0, pi/4, area1, target)
            b = 0
        else:
            a = pi/4
            b = bsearch(0, asin(1 / sqrt(3)), area2, target)
        faces = faces_for_ab(a, b)
        print('{} {} {}\n{} {} {}\n{} {} {}'.format(*faces[0], *faces[1], *faces[2]))


if __name__ == '__main__':
    main()
