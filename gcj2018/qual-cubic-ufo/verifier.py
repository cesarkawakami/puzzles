#!/usr/bin/env python3

from math import atan2, sqrt
from itertools import product, combinations


class Point3:
    def __init__(self, x, y, z):
        self.x, self.y, self.z = x, y, z
    def __add__(self, other):
        return Point3(self.x + other.x, self.y + other.y, self.z + other.z)
    def __rmul__(self, other):
        return Point3(other * self.x, other * self.y, other * self.z)
    def __abs__(self):
        return sqrt(self.x ** 2 + self.y ** 2 + self.z ** 2)
    def cross(self, other):
        return Point3(
            self.y * other.z - self.z * other.y,
            self.z * other.x - self.x * other.z,
            self.x * other.y - self.y * other.x,
        )
    def to_2(self):
        return Point2(self.x, self.z)


class Point2:
    def __init__(self, x, y):
        self.x, self.y = x, y


def convex_area(points):
    def angle_key(p):
        return atan2(p.y, p.x)
    rv = -1
    for size in range(3, len(points)):
        for polygon in combinations(points, size):
            polygon = sorted(polygon, key=angle_key)
            area = 0
            for p, q in zip(polygon, polygon[1:] + polygon[:1]):
                area += p.x * q.y - p.y * q.x
            area = max(area, -area) / 2
            rv = max(rv, area)
    return rv


def aeq(a, b, tol=1e-6):
    return abs(a - b) < tol


# Receives input, then candidate output, concatenated
def main():
    case_count = int(input())
    targets = []
    for _ in range(case_count):
        target = float(input())
        targets.append(target)

    for case_number, target in enumerate(targets, start=1):
        header = input().strip()
        expected = 'Case #{}:'.format(case_number)
        if header != expected:
            raise ValueError('Unexpected header format: {!r}, expected={!r}'.format(header, expected))
        a = Point3(*map(float, input().split()))
        b = Point3(*map(float, input().split()))
        c = Point3(*map(float, input().split()))

        if not aeq(abs(a), 0.5) or not aeq(abs(b), 0.5) or not aeq(abs(c), 0.5):
            raise ValueError('Unexpected norm: {}, {}, {}, expected: 0.5'.format(abs(a), abs(b), abs(c)))
        if not abs(a.cross(b).cross(c)) < 1e-9:
            raise ValueError('Unexpected value of |a x b x c|: {}, expected near zero'.format(abs(a.cross(b).cross(c))))

        vertices = []
        for ma, mb, mc in product([-1, 1], repeat=3):
            vertices.append((ma * a + mb * b + mc * c).to_2())

        area_of_shadow = convex_area(vertices)
        delta = abs(target - area_of_shadow)
        result = 'PASS' if delta < 1e-6 else 'FAIL'
        print('Case #{}: {}. target={}, you={}, delta={}'.format(case_number, result, target, area_of_shadow, delta))


if __name__ == '__main__':
    main()
