#!/usr/bin/env python3

from collections import deque
from math import sqrt
from sys import stdin
from typing import Optional, Tuple, Set


class RootSet:

    def __init__(self, is_r: bool, roots: Tuple[int, ...]) -> None:
        self.is_r, self.roots = is_r, roots

    @classmethod
    def empty(cls) -> 'RootSet':
        return cls(False, ())

    @classmethod
    def from_roots(cls, roots: Tuple[int, ...]) -> 'RootSet':
        return cls(False, roots)

    @classmethod
    def all_reals(cls) -> 'RootSet':
        return cls(True, ())

    def intersect(self, other: 'RootSet') -> 'RootSet':
        if other.is_r:
            return self
        elif self.is_r:
            return other

        roots: Tuple[int, ...] = ()
        for self_root in self.roots:
            for other_root in other.roots:
                if self_root == other_root:
                    roots += (self_root,)
        return RootSet.from_roots(roots)

    def is_nonempty(self) -> bool:
        return self.is_r or bool(self.roots)

    def __repr__(self) -> str:
        return f'RootSet({self.is_r}, {self.roots})'

    def leftmost(self) -> Optional[int]:
        if self.is_r:
            return 0

        for root in sorted(self.roots):
            if root >= 0:
                return root
        return None


class Quadratic:

    def __init__(self, a: int, b: int, c: int) -> None:
        self.a, self.b, self.c = a, b, c

    def roots(self) -> RootSet:
        a, b, c = self.a, self.b, self.c
        if a != 0:
            delta = b ** 2 - 4 * a * c
            if delta < 0:
                return RootSet.empty()
            elif delta == 0:
                if b % (2 * a) != 0:
                    return RootSet.empty()
                return RootSet.from_roots((-b // 2 // a,))
            else:
                sqrt_delta = int(sqrt(delta))
                if sqrt_delta ** 2 != delta:
                    return RootSet.empty()

                nums = (-b + sqrt_delta, -b - sqrt_delta)
                roots: Tuple[int, ...] = ()
                for num in nums:
                    if num % (2 * a) == 0:
                        roots += (num // 2 // a,)

                if not roots:
                    return RootSet.empty()
                else:
                    return RootSet.from_roots(roots)
        elif b != 0:
            if c % b == 0:
                return RootSet.from_roots((-c // b,))
            return RootSet.empty()
        elif c != 0:
            return RootSet.empty()
        else:
            return RootSet.all_reals()


class ScalarTrajectory:

    def __init__(self, p: int, v: int, a: int) -> None:
        self.p, self.v, self.a = p, v, a

    def __sub__(self, other: 'ScalarTrajectory') -> 'ScalarTrajectory':
        return ScalarTrajectory(
            self.p - other.p,
            self.v - other.v,
            self.a - other.a,
        )

    def roots(self) -> RootSet:
        a = self.a
        b = 2 * self.v + self.a
        c = 2 * self.p
        roots = Quadratic(a, b, c).roots()
        return roots


class SpaceTrajectory:

    def __init__(
        self,
        x: ScalarTrajectory,
        y: ScalarTrajectory,
        z: ScalarTrajectory,
    ) -> None:
        self.x, self.y, self.z = x, y, z

    @classmethod
    def from_line(cls, line: str) -> 'SpaceTrajectory':
        eqs = line.strip().split(', ')
        px, py, pz, vx, vy, vz, ax, ay, az = [
            int(y) for x in eqs for y in x[3:-1].split(',')
        ]
        return cls(
            ScalarTrajectory(px, vx, ax),
            ScalarTrajectory(py, vy, ay),
            ScalarTrajectory(pz, vz, az),
        )

    def __sub__(self, other: 'SpaceTrajectory') -> 'SpaceTrajectory':
        return SpaceTrajectory(
            self.x - other.x,
            self.y - other.y,
            self.z - other.z,
        )

    def roots(self) -> RootSet:
        roots_x, roots_y, roots_z = \
            [t.roots() for t in [self.x, self.y, self.z]]
        return roots_x.intersect(roots_y).intersect(roots_z)


def part2(inp: str) -> None:
    particles = [
        SpaceTrajectory.from_line(line)
        for line in inp.splitlines()
    ]

    collisions = []
    for i, pi in enumerate(particles):
        for j, pj in enumerate(particles):
            if i == j:
                continue

            collision_time = (pi - pj).roots().leftmost()
            if collision_time is not None:
                collisions.append((collision_time, i, j))

    collision_queue = deque(sorted(collisions))
    removed: Set[int] = set()
    while collision_queue:
        to_add_to_removed: Set[int] = set()
        t, _, _ = collision_queue[0]
        while collision_queue and collision_queue[0][0] == t:
            _, i, j = collision_queue.popleft()
            if i not in removed and j not in removed:
                to_add_to_removed.add(i)
                to_add_to_removed.add(j)
        removed |= to_add_to_removed

    print(f'Part 2: {len(particles) - len(removed)}')


def main() -> None:
    inp = stdin.read()
    part2(inp)


if __name__ == '__main__':
    main()
