#!/usr/bin/env python3

import re
from math import sqrt
from sys import stdin
from typing import List, Tuple, Type, TypeVar, Union

EPS = 1e-3


def decompose(line: str) -> Tuple[
    Tuple[float, float, float],
    Tuple[float, float, float],
    Tuple[float, float, float],
]:
    px, py, pz, vx, vy, vz, ax, ay, az = \
        map(float, re.findall(r'[\-0123456789]+', line))
    return ((px, py, pz), (vx, vy, vz), (ax, ay, az))


def part1(inp: str) -> None:
    particles = []
    for index, line in enumerate(inp.splitlines()):
        particles.append((index, decompose(line)))

    particles.sort(
        key=lambda x: abs(x[1][2][0]) + abs(x[1][2][1]) + abs(x[1][2][2]),
    )

    for p in particles[:10]:
        print(p)


class RealLine:
    __slots__: List[str] = []


QuadraticSolution = Union[Tuple[float, ...], RealLine]


class STraj:
    __slots__ = ['p', 'v', 'a']

    def __init__(self, p: float, v: float, a: float) -> None:
        self.p, self.v, self.a = p, v, a

    def sub(self, other: 'STraj') -> 'STraj':
        return STraj(self.p - other.p, self.v - other.v, self.a - other.a)

    def collide_with(self, other: 'STraj') -> QuadraticSolution:
        return self.sub(other).collide_zero()

    def collide_zero(self) -> QuadraticSolution:
        # print(f'{self.a} {self.v} {self.p}')
        if self.a != 0:
            delta = self.v ** 2 - 4 * self.a * self.p
            if delta < 0:
                return ()
            elif delta == 0:
                return (-self.v / 2 / self.a,)

            return (
                (-self.v + sqrt(delta)) / 2 / self.a,
                (-self.v - sqrt(delta)) / 2 / self.a,
            )
        elif self.v != 0:
            return (-self.p / self.v,)
        elif self.p != 0:
            return ()
        else:
            return RealLine()

    def at(self, t: float) -> float:
        return self.a * t * t + self.v * t + self.p


PointT = TypeVar('PointT', bound='Point')


class Point:
    __slots__ = ['x', 'y', 'z']

    def __init__(self, x: float, y: float, z: float) -> None:
        self.x, self.y, self.z = x, y, z

    def approx_equal(self, other: 'Point') -> bool:
        return (
            abs(self.x - other.x) < EPS and
            abs(self.y - other.y) < EPS and
            abs(self.z - other.z) < EPS
        )

    def __repr__(self) -> str:
        return f'Point({self.x}, {self.y}, {self.z})'


ParticleT = TypeVar('ParticleT', bound='Particle')


class Particle:
    __slots__ = ['tx', 'ty', 'tz']

    def __init__(self, tx: STraj, ty: STraj, tz: STraj) -> None:
        self.tx, self.ty, self.tz = tx, ty, tz

    @classmethod
    def from_line(cls: Type[ParticleT], line: str) -> ParticleT:
        ((px, py, pz), (vx, vy, vz), (ax, ay, az)) = decompose(line)
        return cls(
            STraj(px, vx, ax),
            STraj(py, vy, ay),
            STraj(pz, vz, az),
        )

    def at(self, t: float) -> Point:
        return Point(self.tx.at(t), self.ty.at(t), self.tz.at(t))

    def collides_with(self, other: 'Particle') -> bool:
        tx_cands = self.tx.collide_with(other.tx)
        ty_cands = self.ty.collide_with(other.ty)
        tz_cands = self.tz.collide_with(other.tz)
        if not isinstance(tx_cands, RealLine):
            t_cands = tx_cands
        elif not isinstance(ty_cands, RealLine):
            t_cands = ty_cands
        elif not isinstance(tz_cands, RealLine):
            t_cands = tz_cands
        else:
            # if all three are the real line, then particles collide
            return True
        # print(t_cands)
        for t in t_cands:
            if self.at(t).approx_equal(other.at(t)):
                return True
        return False


def part2(inp: str) -> None:
    particles = [Particle.from_line(line) for line in inp.splitlines()]
    to_delete = set()
    for i, pi in enumerate(particles):
        for j, pj in enumerate(particles):
            if i == j:
                continue
            if pi.collides_with(pj):
                print('coll')
                to_delete.add(i)

    remaining_particles = [
        p for i, p in enumerate(particles) if i not in to_delete
    ]

    print(f'Part 2: {len(remaining_particles)}')


def main() -> None:
    inp = stdin.read()
    part1(inp)
    part2(inp)


if __name__ == '__main__':
    main()
