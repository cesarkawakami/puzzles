#!/usr/bin/env python3

from sys import stdin
from typing import List, NamedTuple


class SecurityScannerState(NamedTuple):
    depth: int
    range_: int
    position: int
    direction: int

    def next(self) -> 'SecurityScannerState':
        if self.range_ <= 1:
            return self

        candidate_position = self.position + self.direction
        if not 0 <= candidate_position < self.range_:
            return self._replace(direction=-self.direction).next()
        return self._replace(position=candidate_position)

    def caught(self, packet_depth: int) -> bool:
        if packet_depth != self.depth:
            return False
        return self.position == 0

    def severity(self, packet_depth: int) -> int:
        return self.depth * self.range_ if self.caught(packet_depth) else 0

    def will_catch(self, initial_packet_depth: int) -> bool:
        time_delay = self.depth - initial_packet_depth
        return time_delay % ((self.range_ - 1) * 2) == 0


class FirewallState(NamedTuple):
    security_scanners: List[SecurityScannerState]

    def next(self) -> 'FirewallState':
        return self._replace(
            security_scanners=[x.next() for x in self.security_scanners],
        )

    def severity(self, packet_depth: int) -> int:
        return sum(x.severity(packet_depth) for x in self.security_scanners)

    def caught(self, packet_depth: int) -> bool:
        return any(x.caught(packet_depth) for x in self.security_scanners)

    def max_depth(self) -> int:
        return max(x.depth for x in self.security_scanners)

    def will_catch(self, initial_packet_depth: int) -> bool:
        return any(
            x.will_catch(initial_packet_depth) for x in self.security_scanners
        )


class PuzzleState(NamedTuple):
    packet_depth: int
    firewall: FirewallState

    def next(self) -> 'PuzzleState':
        return self._replace(
            packet_depth=self.packet_depth + 1,
            firewall=self.firewall.next(),
        )

    def severity(self) -> int:
        return self.firewall.severity(self.packet_depth)

    def caught(self) -> bool:
        return self.firewall.caught(self.packet_depth)

    def max_depth(self) -> int:
        return self.firewall.max_depth()


def input_puzzle(inp: str) -> PuzzleState:
    security_scanners = []
    max_depth = 0
    for scanner_input in inp.splitlines():
        depth_s, range_s = scanner_input.strip().split(': ')
        depth, range_ = int(depth_s), int(range_s)
        security_scanners.append(SecurityScannerState(
            depth=depth,
            range_=range_,
            position=0,
            direction=1,
        ))
        max_depth = max(max_depth, depth)
    puzzle_state = PuzzleState(
        packet_depth=0,
        firewall=FirewallState(
            security_scanners=security_scanners,
        ),
    )
    return puzzle_state


def forecast_states(puzzle_state: PuzzleState) -> List[PuzzleState]:
    all_states = [puzzle_state]
    for _ in range(abs(puzzle_state.packet_depth) + puzzle_state.max_depth()):
        puzzle_state = puzzle_state.next()
        all_states.append(puzzle_state)
    return all_states


def forecast_is_caught(puzzle_state: PuzzleState) -> bool:
    all_states = forecast_states(puzzle_state)
    return any(x.caught() for x in all_states)


def part1(inp: str) -> None:
    puzzle_state = input_puzzle(inp)
    all_states = forecast_states(puzzle_state)
    total_severity = sum(x.severity() for x in all_states)
    print(f'Part 1: {total_severity}')


def part2(inp: str) -> None:
    initial_puzzle_state = input_puzzle(inp)
    for delay in range(100000000):
        puzzle_state = initial_puzzle_state._replace(packet_depth=-delay)
        if not puzzle_state.firewall.will_catch(-delay):
            print(f'Part 2: {delay}')
            return
    raise RuntimeError('unable to find solution :(')


def main() -> None:
    inp = stdin.read()
    part1(inp)
    part2(inp)


if __name__ == '__main__':
    main()
