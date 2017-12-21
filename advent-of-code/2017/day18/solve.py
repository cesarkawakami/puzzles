#!/usr/bin/env python3

from queue import Queue
from string import ascii_lowercase
from sys import stdin
from threading import Thread
from typing import List, Optional


class RecoveryError(Exception):
    def __init__(self, value: int) -> None:
        self.value = value


class Interpreter:
    def __init__(self) -> None:
        self.current_frequency: Optional[int] = None
        self.instruction_pointer = 0
        self.registers = {k: 0 for k in ascii_lowercase}
        self.instructions: List[str] = []

    def interpret(self, instruction: str) -> None:
        op, *args = instruction.split()
        jump_delta = 1

        if op == 'snd':
            self.on_send(self.value_of(args[0]))
        elif op == 'set':
            self.registers[args[0]] = self.value_of(args[1])
        elif op == 'add':
            self.registers[args[0]] += self.value_of(args[1])
        elif op == 'mul':
            self.registers[args[0]] *= self.value_of(args[1])
        elif op == 'mod':
            self.registers[args[0]] %= self.value_of(args[1])
        elif op == 'rcv':
            self.on_recover(args[0])
        elif op == 'jgz':
            if self.value_of(args[0]) > 0:
                jump_delta = self.value_of(args[1])

        self.instruction_pointer += jump_delta

    def value_of(self, expression: str) -> int:
        if expression.isalpha():
            return self.registers[expression]
        else:
            return int(expression)

    def load_program(self, program: str) -> None:
        self.instructions.extend(program.splitlines())

    def run(self) -> None:
        while True:
            self.interpret(self.instructions[self.instruction_pointer])

    def on_send(self, value: int) -> None:
        self.current_frequency = value

    def on_recover(self, value: str) -> None:
        if self.value_of(value) != 0:
            assert self.current_frequency is not None
            raise RecoveryError(self.current_frequency)


class InterpreterV2(Interpreter):
    def __init__(
        self,
        send_queue: 'Queue[int]',
        receive_queue: 'Queue[int]',
    ) -> None:
        super().__init__()
        self.send_queue = send_queue
        self.receive_queue = receive_queue
        self.value_send_count = 0

    def on_send(self, value: int) -> None:
        self.value_send_count += 1
        self.send_queue.put(value)

    def on_recover(self, register: str) -> None:
        self.registers[register] = self.receive_queue.get(timeout=3)


def part1(inp: str) -> None:
    interpreter = Interpreter()
    interpreter.load_program(inp)
    try:
        interpreter.run()
    except RecoveryError as ex:
        print(f'Part 1: {ex.value}')


def part2(inp: str) -> None:
    queue01: 'Queue[int]' = Queue()
    queue02: 'Queue[int]' = Queue()
    interpreters = [
        InterpreterV2(queue01, queue02),
        InterpreterV2(queue02, queue01),
    ]
    for i, x in enumerate(interpreters):
        x.load_program(inp)
        x.registers['p'] = i
    threads = [Thread(target=x.run) for x in interpreters]
    for t in threads:
        t.start()
    for t in threads:
        t.join()
    print(f'Part 2: {interpreters[1].value_send_count}')


def main() -> None:
    inp = stdin.read()
    part1(inp)
    part2(inp)


if __name__ == '__main__':
    main()
