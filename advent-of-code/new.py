#!/home/cesarkawakami/pcode/puzzles/advent-of-code/venv/bin/python3

import os
import os.path
import requests
from argparse import ArgumentParser
from typing import Dict


PYTHON_TEMPLATE = '''\
#!/home/cesarkawakami/pcode/puzzles/advent-of-code/venv/bin/python3

from sys import stdin


def main():
    inp = stdin.read()


if __name__ == '__main__':
    main()
'''


def get_cookies() -> Dict[str, str]:
    rv = {}
    with open(os.path.join(os.environ['HOME'], '.adventofcode'), 'r') as f:
        for line in f:
            cookie_name, cookie_value = line.rstrip('\n').split('=')
            rv[cookie_name] = cookie_value
    return rv


def download_input(year: int, day: int) -> str:
    cookies = get_cookies()
    response = requests.get(
        f'http://adventofcode.com/{year}/day/{day}/input',
        cookies=cookies,
    )
    return response.text


def main() -> None:
    parser = ArgumentParser()
    parser.add_argument('year', type=int)
    parser.add_argument('day', type=int)
    args = parser.parse_args()

    input_string = download_input(args.year, args.day)

    day_path = os.path.join(
        os.path.dirname(__file__),
        str(args.year),
        f'day{args.day}',
    )
    for part in ['part1', 'part2']:
        part_path = os.path.join(day_path, part)
        os.makedirs(part_path, exist_ok=True)
        with open(os.path.join(part_path, 'input'), 'w') as f:
            f.write(input_string)
        python_path = os.path.join(part_path, 'solve.py')
        with open(python_path, 'w') as f:
            f.write(PYTHON_TEMPLATE)
        os.chmod(python_path, 0o755)


if __name__ == '__main__':
    main()
