# Day 14: Regolith Reservoir
# Rank: 152 / 149

from utils import get_input, ints
from itertools import pairwise
from typing import Set, Tuple


def main(text: str):
    blocked: Set[Tuple[int, int]] = set()

    for line in text.split('\n'):
        parts = line.split(' -> ')
        for p1, p2 in pairwise(parts):
            x1, y1 = ints(p1)
            x2, y2 = ints(p2)
            if x1 == x2:
                for y in range(min(y1, y2), 1 + max(y1, y2)):
                    blocked.add((x1, y))
            else:
                assert y1 == y2
                for x in range(min(x1, x2), 1 + max(x1, x2)):
                    blocked.add((x, y1))

    max_y = max(y for _, y in blocked) + 2

    print('Part 1:', solve(set(blocked), max_y))

    for x in range(500 - max_y - 1, 2 + 500 + max_y):
        blocked.add((x, max_y))

    print('Part 2:', solve(blocked, max_y))


def solve(blocked: Set[Tuple[int, int]], max_y: int) -> int:
    count = 0
    while True:
        x, y = 500, 0
        if (x, y) in blocked:
            return count

        while True:
            if y > max_y:
                return count
            if (x, y + 1) not in blocked:
                y += 1
            elif (x - 1, y + 1) not in blocked:
                y += 1
                x -= 1
            elif (x + 1, y + 1) not in blocked:
                y += 1
                x += 1
            else:
                blocked.add((x, y))
                count += 1
                break


if __name__ == '__main__':
    main(get_input(14))
