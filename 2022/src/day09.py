# Day 9: Rope Bridge
# Rank: 34 / 975 (oops!)

from utils import get_input, sign
from typing import List


DIRECT_MOVES = {'L': -1, 'R': 1, 'U': 1j, 'D': -1j}


def main(text: str):
    lines = text.split('\n')

    print('Part 1:', tail_visits(lines, 2))
    print('Part 2:', tail_visits(lines, 10))

def tail_visits(lines: List[str], length: int) -> int:
    visits = {0}
    rope = [0] * length

    for line in lines:
        direction, n = line.split(' ')
        for _ in range(int(n)):
            rope[0] += DIRECT_MOVES[direction]  # Move the head
            for i in range(1, len(rope)):  # Follow with each pair
                head, tail = rope[i - 1], rope[i]
                rope[i] += follow(head, tail)
            visits.add(rope[-1])

    return len(visits)

def follow(head: complex, tail: complex):
    # First, take the difference between head and tail
    # Second, we only move if we're at least 2 away **not** manhattan, but inf-norm distance
    # Third, we always move as much as possible, i.e. in the direction of the sign of the difference
    # Fourth, we *only* move in the sign of the distance, not more
    d, di = head.real - tail.real, head.imag - tail.imag
    if abs(d) > 1 or abs(di) > 1:
        return sign(d) + sign(di) * 1j
    return 0


if __name__ == '__main__':
    main(get_input(9))
