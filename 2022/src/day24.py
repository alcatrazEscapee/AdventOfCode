# Day 24: Blizzard Basin
# Rank: 76 / 57

from utils import get_input, a_star
from typing import Set, Tuple
from time import time_ns

import functools


UP = (0, -1)
DOWN = (0, 1)
LEFT = (-1, 0)
RIGHT = (1, 0)

MOVES = {'^': UP, 'v': DOWN, '<': LEFT, '>': RIGHT}


def main(text: str):
    lines = [line[1:-1] for line in text.split('\n')][1:-1]  # Trim the outside edges
    width, height = len(lines[0]), len(lines)
    initial_blizzard = set(
        (x, y, c)
        for y, line in enumerate(lines)
        for x, c in enumerate(line)
        if c != '.'
    )

    start = (0, -1)
    end = (width - 1, height)

    @functools.lru_cache(None)
    def blizzard(n: int) -> Set[Tuple[int, int, str]]:
        # Computes the blizzard at turn N, with recursive + memoization reducing work required
        if n == 0:
            return initial_blizzard
        prev = blizzard(n - 1)
        new_bliz = set()
        for x, y, c in prev:
            dx, dy = MOVES[c]
            px, py = (x + dx) % width, (y + dy) % height
            new_bliz.add((px, py, c))
        assert len(new_bliz) == len(prev)
        return new_bliz

    def in_blizzard(bliz: Set[Tuple[int, int, str]], x: int, y: int) -> bool:
        # Checks if the given position matches any known blizzard positions
        return any((x, y, b) in bliz for b in '<>^v')

    def step(pos_t: Tuple[int, int, int]):
        # The step function, as used by A*
        # Returns a list of all valid points
        x, y, t = pos_t
        steps = []
        bliz = blizzard(t + 1)
        if not in_blizzard(bliz, x, y):  # Allow not moving
            steps.append((x, y, t + 1))
        for dx, dy in (UP, DOWN, LEFT, RIGHT):
            adj = px, py = x + dx, y + dy
            if not in_blizzard(bliz, px, py) and ((0 <= px < width and 0 <= py < height) or adj == end or adj == start):
                steps.append((px, py, t + 1))
        return steps

    # Conditions for reaching the start or end, as we can reach them at any time
    def is_end(pos_t: Tuple[int, int, int]) -> bool:
        x, y, _ = pos_t
        return (x, y) == end

    def is_start(pos_t: Tuple[int, int, int]) -> bool:
        x, y, _ = pos_t
        return (x, y) == start

    # Simple A* heuristics for moving towards the start and the end. Assume we can move uninterrupted.
    def heuristic_to_end(pos_t: Tuple[int, int, int]) -> int:
        x, y, _ = pos_t
        return abs(x - width) + abs(y - height)

    def heuristic_to_start(pos_t: Tuple[int, int, int]) -> int:
        x, y, _ = pos_t
        return abs(x) + abs(y)

    # Three invocations to A*
    # Since we can re-explore positions, due to also tracking the time step, the heuristics help here with runtime
    # I see reductions of heap ops by ~46% on the example input and ~60% on my input.
    tick = time_ns()
    first = a_star(start + (0,), is_end, step, heuristic_to_end).cost
    second = first + a_star(end + (first,), is_start, step, heuristic_to_start).cost
    third = second + a_star(start + (second,), is_end, step, heuristic_to_end).cost

    print('Part 1:', first)
    print('Part 2:', third)
    print('  in %d ms' % ((time_ns() - tick) // 1_000_000))


if __name__ == '__main__':
    main(get_input(24))
