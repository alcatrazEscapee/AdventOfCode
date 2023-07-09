# Day 12: Hill Climbing Algorithm
# Rank: 43 / 80

from utils import get_input, a_star, FiniteGrid, Point2
from typing import Iterator


def main(text: str):
    grid = FiniteGrid.of_str(text).map_values(ord)
    start = end = None
    for pos in grid.locations():
        if grid[pos] == ord('S'):
            start = pos
        elif grid[pos] == ord('E'):
            end = pos

    assert start is not None and end is not None

    grid[start] = ord('a')
    grid[end] = ord('z')

    def step(p: Point2) -> Iterator[Point2]:
        for adj in p.neighbors():
            if adj in grid and grid[p] + 1 >= grid[adj]:
                yield adj

    print('Part 1:', a_star(start, end, step).cost)

    # Part 2: reverse the setup and traverse down
    def is_end(p: Point2) -> bool:
        return grid[p] == ord('a')

    def rev_step(p: Point2) -> Iterator[Point2]:
        for adj in p.neighbors():
            if adj in grid and grid[adj] + 1 >= grid[p]:
                yield adj

    print('Part 2:', a_star(end, is_end, rev_step).cost)


if __name__ == '__main__':
    main(get_input(12))
