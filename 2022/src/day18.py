# Day 18: Boiling Boulders
# Rank: 10 / 131

from utils import get_input, ints
from collections import deque
from typing import Tuple, Set

STEPS: Tuple[Tuple[int, int, int], ...] = ((0, 0, 1), (0, 0, -1), (0, 1, 0), (0, -1, 0), (1, 0, 0), (-1, 0, 0))


def main(text: str):
    lines = text.split('\n')
    points = {ints(line) for line in lines}
    part1, checks = surface_area(points)

    print('Part 1:', part1)

    # Part 2, we flood fill the remaining area
    # We use a simple heuristic to determine if we are currently in the exterior - by declaring any region 'too large' as the exterior
    interior = set()
    while checks:
        c = checks.pop()  # Start checking from the current position
        queue = deque([c])  # Use a queue so we BFS instead of DFS, why? Because this means in an unbounded case, we 'wrap around' the exterior, eliminating many other check points and vastly improving our runtime
        area = {c}

        while queue:
            if len(area) > 10_000:  # Assume the area is unbounded
                checks -= area
                break
            px, py, pz = queue.popleft()
            for dx, dy, dz in STEPS:
                a = px + dx, py + dy, pz + dz
                if a not in area and a not in points:
                    queue.append(a)
                    area.add(a)
        else:
            # Found a bounded interior
            interior |= area
            checks -= area

    # Replace all the interior with points, and then recompute the surface area
    points |= interior
    print('Part 2:', surface_area(points)[0])


def surface_area(points: Set[Tuple[int, int, int]]) -> Tuple[int, Set[Tuple[int, int, int]]]:
    ext = set()
    sa = 0
    for x, y, z in points:
        for dx, dy, dz in STEPS:
            p = x + dx, y + dy, z + dz
            if p not in points:
                ext.add(p)
                sa += 1
    return sa, ext


if __name__ == '__main__':
    main(get_input(18))
