# Day 23: Unstable Diffusion
# Rank: 530 / 425

from utils import get_input, FiniteGrid, Point2
from collections import defaultdict
from time import time_ns

# Cardinal Directions
N = Point2(0, -1)
NE = Point2(1, -1)
NW = Point2(-1, -1)
S = Point2(0, 1)
SE = Point2(1, 1)
SW = Point2(-1, 1)
W = Point2(-1, 0)
E = Point2(1, 0)

ADJ8 = (NW, N, NE, W, E, SW, S, SE)

PROPOSALS = (
    ((N, NE, NW), N),
    ((S, SE, SW), S),
    ((W, NW, SW), W),
    ((E, NE, SE), E),
)


def main(text: str):

    # Abuse `FiniteGrid` for parsing, but we actually just track a set
    grid = FiniteGrid.of_str(text)
    elves = set(p for p in grid.locations() if grid[p] == '#')

    count = 0
    part1 = part2 = False
    tick = time_ns()
    while not part1 or not part2:
        next_elves = set()

        # Phase 1: Each elf proposes their move, or doesn't move
        next_elves_proposed_moves = defaultdict(set)
        for pos in elves:
            if all((pos + dp) not in elves for dp in ADJ8):
                next_elves.add(pos)  # Elf has no neighbors, and does not move
            else:
                for pi in range(4):  # Check each proposal
                    checks, move = PROPOSALS[(pi + count) % 4]
                    if all((pos + check) not in elves for check in checks):
                        next_elves_proposed_moves[pos + move].add(pos)
                        break
                else:
                    next_elves.add(pos)

        # Phase 2: resolve all proposed moves
        for pos, all_from in next_elves_proposed_moves.items():
            if len(all_from) == 1:
                next_elves.add(pos)  # One elf moves to the destination
            else:
                next_elves |= all_from  # None of the elves move

        if count == 10:
            # Part 1: count the blank space around the elves
            part1 = True
            min_x, max_x = min(x for x, _ in elves), max(x for x, _ in elves)
            min_y, max_y = min(y for _, y in elves), max(y for _, y in elves)
            print('Part 1:', sum((x, y) not in elves for x in range(min_x, 1 + max_x) for y  in range(min_y, 1 + max_y)))
            print('  in %d ms' % ((time_ns() - tick) / 1_000_000))

        if elves == next_elves:
            # Part 2: Identify the first round where no elf moves
            part2 = True
            print('Part 2:', count + 1)
            print('  in %d ms' % ((time_ns() - tick) / 1_000_000))

        count += 1
        elves = next_elves


if __name__ == '__main__':
    main(get_input(23))
