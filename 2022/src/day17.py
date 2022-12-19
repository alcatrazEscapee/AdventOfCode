# Day 17: Pyroclastic Flow
# Rank: 112 / 401

from utils import get_input
from typing import Tuple, Set


ROCKS: Tuple[Set[Tuple[int, int]], ...] = (
    {(0, 0), (1, 0), (2, 0), (3, 0)},  # Horizontal
    {(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)},  # Plus
    {(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)},  # Backwards L
    {(0, 0), (0, 1), (0, 2), (0, 3)},  # Vertical
    {(0, 0), (0, 1), (1, 0), (1, 1)},  # Square
)

# A modifier for the amount of history to keep in the tetris grid
# This both improves the speed (as less history means less positions to check), and ease of detecting cycles for part 2
# On my input at least, any history value < 50 will cause pieces to drop off the board (which the solution below will detect and throw an exception for)
HISTORY = 50


def main(jets: str):

    jet_tick = rock_tick = 0
    floor = {(x, 0) for x in range(7)}
    total_rocks = 0

    # Phase of the iteration
    # 'part1' = Running up until rock 2022, for part 1
    # 'wait' = Waiting for a cycle to be detected
    # 'part2' = Filling in the last remaining rows until part 2 is done
    phase = 'part1'

    seen = dict()  # Prior states, mapped to the (max y, total rocks)
    skip_y: int = 0  # A y value to add, counting for skipped iterations due to the cycle

    while True:
        max_y = max(y for _, y in floor)

        if total_rocks == 2022:  # Phase 'part1'
            print('Part 1:', max_y)
            phase = 'wait'  # Move onto cycle detection

        if total_rocks == 1000000000000:  # Phase 'part2'
            print('Part 2:', max_y + skip_y)
            return  # And exit

        if phase == 'wait':
            top_floor = frozenset((x, y - max_y) for x, y in floor if y >= max_y - HISTORY)
            state = top_floor, rock_tick, jet_tick
            if state in seen:
                prior_max_y, prior_total_rocks = seen[state]

                # Skip ahead by the known interval
                cycle_length = total_rocks - prior_total_rocks
                cycle_count = (1000000000000 - total_rocks - 1) // cycle_length
                total_rocks += cycle_count * cycle_length
                skip_y += (max_y - prior_max_y) * cycle_count
                phase = 'part2'
            else:
                seen[state] = max_y, total_rocks

        # Drop any from the floor older than our history
        # This is not strictly necessary, but it improves performance at the cost of having an unsound solution (as a flat history value could produce invalid results)
        floor = {(x, y) for x, y in floor if y > max_y - HISTORY}

        # Advance the rock
        rock = {(x + 2, max_y + y + 4) for x, y in ROCKS[rock_tick]}
        rock_tick = (rock_tick + 1) % len(ROCKS)

        while True:
            jet = jets[jet_tick]
            jet_tick = (jet_tick + 1) % len(jets)

            # Move horizontally, if possible
            if jet == '<':
                test_rock = {(x - 1, y) for x, y in rock}
                if all((x, y) not in floor and x >= 0 for x, y in test_rock):
                    rock = test_rock
            elif jet == '>':
                test_rock = {(x + 1, y) for x, y in rock}
                if all((x, y) not in floor and x < 7 for x, y in test_rock):
                    rock = test_rock

            # Move down, and stop if we reach solid ground
            test_rock = {(x, y - 1) for x, y in rock}
            if any(r in floor for r in test_rock):
                floor |= rock
                total_rocks += 1
                break
            else:
                rock = test_rock

            if any(y < 0 for _, y in rock):
                raise ValueError('HISTORY = %d is too low, a piece managed to drop through the entire board!' % HISTORY)


if __name__ == '__main__':
    main(get_input(17))
