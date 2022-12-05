# Day 5: Supply Stacks
# Rank: 15 / 18

from utils import get_input, ints
from typing import Tuple, List


def main(text: str):
    # At contest time, I simply hardcoded the input as a list of strings
    # This was very fast, but for completeness, here's an actual parser
    original_crates, moves = parse_input(text)

    # Solve both with two separate copies of the original crates
    part1 = list(map(list, original_crates))
    part2 = list(map(list, original_crates))
    for line in moves:
        move, i_from, i_to = ints(line)

        # Part 1 - each crate behaves like a stack, so we can use append / pop(-1)
        for _ in range(move):
            part1[i_to].append(part1[i_from].pop(-1))

        # Part 2 - we have to reverse the slice, one way is to pop into a buffer, then pop out!
        buffer = []
        for _ in range(move):
            buffer.append(part2[i_from].pop(-1))
        for _ in range(move):
            part2[i_to].append(buffer.pop(-1))

    print('Part 1:', top_of_stack(part1))
    print('Part 2:', top_of_stack(part2))


def top_of_stack(crates: List[List[str]]) -> str:
    return ''.join(c[-1] for c in crates[1:])  # Skip the zeroth crate

def parse_input(text: str) -> Tuple[List[List[str]], List[str]]:
    stack, moves = text.split('\n\n')
    crates = [['?'], [], [], [], [], [], [], [], [], []]  # Empty crate in position zero, nine other crates
    for line in stack.split('\n')[-2::-1]:  # Reverse order, skip the first line (crate numbers)
        for i in range(9):
            if 4 * i + 1 < len(line) and (c := line[4 * i + 1]) != ' ':
                crates[i + 1].append(c)
    return crates, moves.split('\n')


if __name__ == '__main__':
    main(get_input(5))
