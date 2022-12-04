# Day 4: Camp Cleanup
# Rank: 255 / 113

from utils import get_input, ints


def main(text: str):
    part1 = part2 = 0
    for line in text.split('\n'):
        a0, a1, b0, b1 = ints(line, sign_prefixes=False)
        lhs = set(range(a0, 1 + a1))
        rhs = set(range(b0, 1 + b1))

        if lhs <= rhs or rhs <= lhs:
            part1 += 1  # One completely contained within the other

        if lhs & rhs:
            part2 += 1  # Any intersection between two sets

    print('Part 1:', part1)
    print('Part 2:', part2)


if __name__ == '__main__':
    main(get_input(4))
