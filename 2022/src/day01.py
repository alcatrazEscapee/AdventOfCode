# Day 1: Calorie Counting
# Rank: 45 / 135

from utils import get_input, ints


def main(text: str):
    weights = sorted([
        sum(ints(elf))
        for elf in text.split('\n\n')
    ], reverse=True)

    print('Part 1:', weights[0])
    print('Part 2:', sum(weights[:3]))


if __name__ == '__main__':
    main(get_input(1))
