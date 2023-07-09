# Day 3: Rucksack Reorganization
# Rank: 164 / 139

from utils import get_input


def main(text: str):
    lines = text.split('\n')
    part1 = part2 = 0

    for line in lines:
        mid = len(line) // 2
        first, second = line[:mid], line[mid:]
        c = next(c for c in set(first) & set(second))
        part1 += priority(c)

    print('Part 1:', part1)

    for i in range(len(lines) // 3):
        e1, e2, e3 = lines[i * 3:(i + 1) * 3]
        c = next(c for c in set(e1) & set(e2) & set(e3))
        part2 += priority(c)

    print('Part 2:', part2)

def priority(c: str) -> int:
    return ord(c) + (1 - ord('a') if c.islower() else 27 - ord('A'))


if __name__ == '__main__':
    main(get_input(3))
