# Day 2: Rock Paper Scissor
# Rank: 511 / 277

from utils import get_input


def main(text: str):
    lines = text.split('\n')
    mapping = {'X': 0, 'A': 0, 'B': 1, 'Y': 1, 'C': 2, 'Z': 2}  # Score = mapping[x] + 1
    part1 = part2 = 0
    for line in lines:
        them, me = mapping[line[0]], mapping[line[2]]

        part1 += 1 + me  # Always count the hand we played
        if (them + 1) % 3 == me:  # Win
            part1 += 6
        elif them == me:  # Tie
            part1 += 3

        if me == mapping['X']:  # Lose
            part2 += 1 + ((them + 2) % 3)
        elif me == mapping['Y']:  # Draw
            part2 += 1 + them + 3
        elif me == mapping['Z']:  # Win
            part2 += 1 + ((them + 1) % 3) + 6

    print('Part 1:', part1)
    print('Part 2:', part2)


if __name__ == '__main__':
    main(get_input(2))
