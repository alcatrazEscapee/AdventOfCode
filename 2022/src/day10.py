# Day 10: Cathode-Ray Tube
# Rank: 409 / 107

from utils import get_input, FiniteGrid
from aoc_font import recognize


def main(text: str):
    x = 1
    cycle = 1
    pulses = []  # The marked list of pulses (cycle * x)
    pixels = FiniteGrid.of_empty(40, 6, '.')  # For part 2

    def spin(n: int):
        nonlocal cycle
        for _ in range(n):
            if (cycle - 20) % 40 == 0:  # Record marked pulses for part 1
                pulses.append(x * cycle)

            px, py = (cycle - 1) % 40, (cycle - 1) // 40
            if abs(px - x) <= 1:  # When the sprite lines up, mark the screen
                pixels[px, py] = '#'
            cycle += 1

    for line in text.split('\n'):
        parts = line.split(' ')
        if parts[0] == 'noop':
            spin(1)
        elif parts[0] == 'addx':
            # This processor is not pipelined, so we don't need to support adding ahead of the current execution
            # So, we simply spin two cycles, then increment x
            spin(2)
            x += int(parts[1])

    print('Part 1:', sum(pulses))
    print('Part 2: %s\n%s' % (recognize(pixels), pixels))


if __name__ == '__main__':
    main(get_input(10))
