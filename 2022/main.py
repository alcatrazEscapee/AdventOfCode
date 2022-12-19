# Puzzle inputs go in a folder /inputs/day##.txt
# Answers go in a file /answers.txt in csv format, with a header row

import sys
import csv
import unittest

from io import StringIO
from utils import get_input
from contextlib import redirect_stdout

try:
    from src import day01, day02, day03, day04, day05, day06, day07, day08, day09, day10, day11, day12, day13, day14, day15, day16, day17, day18, day19, day20, day21, day22, day23, day24, day25
except ImportError:
    pass

class AoC2022Test(unittest.TestCase):
    pass


def make_test_method(day: int, module, part1_expected: str, part2_expected: str):
    def apply(self):
        inp = get_input(day, './inputs/day%02d.txt')
        with StringIO() as out, redirect_stdout(out):
            module.main(inp)
            output = out.getvalue()

        part1 = part2 = None
        for line in output.split('\n'):
            if line.startswith('Part 1: '):
                part1 = line[8:]
            elif line.startswith('Part 2: '):
                part2 = line[8:]

        self.assertEqual(part1_expected, part1)
        self.assertEqual(part2_expected, part2)
    return apply

def setup():
    with open('./answers.txt', 'r', encoding='utf-8') as f:
        answers = list(csv.reader(f))

    try:
        for day in range(1, 1 + 25):
            test = make_test_method(day, eval('day%02d' % day), *answers[day][:2])
            test.__name__ = 'test day %02d' % day
            setattr(AoC2022Test, test.__name__, test)
    except NameError:
        pass


if __name__ == '__main__':
    setup()
    sys.argv = sys.argv[:1]
    unittest.main()
