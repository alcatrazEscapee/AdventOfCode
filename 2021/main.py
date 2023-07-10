# Runs each day, checks against expected answer, and reports runtime. 
#
# Puzzle inputs go in a folder /inputs/day##.txt
# Example inputs (if present) go in a folder /inputs/day##_examples.txt
# Each example should be separated by '\n\n=====\n\n'. Examples are optional
# Answers go in a file /inputs/answers.txt in csv format, with no headers
#
# /inputs/answers.txt
# 1234,ABCD,abcd,...
#
# Run with `python main.py` in the root directory.

import sys
import csv
import unittest

from io import StringIO
from time import time_ns
from utils import get_input
from contextlib import redirect_stdout

from src import day01, day02, day03, day04, day05, day06, day07, day08, day09, day10, day11, day12, day13, day14, day15, day16, day17, day18, day19, day20, day21, day22, day23, day24, day25

DAYS = day01, day02, day03, day04, day05, day06, day07, day08, day09, day10, day11, day12, day13, day14, day15, day16, day17, day18, day19, day20, day21, day22, day23, day24, day25
TIMES = {}

class AoCTest(unittest.TestCase):
    @classmethod
    def tearDownClass(cls):
        print('\n\n=== Runtime Summary ===')
        for day, t in TIMES.items():
            print('Day %02d : %d ms' % (day, t))


def make_test_method(name: str, example: int, day: int, module, part1_expected: str, part2_expected: str):
    def apply(self):
        if example == -1:
            inp = get_input(day, './inputs/day%02d.txt')
        else:
            inp = get_input(day, './inputs/day%02d_examples.txt').split('\n\n=====\n\n')[example]
        start = time_ns()
        with StringIO() as out, redirect_stdout(out):
            module.main(inp)
            output = out.getvalue()
        end = time_ns()

        part1 = part2 = 'None'
        for line in output.split('\n'):
            if line.startswith('Part 1: '):
                part1 = line[8:]
            elif line.startswith('Part 2: '):
                part2 = line[8:]

        self.assertEqual(part1_expected, part1)
        self.assertEqual(part2_expected, part2)
        TIMES[day] = (end - start) // 1_000_000
    
    apply.__name__ = name
    setattr(AoCTest, apply.__name__, apply)

def setup():
    with open('./inputs/answers.txt', 'r', encoding='utf-8') as f:
        answers = list(csv.reader(f))

    for day in range(1, 1 + 25):
        part1, part2, *examples = answers[day - 1]
        make_test_method('test day %02d' % day, -1, day, DAYS[day - 1], part1, part2)
        
        for example in range(len(examples) // 2):
            make_test_method('test day %02d example %d' % (day, example), example, day, DAYS[day - 1], examples[2 * example], examples[2 * example + 1])


if __name__ == '__main__':
    setup()
    sys.argv = sys.argv[:1]
    unittest.main()
