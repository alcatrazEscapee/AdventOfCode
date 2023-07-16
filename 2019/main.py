# Runs each day, checks against expected answer, and reports runtime. 
#
# Puzzle inputs go in a folder /inputs/day##.txt
# Answers go in a file /inputs/answers.txt in csv format, with a header row
#
# /inputs/answers.txt
# part1,part2,comment
# 1234,ABCD,this is space for optional comments, if desired
#
# Run with `python main.py` in the root directory.

import os
import sys
import csv
import runpy
import unittest

from io import StringIO
from time import time_ns
from contextlib import redirect_stdout

TIMES = {}

class AoCTest(unittest.TestCase):
    @classmethod
    def tearDownClass(cls):
        print('\n\n=== Runtime Summary ===')
        for day, t in TIMES.items():
            print('Day %02d : %d ms' % (day, t))


def make_test_method(day: int, part1_expected: str, part2_expected: str):
    def apply(self):
        start = time_ns()
        with StringIO() as out, redirect_stdout(out):
            runpy.run_path('./day%02d.py' % day, {}, '__main__')
            output = out.getvalue()
        end = time_ns()

        part1 = part2 = ''
        for line in output.split('\n'):
            if line.startswith('Part 1: '):
                part1 = line[8:]
            elif line.startswith('Part 2: '):
                part2 = line[8:]

        self.assertEqual(part1_expected, part1)
        self.assertEqual(part2_expected, part2)
        TIMES[day] = (end - start) // 1_000_000
    return apply

def setup():
    with open('./inputs/answers.txt', 'r', encoding='utf-8') as f:
        answers = list(csv.reader(f))

    for day in range(1, 1 + 25):
        test = make_test_method(day, *answers[day][:2])
        test.__name__ = 'test day %02d' % day
        setattr(AoCTest, test.__name__, test)


if __name__ == '__main__':
    setup()
    sys.argv = sys.argv[:1]
    os.chdir('src')
    unittest.main()
