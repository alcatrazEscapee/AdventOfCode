import unittest
import difflib

class AoC2022Test(unittest.TestCase):
    def test(self):
        with open('./out/log.txt', 'r', encoding='utf-8') as f:
            expected = f.read()
        with open('./inputs/answers.txt', 'r', encoding='utf-8') as f:
            actual = f.read()
        self.assertEqual(expected, actual, '\n\n%s\n\n=== Expected ===\n\n%s\n\n=== Actual\n\n%s\n\n=== Diff ===\n' % (
            expected, actual,
            '\n'.join(difflib.unified_diff(actual.split('\n'), expected.split('\n'), fromfile='actual', tofile='expected'))
        ))

if __name__ == '__main__':
    unittest.main()