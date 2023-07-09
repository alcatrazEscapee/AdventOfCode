# Day 20: Grove Positioning System
# Rank: 16 / 10 !!

from utils import get_input, ints
from typing import Sequence, List, Set
from time import time_ns

import math


class BucketList:
    # This is an optimized structure for this problem specifically
    # 1. In order to avoid problems with duplicates, we keep the input values in a separate list and only act on the ordinal of these values
    # 2. We keep the values in buckets, which means all of our previously-linear searches (such as finding an element, popping an element, inserting an element, etc)
    #    instead of being O(n) operations, on an *optimally distributed set of buckets*, act more like O(sqrt(n))
    # The runtime using `BucketList`, which is O(n x sqrt(n)) is a ~10x magnitude speedup over using a flat list (which would be O(n^2))

    def __init__(self, values: Sequence[int]):
        n = math.ceil(math.sqrt(len(values)))
        self.values: List[int] = list(values)  # The original values, so we can just deal with their ordinals, which are unique
        self.buckets: List[List[int]] = [list(range(i, min(i + n, len(values)))) for i in range(0, len(values), n)]  # A list of buckets, containing the original order
        self.sets: List[Set[int]] = [set(b) for b in self.buckets]  # A list of sets, for quickly finding by original index.
        self.modulus: int = len(values) - 1  # The modulus that we shift by

    def pop_shift(self, n: int):
        # Find the bucket index, and interior index for the current value
        # Set contains is O(1), and we should do < sqrt(n) of them, and one index, which is also O(sqrt(n))
        i1, i2 = next((i1, self.buckets[i1].index(n)) for i1, s in enumerate(self.sets) if n in s)

        # Pop from the bucket and set at (i1, i2), value n
        # This should also be O(1) and O(sqrt(n)) respectively
        self.sets[i1].remove(n)
        bucket = self.buckets[i1]
        bucket.pop(i2)

        # Calculate the new index, and step forward until we find a matching bucket
        # Stepping forward should again be O(sqrt(n)) as we only check int comparisons, and we're stepping by sqrt(n) on average
        i3 = i2 + (self.values[n] % self.modulus)
        while i3 >= len(bucket):
            i3 -= len(bucket)
            i1 += 1
            if i1 >= len(self.buckets):
                i1 = 0
            bucket = self.buckets[i1]

        # Insert the value into the found bucket, and update the set
        # One last O(sqrt(n)) operation
        self.buckets[i1].insert(i3, n)
        self.sets[i1].add(n)

    def flatten(self) -> List[int]:
        return [self.values[x] for y in self.buckets for x in y]

    def __str__(self) -> str: return repr(self)
    def __repr__(self) -> str: return '[Buckets: %s | Sets: %s | Values: %s | Actual: %s]' % (self.buckets, self.sets, self.values, self.flatten())


def main(text: str):
    values = ints(text)
    tick = time_ns()
    print('Part 1:', solve(values, n=1))
    print('  in %d ms' % (((tock := time_ns()) - tick) / 1_000_000))
    print('Part 2:', solve([v * 811589153 for v in values], n=10))
    print('  in %d ms' % ((time_ns() - tock) / 1_000_000))


def solve(values: Sequence[int], n: int) -> int:
    bl = BucketList(values)
    for _ in range(n):
        for i in range(len(values)):
            bl.pop_shift(i)
    values = bl.flatten()
    zero = values.index(0)
    return sum(values[(zero + offset) % len(values)] for offset in (1000, 2000, 3000))


if __name__ == '__main__':
    main(get_input(20))
