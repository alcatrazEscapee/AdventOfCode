# Day 20: Grove Positioning System
# Rank: 16 / 10 !!

from utils import get_input, ints
from typing import Sequence


def main(text: str):
    # Conceptually, today is fairly simple. It is also tractable with a not-too-complex data structure
    #   If you *were* to hyper optimize this, you need a data structure that supports a) access known element, b) insert at random element, and c) pop at random element
    #   Lists in python have O(n), O(n) and O(n) for all of those which ended up being fine - I was expecting much worse runtime.
    # The optimal data structure, that I can think of, is a wheel like structure with 'spokes':
    #   The original order is stored in an array, with each element pointing to an element on the wheel, so a) is O(1)
    #   The wheel itself is a doubly linked list, so b) and c) are O(1)
    #   Thus the whole solution would be O(n)
    # This solution is O(n^3)... but hey, n=5000 so it works :D
    values = ints(text)
    print('Part 1:', solve(values, n=1))
    print('Part 2:', solve([v * 811589153 for v in values], n=10))


def solve(values: Sequence[int], n: int) -> int:
    values = list(enumerate(values))
    for _ in range(n):
        for i in range(len(values)):  # i = Original ordinal of the element to be moved
            for k, (j, v) in enumerate(values):  # j = search ordinal, k = current index, v = the value at that index
                if j == i:
                    values.pop(k)
                    values.insert((k + v) % len(values), (i, v))
                    break
    values = [v for _, v in values]
    zero = values.index(0)
    return sum(values[(zero + offset) % len(values)] for offset in (1000, 2000, 3000))


if __name__ == '__main__':
    main(get_input(20))
