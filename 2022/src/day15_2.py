import time

from utils import get_input, ints
from typing import NamedTuple, Optional, Iterable

# READ THE PROBLEM; UNDERSTAND THE PROBLEM; IMPLEMENT THE SOLUTION
# Remember: ints() for parsing, Counter, Point2/3, Grids (Finite/Infinite), A*, DP (lru_cache)

class Rect(NamedTuple):
    """ A 2-dimensional box defined by [x0, x1] x [y0, y1] """
    x0: int
    x1: int
    y0: int
    y1: int

    def intersect(self, other: 'Rect') -> Optional['Rect']:
        """ Compute the intersection of this box with another, if it exists. """
        if not (self.x1 < other.x0 or self.x0 > other.x1 or
                self.y1 < other.y0 or self.y0 > other.y1):
            return Rect(
                max(self.x0, other.x0), min(self.x1, other.x1),
                max(self.y0, other.y0), min(self.y1, other.y1)
            )

    def decompose(self, other: 'Rect') -> Iterator['Rect']:
        """ Where other is a strict subset of this rect, return a set of disjoint boxes which partition a - b """
        a, b = self, other
        for rect in (
            Rect(a.x0, b.x0 - 1, a.y0, a.y1),
            Rect(b.x1 + 1, a.x1, a.y0, a.y1),
            Rect(b.x0, b.x1, a.y0, b.y0 - 1),
            Rect(b.x0, b.x1, b.y1 + 1, a.y1),
        ):
            if rect.area() > 0:
                yield rect

    def area(self) -> int:
        return max(0, 1 + self.x1 - self.x0) * max(0, 1 + self.y1 - self.y0)


def main(text: str):
    lines = text.split('\n')
    areas = []
    tick = time.time_ns()

    for line in lines:
        sx, sy, bx, by = ints(line)
        r = abs(sx - bx) + abs(sy - by)  # Radius of the diamond
        x0, y0 = sx, sy - r  # Bottom -> Bottom left
        x1, y1 = sx, sy + r  # Top -> Top Right

        x0, y0 = transform(x0, y0)
        x1, y1 = transform(x1, y1)

        areas.append(Rect(x0, x1, y0, y1))

    for area in areas:
        print('initial', area)

    width = 2000000

    x0, y0 = transform(0, -3 * width)
    x1, y1 = transform(0, 3 * width)

    remaining_space = [Rect(x0, x1, y0, y1)]

    for area in areas:
        new_remaining = []
        for remaining in remaining_space:
            intersect = remaining.intersect(area)
            if intersect is not None:
                for part in remaining.decompose(intersect):  # Decompose into parts
                    new_remaining.append(part)
            else:
                new_remaining.append(remaining)
        remaining_space = new_remaining

    for rem in remaining_space:
        if rem.area() == 1:
            x0, y0 = untransform(rem.x0, rem.y0)
            print(x0 * 4000000 + y0)
            print('at', untransform(rem.x0, rem.y0))
    tock = time.time_ns()
    print('%d ms' % ((tock - tick) / 1_000_000))


def transform(x: int, y: int):
    return x + y, y - x

def untransform(x: int, y: int):
    return (x - y) // 2, (x + y) // 2


if __name__ == '__main__':
    main(get_input(15))
