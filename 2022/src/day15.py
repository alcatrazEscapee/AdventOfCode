from utils import get_input, ints
from typing import NamedTuple, Optional, Iterator


def main(text: str):
    ranges = []
    areas = []
    width = 2000000
    for line in text.split('\n'):
        sx, sy, bx, by = ints(line)
        r = abs(sx - bx) + abs(sy - by)  # Radius of the diamond

        dy = abs(sy - width)
        dx = r - dy
        if dx > 0:
            ranges.append((sx - dx, sx + dx))

        x0, y0 = sx, sy - r  # Bottom -> Bottom left
        x1, y1 = sx, sy + r  # Top -> Top Right

        # Coordinates of the new transformed square area
        x0, y0 = transform(x0, y0)
        x1, y1 = transform(x1, y1)

        areas.append(Rect(x0, x1, y0, y1))

    # Part 1, we implement by finding the illegal ranges, and then joining ranges together to obtain a set of disjoint ranges

    ranges = sorted(ranges, reverse=True)
    disjoint_ranges = []
    while len(ranges) >= 2:
        # Treating the ranges like a stack, merge them two at a time.
        # If we have a result which cannot be merged, then discard the newly found disjoint range
        a = a0, a1 = ranges.pop(-1)
        b = b0, b1 = ranges.pop(-1)

        if a1 < b0:  # Disjoint ranges
            disjoint_ranges.append(a)
            ranges.append(b)
        elif b1 <= a1:  # b is a subset of a, so discard b
            ranges.append(a)
        else:  # b1 > a1, so merge the two ranges
            ranges.append((a0, b1))

    print('Part 1:', sum(map(lambda v: v[1] - v[0], ranges)))

    # For part 2, we implement a coordinate subdivision method
    # We use `transform()` to skew the coordinates, such that each sensor's manhattan distance area is actually a square
    # From there we can reason about square areas, subdividing, and intersecting them and only retaining the disjoint square regions in which no sensor is covering
    # Since there are not many sensors, this eliminates areas very quickly (total runtime ~2ms)

    # For the 'initially valid' state, we just pick a square (in original coordinates) diamond, which is 'large enough'
    # It needs to contain original square, which means we pick a much larger diamond.
    # Excess area is not a problem, since we identify it's contigous and won't accidentally find it as the valid solution
    x0, y0 = transform(0, -3 * width)
    x1, y1 = transform(0, 3 * width)

    remaining = [Rect(x0, x1, y0, y1)]  # All positions that are still valid
    for area in areas:
        new_remaining = []
        for rem in remaining:
            intersect = rem.intersect(area)  # Found an intersection, so subdivide and retain only disjoint parts not intersecting `area`
            if intersect is not None:
                for part in rem.decompose(intersect):
                    new_remaining.append(part)
            else:
                new_remaining.append(rem)
        remaining = new_remaining

    # At this point according to the problem, we'll only have a single point remaining that is of area = 1
    # Simply find it and show the result.
    x0, y0 = next(untransform(rem.x0, rem.y0) for rem in remaining if rem.area() == 1)
    print('Part 2:', x0 * 4000000 + y0)

def transform(x: int, y: int):
    return x + y, y - x

def untransform(x: int, y: int):
    return (x - y) // 2, (x + y) // 2


class Rect(NamedTuple):
    """ A 2-dimensional box defined by [x0, x1] x [y0, y1]. Borrowed form 2021 Day 22 """
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


if __name__ == '__main__':
    main(get_input(15))
