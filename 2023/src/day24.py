from typing import NamedTuple
from aoc import Fraction, solve


MIN = 200000000000000
MAX = 400000000000000

class Hail(NamedTuple):
    x: int
    y: int
    z: int
    vx: int
    vy: int
    vz: int


def main():
    hail = []

    with open('./inputs/day24.txt', 'r', encoding='utf-8') as f:
        for line in f.read().split('\n'):
            x, y, z, vx, vy, vz = map(int, line.replace('@', '').replace(',', '').split())
            hail.append(Hail(x, y, z, vx, vy, vz))
    
    # ===== Part 1 =====
    # Iterate through all pairs, and compute the intersection using exact-value integer (fractional)
    # linear equations.
    part1 = 0
    for i, p in enumerate(hail):
        for j in range(i + 1, len(hail)):
            if intersect_2d(p, hail[j]):
                part1 += 1
    
    print('Part 1:', part1)

    # ===== Part 2 =====
    # Assume the rock is given by pr + t*vr, and hail given by pj + tj*vj
    # Then,
    #
    #     pr + tj*vr = pj + tj*vj for j=1,..
    #  => (pr - pj) = -tj * (vr - vj)
    #  => (pr - pj) x (vr - vj) = 0
    #
    # Then, by taking j=0,1 and j=0,2, we can equate these together:
    #
    #  => (pr - p0) x (vr - v0) = (pr - p1) x (vr - v1)
    #  => (pr - p0) x (vr - v0) = (pr - p2) x (vr - v2)
    #
    # Note that the terms p x v all cancel out in the expansion of the cross product,
    # so we are left with a series of six (2 x 3) linear equations in pr, vr:
    #
    # We can reformulate this as a linear system and solve for the resulting pr, vr
    #
    # This solution uses `aoc.solve()`, which internally uses `aoc.Fraction` to represent
    # arbitrary precision rational values, which produces an exact integral answer.
    # 
    # Also, it doesn't introduce any external dependencies on math libraries which insist on
    # using floating point calculations

    h0, h1, h2, *_ = hail

    a = [
        [0,             h1.vz - h0.vz, h0.vy - h1.vy, 0,           h0.z - h1.z, h1.y - h0.y],
        [h0.vz - h1.vz, 0,             h1.vx - h0.vx, h1.z - h0.z, 0,           h0.x - h1.x],
        [h1.vy - h0.vy, h0.vx - h1.vx, 0,             h0.y - h1.y, h1.x - h0.x, 0          ],

        [0,             h2.vz - h0.vz, h0.vy - h2.vy, 0,           h0.z - h2.z, h2.y - h0.y],
        [h0.vz - h2.vz, 0,             h2.vx - h0.vx, h2.z - h0.z, 0,           h0.x - h2.x],
        [h2.vy - h0.vy, h0.vx - h2.vx, 0,             h0.y - h2.y, h2.x - h0.x, 0          ],
    ]
    b = [
        h1.y * h1.vz -h0.y * h0.vz + h0.z * h0.vy - h1.z * h1.vy,
        h0.x * h0.vz -h1.x * h1.vz - h0.z * h0.vx + h1.z * h1.vx,
        h1.x * h1.vy -h0.x * h0.vy - h1.y * h1.vx + h0.y * h0.vx,

        h2.y * h2.vz -h0.y * h0.vz + h0.z * h0.vy - h2.z * h2.vy,
        h0.x * h0.vz -h2.x * h2.vz - h0.z * h0.vx + h2.z * h2.vx,
        h2.x * h2.vy -h0.x * h0.vy - h2.y * h2.vx + h0.y * h0.vx,
    ]

    rx, ry, rz, *_ = solve(a, b)

    print('Part 2:', int(rx + ry + rz))


def intersect_2d(p: Hail, q: Hail) -> bool:
    t2 = Fraction(p.vy * (p.x - q.x) - p.vx * (p.y - q.y), p.vy * q.vx - p.vx * q.vy)
    t1 = (q.vy * t2 + q.y - p.y) / (p.vy * t2)

    if t1.d == 0 or t1.n < 0 or t2.d == 0 or t2.n < 0:
        return False

    x = q.x + q.vx * t2
    y = q.y + q.vy * t2

    return MIN <= x <= MAX and MIN <= y <= MAX


if __name__ == '__main__':
    main()
