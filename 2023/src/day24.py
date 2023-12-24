from typing import NamedTuple

import numpy as np

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
    # This problem is straightforward enough that we can derive a series of integer-equations,
    # using exact values (large integers and fractions) which solves the intersection for each pair
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
    # Note that this relies on computing exact values in floating point, which *seems* to
    # work and rounding gets us the correct exact integral answer...

    h0, h1, h2, *_ = hail

    a = np.array([
        [0,             h1.vz - h0.vz, h0.vy - h1.vy, 0,           h0.z - h1.z, h1.y - h0.y],
        [h0.vz - h1.vz, 0,             h1.vx - h0.vx, h1.z - h0.z, 0,           h0.x - h1.x],
        [h1.vy - h0.vy, h0.vx - h1.vx, 0,             h0.y - h1.y, h1.x - h0.x, 0          ],

        [0,             h2.vz - h0.vz, h0.vy - h2.vy, 0,           h0.z - h2.z, h2.y - h0.y],
        [h0.vz - h2.vz, 0,             h2.vx - h0.vx, h2.z - h0.z, 0,           h0.x - h2.x],
        [h2.vy - h0.vy, h0.vx - h2.vx, 0,             h0.y - h2.y, h2.x - h0.x, 0          ],
    ])
    b = -np.array([
        h0.y * h0.vz - h1.y * h1.vz - h0.z * h0.vy + h1.z * h1.vy,
        h1.x * h1.vz - h0.x * h0.vz + h0.z * h0.vx - h1.z * h1.vx,
        h0.x * h0.vy - h1.x * h1.vy + h1.y * h1.vx - h0.y * h0.vx,

        h0.y * h0.vz - h2.y * h2.vz - h0.z * h0.vy + h2.z * h2.vy,
        h2.x * h2.vz - h0.x * h0.vz + h0.z * h0.vx - h2.z * h2.vx,
        h0.x * h0.vy - h2.x * h2.vy + h2.y * h2.vx - h0.y * h0.vx,
    ])

    x = np.linalg.solve(a, b)

    print('Part 2:', round(x[0] + x[1] + x[2]))


def intersect_2d(p: Hail, q: Hail) -> bool:
    # Where xn, xd are variables, they are interpreted as fractions xn/xd, with xn, xd integers
    # We abuse python's arbitrary precision integers here, to compute exact value answers

    t2n, t2d = norm(
        p.vy * (p.x - q.x) - p.vx * (p.y - q.y),
        p.vy * q.vx - p.vx * q.vy
    )
    
    if t2d == 0 or t2n < 0:
        return False  # No intersection

    t1n, t1d = norm(
        q.vy * t2n + t2d * (q.y - p.y),
        t2d * p.vy
    )
    
    if t1d == 0 or t1n < 0:
        return False  # No intersection
    
    x = q.x * t2d + q.vx * t2n
    y = q.y * t2d + q.vy * t2n

    q_min = MIN * t2d
    q_max = MAX * t2d

    return q_min <= x <= q_max and q_min <= y <= q_max


def norm(n: int, d: int) -> tuple[int, int]:
    # Normalize a fraction n/d s.t. if n/d == 0, then n=1, d=0; and that d > 0 always
    if n == 0:
        return 0, 1
    if d < 0:
        return -n, -d
    return n, d


if __name__ == '__main__':
    main()
