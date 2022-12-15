from utils_all import *

# READ THE PROBLEM; UNDERSTAND THE PROBLEM; IMPLEMENT THE SOLUTION
# Remember: ints() for parsing, Counter, Point2/3, Grids (Finite/Infinite), A*, DP (lru_cache)

def main(text: str):
    lines = text.split('\n')

    # p1 5733732
    # p1 4985193

    data = []
    for line in lines:
        sx, sy, bx, by = ints(line)
        dist = norm1((sx - bx, sy - by))
        data.append([sx, sy, bx, by, dist])


    MAX = 2000000#20

    possible = set()
    all_possibles = []
    for sx, sy, bx, by, dist in data:
        this = set()
        #print(sx, sy, bx, by, dist)
        for dx in range(-dist - 1, 2 + dist):
            P = (sx + dx, sy + abs(dx) - abs(dist) - 1)
            Q = (sx + dx, sy - (abs(dx) - abs(dist) - 1))
            #print('doing', dx, P, Q)
            possible.add(P)
            possible.add(Q)
        print('results', len(possible))
        all_possibles.append(this)
    #possible = functools.reduce(lambda x, y: x | y, all_possibles)
    possible = [(x, y) for x, y in possible if x <= MAX and y <= MAX and x >= 0 and y >= 0]
    #print((14, 11) in possible, possible)
    print('in total', len(possible))
    for px, py in possible:
        can = True
        for sx, sy, bx, by, dist in data:
            if norm1((sx - px, sy - py)) <= dist:
                can = False
                break
        if can:
            print('FOUND THE ANSWER', px, py)


    return
    for x in range(0, 20):
        for y in range(0, 20):
            can = True
            dists = set()
            for sx, sy, bx, by, dist in data:
                if abs(x - sx) + abs(y - sy) <= abs(sx - bx) + abs(sy - by):
                    t = abs(x - sx) + abs(y - sy)
                    can = False
                    if t in dists:
                        can = False
                    dists.add(t)
            if can:

                print('can slow', x, y)

    return
    ty = 2000000
    ranges = []
    for sx, sy, bx, by, dist in data:
        dy = abs(sy - ty)
        dx = dist - dy
        if dx > 0:
            ranges.append((sx - dx, sx + dx))

    ranges = sorted(ranges, key=lambda x: x[0])
    print('ranges', ranges)

    ranges2 = list(ranges)
    joined = []
    while len(ranges2) >= 2:
        a, b, *rest = ranges2
        c = merge_range(a, b, joined)
        print('merged', c)
        rest.insert(0, c)
        ranges2 = rest
        print(ranges2)

    print('final', joined, ranges2)

    ret = sum(map(lambda x: x[1] - x[0], ranges2))

def merge_range(a, b, joined):
    a0, a1 = a
    b0, b1 = b
    #print(a, b, '->', joined)
    assert a0 <= b0
    if b1 <= a1:
        return a0, a1  # contained in
    if b0 <= a1:
    #    print('merge')
        return a0, b1  # merge
    if b0 > a1:
        # disjoint
        joined.append((a0, a1))
        return b0, b1
    #print('wut', a, b)







if __name__ == '__main__':
    main(get_input(15))
