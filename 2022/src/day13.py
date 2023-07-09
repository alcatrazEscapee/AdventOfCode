# Day 13: Distress Signal
# Rank: 18 / 27

from utils_all import get_input
from typing import List
from functools import cmp_to_key

Packet = int | List['Packet']


def main(text: str):
    part1 = 0
    packets: List[Packet] = []
    for index, pair in enumerate(text.split('\n\n')):
        left, right = pair.split('\n')
        left, right = eval(left), eval(right)  # eval()... the root of all evil, makes this day easy as pie

        if order(left, right) == 1:
            part1 += index + 1

        packets.append(left)
        packets.append(right)

    packets.append([[2]])
    packets.append([[6]])

    print('Part 1:', part1)

    # functools.cmp_to_key() turns a part 1 solution into a part 2 solution... just like that!
    result = sorted(packets, key=cmp_to_key(order), reverse=True)

    i1 = i2 = 0
    for index, packet in enumerate(result):
        if packet == [[2]]:
            i1 = index + 1
        elif packet == [[6]]:
            i2 = index + 1

    print('Part 2:', i1 * i2)


def order(lhs: Packet, rhs: Packet) -> int:
    if isinstance(lhs, int) and isinstance(rhs, int):
        if lhs < rhs:
            return 1
        elif lhs > rhs:
            return -1
        else:
            return 0
    elif isinstance(lhs, list) and isinstance(rhs, list):
        for lv, rv in zip(lhs, rhs):
            o = order(lv, rv)
            if o != 0:
                return o
        if len(lhs) < len(rhs):
            return 1
        if len(lhs) > len(rhs):
            return -1
        return 0
    elif isinstance(lhs, int):
        return order([lhs], rhs)
    else:
        return order(lhs, [rhs])


if __name__ == '__main__':
    main(get_input(13))
