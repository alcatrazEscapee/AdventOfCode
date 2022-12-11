# Day 11: Monkey in the Middle
# Rank: 32 / 62

from utils import get_input, ints
from collections import defaultdict, Counter
from typing import List, Tuple, NamedTuple, Callable

import math
import operator
import functools


class Monkey(NamedTuple):
    id: int
    start_items: Tuple[int]
    op: Callable[[int], int]
    div_by: int
    if_true: int
    if_false: int


def main(text: str):
    monkeys, p = parse_monkeys(text)
    print('Part 1:', simulate(monkeys, p, True))
    print('Part 2:', simulate(monkeys, p, False))

def simulate(monkeys: List[Monkey], p: int, part1: bool) -> int:
    # Part 1 simulates 20 rounds, and divides the stress by 3 each time
    # Part 2 simulates 10k rounds, and without dividing by three this quickly (i.e. within ~100 rounds), reaches numbers with >1k digits... and asks you to square them.
    # This is clearly infeasible, even for python. So, we have to be clever
    # Since we only ever check the divisibility of a given stress level, and we do basic add + multiply operations (which are functional operations mod p)
    # We can translate each stress level to a tuple of stresses mod p, where p = the product of all monkey's divisors

    inspects = Counter()
    monkey_items = [list(monkey.start_items) for monkey in monkeys]
    total_rounds = 20 if part1 else 10_000
    for _ in range(total_rounds):
        for monkey, items in zip(monkeys, monkey_items):
            for item in items:
                inspects[monkey.id] += 1
                next_item = monkey.op(item)
                if part1:
                    next_item //= 3
                else:
                    next_item %= p
                next_monkey = monkey.if_true if (next_item % monkey.div_by) == 0 else monkey.if_false
                monkey_items[next_monkey].append(next_item)
            items.clear()

    (_, a), (_, b), *_ = inspects.most_common(2)
    return a * b


def parse_monkeys(text: str) -> Tuple[List[Monkey], int]:
    monkeys = []
    monkey_items = []
    for i, part in enumerate(text.split('\n\n')):
        _, start, op, test, if_true, if_false = [x.strip() for x in part.split('\n')]
        start = ints(start)
        div_by = ints(test)[0]
        if_true = ints(if_true)[0]
        if_false = ints(if_false)[0]

        # Very primitive expression parsing
        lhs, op_char, rhs = op.split(' = ')[1].split(' ')
        is_square = rhs == 'old'
        value = 0 if is_square else int(rhs)
        op_par = functools.partial(operator.mul if op_char == '*' else operator.add, value)

        monkeys.append(Monkey(i, start, square if is_square else op_par, div_by, if_true, if_false))
        monkey_items.append(start)

    # For part 2, instead of representing the monkey's items as a single integer,
    # We represent each monkey's value as the tuple of integers, modulo each other monkey's value
    p = math.prod(m.div_by for m in monkeys)
    return monkeys, p

def square(old: int) -> int:
    return old * old


def main_original(text: str):
    """
    This was the leaderboard-state main
    As you can see, I didn't separate part 1 and 2, and hardcoded part of the input
    """

    OPS = [
        lambda o: o * 13,
        lambda o: o * o,
        lambda o: o + 6,
        lambda o: o + 2,
        lambda o: o + 3,
        lambda o: o + 4,
        lambda o: o + 8,
        lambda o: o * 7
    ]

    TOPS = [
        lambda o: o * 19,
        lambda o: o + 6,
        lambda o: o * o,
        lambda o: o + 3
    ]

    # OPS = TOPS

    monkeys = []
    for i, part in enumerate(text.split('\n\n')):
        _, start, op, test, if_true, if_false = [x.strip() for x in part.split('\n')]
        start = ints(start)
        op = OPS[i]
        div_by = ints(test)[0]
        if_false = ints(if_false)[0]
        if_true = ints(if_true)[0]

        monkeys.append([list(start), op, div_by, if_true, if_false])
    print(monkeys)

    divs = sorted([m[2] for m in monkeys])

    def to_divs(n: int):
        return tuple(n % d for d in divs)

    for i in range(len(monkeys)):
        monkeys[i][0] = [to_divs(n) for n in monkeys[i][0]]
        print(monkeys[i][0])

    inspects = defaultdict(int)
    for _ in range(10000):
        mi = 0
        for items, op, div_by, if_true, if_false in monkeys:
            for item in items:
                inspects[mi] += 1
                # new_item = op(item)# // 3
                new_item = tuple(op(p) % dv for p, dv in zip(item, divs))
                if new_item[divs.index(div_by)] == 0:
                    monkeys[if_true][0].append(new_item)
                else:
                    monkeys[if_false][0].append(new_item)
            monkeys[mi][0] = []
            mi += 1
    print(sorted(inspects.values()))


if __name__ == '__main__':
    main(get_input(11))
